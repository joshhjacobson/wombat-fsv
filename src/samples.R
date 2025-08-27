library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(Rcpp)

rcpp_cache_dir <- Sys.getenv('RCPP_CACHE_DIR')
options(rcpp.cache.dir = if (rcpp_cache_dir == '') tempdir() else rcpp_cache_dir)

source(Sys.getenv('UTILS_PARTIAL'))
sourceCpp(Sys.getenv('UTILS_CPP_PARTIAL'))
sourceCpp(Sys.getenv('HMC_EXACT_CPP_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--basis-vectors')
parser$add_argument('--constraints')
parser$add_argument('--constraints-nee')
parser$add_argument('--alpha-samples')
parser$add_argument('--delta-precision-diagonals')
parser$add_argument('--scale-factor', choices = c('01', '05', '10', '15'))
parser$add_argument('--output')
args <- parser$parse_args()


get_delta_precision <- function(variance_diagonal, scale_factor) {
  variance_diagonal <- scale_factor * variance_diagonal
  stopifnot(
    all(!is.na(variance_diagonal))
  )
  diag(1 / variance_diagonal)
}


basis_vectors <- fst::read_fst(args$basis_vectors)
constraints <- readRDS(args$constraints)
constraints_nee <- readRDS(args$constraints_nee)
alpha_samples <- readRDS(args$alpha_samples)
delta_precision_diagonals <- fst::read_fst(args$delta_precision_diagonals)

scale_factor <- list(
  '01' = 0.1,
  '05' = 0.5,
  '10' = 1.0,
  '15' = 1.5
)[[args$scale_factor]]


# NOTE: each sample of delta is drawn conditionally for each sample of alpha, so rather than
# draw warm-up samples for an entire chain (as is done when sampling alpha), we draw warm-up
# samples for each sample of delta to allow sufficient mixing
N_WARM_UP <- 15
N_SAMPLES <- alpha_samples$n_samples - alpha_samples$n_warm_up


with(basis_vectors, {
  is_ocean_inventory <<- inventory == 'ocean'
  is_land_region <<- region %in% sprintf('Region%02d', 1 : 11)
  is_ocean_region <<- region %in% c(sprintf('Region%02d', 12 : 22))
  is_nz_region <<- region == 'RegionNZ'
  is_climatology <<- component != 'residual'
  is_fixed_RLT <<- (
    inventory == 'bio_resp_tot' &
      component %in% c('intercept', 'trend') &
      region == 'Region03'
  )
})

delta_to_include <- (
  ((!is_ocean_inventory & is_land_region) |
    (!is_ocean_inventory & !is_climatology & is_nz_region) |
    (is_ocean_inventory & is_ocean_region & !is_climatology)) & !is_fixed_RLT
)

stopifnot(
  !(
    delta_precision_diagonals[delta_to_include, ] %>%
      select(all_of(c(
        'aggregate_coefficient',
        'model_variance_mle'
      ))) %>%
      is.na() %>%
      any()
  )
)

alpha_to_include <- alpha_samples$alpha_df %>%
  pull(basis_vector) %>%
  as.integer()

stopifnot(length(setdiff(
  basis_vectors$basis_vector[delta_to_include],
  basis_vectors$basis_vector[alpha_to_include]
)) == 0)

aggregate_coefficients <- delta_precision_diagonals[delta_to_include, ]$aggregate_coefficient
scale_matrix <- Diagonal(x = 1 / aggregate_coefficients)

g_constraint <- c(
  5 / KG_M2_S_TO_PGC_MONTH,
  5 / KG_M2_S_TO_PGC_MONTH,
  constraints$g_sign,
  constraints$g_residual
)
F_constraint <- rbind(
  constraints_nee$F_nee,
  -constraints_nee$F_nee,
  constraints$F_sign,
  constraints$F_residual
)
F_constraint_alpha <- rbind(
  constraints$F_sign,
  constraints$F_residual
)
stopifnot(nrow(F_constraint) == length(g_constraint))

F_scaled <- F_constraint[, delta_to_include] %*% scale_matrix
g_perturbed_samples <- g_constraint + rbind(
  matrix(0, nrow = 2, ncol = N_SAMPLES),
  tcrossprod(
    F_constraint_alpha[, alpha_to_include], tail(alpha_samples$alpha, N_SAMPLES)
  )
)

chol_delta_precision <- delta_precision_diagonals[delta_to_include, ] %>%
  pull('model_variance_mle') %>%
  get_delta_precision(scale_factor) %>%
  chol()


log_debug('Starting conditional sampling for delta')
delta_samples <- pbmcapply::pbmclapply(seq_len(N_SAMPLES), function(i) {
  sampleHmcConstrained(
    rep(0, sum(delta_to_include)),
    rep(0, sum(delta_to_include)),
    R = chol_delta_precision,
    F = F_scaled,
    g = g_perturbed_samples[, i],
    totalTime = pi / 2,
    nSamples = N_WARM_UP + 1,
    debug = FALSE
  )[N_WARM_UP + 1, ]
}, mc.cores = get_cores(), ignore.interactive = TRUE) %>%
  do.call(rbind, .)

delta_samples_scaled <- as.matrix(delta_samples %*% scale_matrix)

delta_df <- cbind(
  basis_vectors[delta_to_include, ],
  data.frame(
    value = colMeans(delta_samples_scaled)
  )
)
delta_df$value_samples <- t(delta_samples_scaled)

output <- list(
  delta = delta_samples,
  delta_scaled = delta_samples_scaled,
  delta_df = delta_df,
  scale_factor = scale_factor,
  n_samples = N_SAMPLES,
  n_warm_up = 0
)

log_debug('Saving to {args$output}')
saveRDS(output, args$output)

log_debug('Done')
