library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(Rcpp)

source(Sys.getenv('UTILS_PARTIAL'))
sourceCpp(Sys.getenv('UTILS_CPP_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--basis-vectors')
parser$add_argument('--control-emissions')
parser$add_argument('--perturbations')
parser$add_argument('--output')
args <- parser$parse_args()


basis_vectors <- fst::read_fst(args$basis_vectors)
control_emissions_bio <- fst::read_fst(args$control_emissions) %>%
  filter(inventory %in% c('bio_assim', 'bio_resp_tot'))
perturbations_bio <- fst::read_fst(args$perturbations) %>%
  filter(
    inventory %in% c('bio_assim', 'bio_resp_tot'),
    between(lubridate::year(time), 2015, 2020)
  )

# Construct constraint vector for global annual average NEE perturbation
perturbations_annual_average <- perturbations_bio %>%
  left_join(
    control_emissions_bio %>%
      distinct(longitude, latitude, area),
    by = c('longitude', 'latitude')
  ) %>%
  add_basis_vector(basis_vectors) %>%
  group_by(basis_vector) %>%
  summarise(
    # average over 6 years
    value = sum(area * value) / 6,
    .groups = 'drop'
  )

baseline_annual_average <- sum(perturbations_annual_average$value)

F_constraint_nee <- with(perturbations_annual_average, sparseMatrix(
  i = rep(1, nrow(perturbations_annual_average)),
  j = as.integer(basis_vector),
  x = value,
  dims = c(1, nrow(basis_vectors))
))

log_info('Saving to {args$output}')
output <- list(
  F_nee = F_constraint_nee,
  baseline_nee = baseline_annual_average
)
saveRDS(output, args$output)
log_info('Done')
