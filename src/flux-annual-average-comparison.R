library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(Matrix)
library(matrixStats)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented')
parser$add_argument('--samples-alpha')
parser$add_argument('--samples-delta-list', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

compute_posterior_sd <- function(prior, design_matrix, samples_df, posterior_name) {
  prior %>%
    mutate(
      estimate = posterior_name,
      value_prior = value,
      value = value_prior + as.vector(
        design_matrix[, as.integer(samples_df$basis_vector)]
        %*% samples_df$value
      ),
      value_samples = value_prior + as.matrix(
        design_matrix[, as.integer(samples_df$basis_vector)]
        %*% samples_df$value_samples
      ),
      value_sd = rowSds(value_samples)
    ) %>%
    select(-value_prior)
}

printf <- function(...) cat(sprintf(...))
collapse0 <- function(x) paste0(x, collapse = '')
paste_columns <- function(x) paste0(x, collapse = ' & ')

perturbations_base <- fst::read_fst(args$perturbations_augmented)
samples_alpha <- readRDS(args$samples_alpha)
samples_delta_list <- lapply(args$samples_delta_list, readRDS)
samples_delta_names <- sapply(samples_delta_list, function(x) {
  sprintf('FSV posterior: %.1f', x$scale_factor)
})

samples_combined_list <- lapply(samples_delta_list, function(samples_delta) {
  list(
    scale_factor = samples_delta$scale_factor,
    samples = add_samples_alpha_delta(samples_alpha, samples_delta)
  )
})

perturbations_annual_average <- perturbations_base %>%
  group_by(inventory, basis_vector) %>%
  summarise(
    # average over 6 years
    value = KG_M2_S_TO_PGC_MONTH * sum(area * value) / 6,
    .groups = 'drop'
  )

X_global_annual_average <- with(perturbations_annual_average, sparseMatrix(
  i = as.integer(inventory),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory), nlevels(basis_vector))
))

prior_emissions_annual_average <- perturbations_annual_average %>%
  group_by(inventory) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  mutate(estimate = 'Bottom-up')

posterior_emissions_annual_average_v2s <- compute_posterior_sd(
  prior_emissions_annual_average,
  X_global_annual_average,
  samples_alpha$alpha_df,
  'v2.S posterior'
)

posterior_emissions_annual_average_fsv <- lapply(
  samples_combined_list,
  function(samples_combined) {
    compute_posterior_sd(
      prior_emissions_annual_average,
      X_global_annual_average,
      samples_combined$samples$samples_df,
      sprintf('FSV posterior: %.1f', samples_combined$scale_factor)
    )
  }
)

emissions_annual_average <- bind_rows(
  prior_emissions_annual_average,
  posterior_emissions_annual_average_v2s,
  posterior_emissions_annual_average_fsv
) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
        group_by(estimate) %>%
        summarise(
          value = sum(value),
          value_samples = t(colSums(value_samples)),
          .groups = 'drop'
        ) %>%
        mutate(
          inventory = 'nee',
          value_sd = rowSds(value_samples)
        )
    )
  } %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Resp.',
      'nee' = 'NEE',
      'ocean' = 'Ocean'
    )[inventory], levels = c(
      'GPP',
      'Resp.',
      'NEE',
      'Ocean'
    )),
    estimate = factor(
      estimate,
      levels = c(
        'Bottom-up',
        'v2.S posterior',
        samples_delta_names
      )
    )
  ) %>%
  select(c(inventory, estimate, starts_with('value'))) %>%
  select(-value_samples) %>%
  mutate(
    summary = ifelse(
      estimate == 'Bottom-up',
      sprintf('%.2f', value),
      sprintf('%.2f (%.2f)', value, value_sd)
    )
  ) %>%
  select(inventory, estimate, summary) %>%
  tidyr::pivot_wider(
    names_from = inventory,
    values_from = summary
  ) %>%
  arrange(estimate) %>%
  mutate(estimate = as.character(estimate))


inventories <- c('GPP', 'Resp.', 'NEE', 'Ocean')

log_debug('Writing table to {args$output}')
sink(args$output)
cat('
  \\begin{tabular}{\n
    l\n
    S[table-format=-3.2(2.2)]\n
    S[table-format=3.2(2.2)]\n
    S[table-format=-1.2(1.2)]\n
    S[table-format=-1.2(1.2)]\n
  }\n
  \\toprule\n
')
printf(
  'Model & \\multicolumn{%d}{c}{Global Annual Average Flux [PgC/year]} \\\\\n',
  length(inventories)
)
printf('\\cmidrule(l{10pt}){2-%d}\n', ncol(emissions_annual_average))
cat('& {GPP} & {Resp.} & {NEE} & {Ocean} \\\\\n\\midrule\n')
for (i in seq_len(nrow(emissions_annual_average))) {
  emissions_row <- emissions_annual_average[i, ]
  printf(
    '%s & %s \\\\\n',
    ifelse(
      grepl(':', emissions_row$estimate),
      paste('FSV: $\\gamma^{\\omega} = $', sub('.*:\\s*', '', emissions_row$estimate)),
      emissions_row$estimate
    ),
    paste_columns(emissions_row[inventories])
  )
}
cat('\\bottomrule\n\\end{tabular}\n')
sink(NULL)

log_debug('Done')
