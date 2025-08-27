library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(Matrix)
library(matrixStats)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented')
parser$add_argument('--samples-alpha')
parser$add_argument('--samples-delta')
parser$add_argument('--scale-factor', choices = c('01', '05', '10', '15'))
parser$add_argument('--output')
args <- parser$parse_args()


scale_factor <- list(
  '01' = 0.1,
  '05' = 0.5,
  '10' = 1.0,
  '15' = 1.5
)[[args$scale_factor]]
posterior_name <- sprintf('FSV posterior: %s', scale_factor)

perturbations_base <- fst::read_fst(args$perturbations_augmented)
samples_alpha <- readRDS(args$samples_alpha)
samples_delta <- readRDS(args$samples_delta)


samples_combined <- add_samples_alpha_delta(samples_alpha, samples_delta)

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

posterior_emissions_annual_average_v2s <- compute_posterior(
  prior_emissions_annual_average,
  X_global_annual_average,
  samples_alpha$alpha_df,
  'v2.S posterior'
)

posterior_emissions_annual_average_fs <- compute_posterior(
  prior_emissions_annual_average,
  X_global_annual_average,
  samples_combined$samples_df,
  posterior_name
)

emissions_annual_average <- bind_rows(
  prior_emissions_annual_average,
  posterior_emissions_annual_average_v2s,
  posterior_emissions_annual_average_fs
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
          value_q025 = rowQuantiles(value_samples, probs = 0.025),
          value_q975 = rowQuantiles(value_samples, probs = 0.975)
        )
    )
  } %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration',
      'nee' = 'NEE',
      'ocean' = 'Ocean'
    )[inventory], levels = c(
      'GPP',
      'Respiration',
      'NEE',
      'Ocean'
    )),
    estimate = factor(
      estimate,
      levels = c(
        'Bottom-up',
        'v2.S posterior',
        posterior_name
      )
    )
  ) %>%
  select(c(inventory, estimate, starts_with('value'))) %>%
  select(-value_samples) %>%
  arrange(inventory, estimate)

write.csv(emissions_annual_average, args$output)
