library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input-climatology')
parser$add_argument('--mle-climatology')
parser$add_argument('--region-grid-df')
parser$add_argument('--output')
args <- parser$parse_args()

model_variances <- function(data, range_spatial) {
  weights <- data$area * data$value

  distance_spatial <- data %>%
    select(longitude, latitude) %>%
    as.matrix() %>%
    fields::RdistEarth(miles = FALSE)

  var_mle <- crossprod(
    weights,
    exp(-distance_spatial / range_spatial) %*% weights
  )

  # Resulting variance units are (kg/s)^2
  list(
    aggregate_coefficient = sum(weights),
    model_variance_indep = sum(weights^2),
    model_variance_mle = var_mle,
    model_variance_exact_corr = sum(weights)^2
  )
}


correlation_length <- fst::read_fst(args$mle_climatology)
region_grid_df <- fst::read_fst(args$region_grid_df)

log_trace('Computing variances for {args$input_climatology}')
output <- read_climatology(args$input_climatology) %>%
  left_join(
    region_grid_df,
    by = c('longitude', 'latitude')
  ) %>%
  filter(
    region %in% sprintf('Region%02d', 1:11)
  ) %>%
  nest(
    .by = c('region', 'variable')
  ) %>%
  left_join(
    correlation_length,
    by = c('region', 'variable')
  ) %>%
  mutate(
    variances = purrr::pmap(
      list(data, range_spatial),
      model_variances
    )
  ) %>%
  unnest_wider(variances) %>%
  select(-c(data, range_spatial))

log_trace('Writing to {args$output}')
fst::write_fst(output, args$output)

log_trace('Done')
