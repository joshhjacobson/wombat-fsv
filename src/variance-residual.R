library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input-residual')
parser$add_argument('--mle-residual')
parser$add_argument('--region-grid-df')
parser$add_argument('--output')
args <- parser$parse_args()

model_variances <- function(data, range_spatial, range_temporal) {
  n_hours <- n_distinct(data$time)

  # NOTE(jhj): temporal sum is taken over hourly windows;
  # division by n_hours preserves the per second rate
  data <- data %>%
    mutate(weight = area * value / n_hours) %>%
    arrange(time, longitude, latitude)

  # NOTE(jhj): distinct() preserves ordering
  distance_spatial <- data %>%
    select(longitude, latitude) %>%
    distinct() %>%
    as.matrix() %>%
    fields::RdistEarth(miles = FALSE)

  distance_time <- toeplitz(seq(0, n_hours - 1))

  weights <- data %>% pull(weight)
  weights_mat <- matrix(weights, ncol = n_hours)

  var_mle <- weights %*% as.vector(crossprod(
    exp(-distance_spatial / range_spatial),
    weights_mat %*% exp(-distance_time / range_temporal)
  ))

  # Resulting variance units are (kg/s)^2
  list(
    aggregate_coefficient = sum(weights),
    model_variance_indep = sum(weights^2),
    model_variance_mle = var_mle[1],
    model_variance_exact_corr = sum(weights)^2
  )
}


correlation_length <- fst::read_fst(args$mle_residual)
region_grid_df <- fst::read_fst(args$region_grid_df)

log_trace('Computing variances for {args$input_residual}')
output <- read_residual(args$input_residual) %>%
  mutate(
    year_month = floor_date(time, 'month')
  ) %>%
  left_join(
    region_grid_df,
    by = c('longitude', 'latitude')
  ) %>%
  filter(
    region %in% c(sprintf('Region%02d', 1:11), 'RegionNZ')
  ) %>%
  nest(
    .by = c('region', 'year_month')
  ) %>%
  left_join(
    correlation_length,
    by = c('region', 'year_month')
  ) %>%
  mutate(
    variances = purrr::pmap(
      list(data, range_spatial, range_temporal),
      model_variances
    )
  ) %>%
  unnest_wider(variances) %>%
  select(-c(data, range_spatial, range_temporal))

log_trace('Writing to {args$output}')
fst::write_fst(output, args$output)

log_trace('Done')
