library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input-residual')
parser$add_argument('--region-grid-df')
parser$add_argument('--output')
args <- parser$parse_args()

region_grid_df <- fst::read_fst(args$region_grid_df)

log_trace('Removing region month mean from {args$input_residual}')
output <- read_residual(args$input_residual) %>%
  left_join(
    region_grid_df,
    by = c('longitude', 'latitude')
  ) %>%
  filter(
    region %in% c(sprintf('Region%02d', 1:11), 'RegionNZ')
  ) %>%
  mutate(
    month_hour = interaction(
      month(time),
      hour(time),
      sep = '_',
      drop = TRUE
    )
  ) %>%
  tidyr::nest(
    .by = c('month_hour', 'longitude', 'latitude')
  ) %>%
  mutate(
    month_hour_mean = purrr::map_dbl(data, function(x) mean(x$value))
  ) %>%
  tidyr::unnest(
    cols = c(data)
  ) %>%
  mutate(
    value = value - month_hour_mean
  ) %>%
  select(
    -c(month_hour, month_hour_mean)
  ) %>%
  arrange(
    longitude,
    latitude,
    time
  ) %>%
  select(longitude, latitude, time, everything())

log_trace('Writing to {args$output}')
fst::write_fst(output, args$output)

log_trace('Done')
