library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(Matrix)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('MLE_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input-climatology')
parser$add_argument('--region-grid-df')
parser$add_argument('--output')
args <- parser$parse_args()

region_grid_df <- fst::read_fst(args$region_grid_df)

log_trace('Fitting MLE range to {args$input_climatology}')
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
  mutate(
    range_spatial = purrr::map(data, mle_range_spatial)
  ) %>%
  unnest_wider(range_spatial) %>%
  select(region, variable, range_spatial, convergence)

log_trace('Writing to {args$output}')
fst::write_fst(output, args$output)

log_trace('Done')
