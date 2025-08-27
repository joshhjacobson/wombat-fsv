library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(Matrix)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('MLE_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input-residual')
parser$add_argument('--output')
args <- parser$parse_args()

log_trace('Fitting MLE range to {args$input_residual}')
output <- fst::read_fst(args$input_residual) %>%
  mutate(
    year_month = floor_date(time, 'month')
  ) %>%
  filter(year_month >= '2014-09-01') %>%
  nest(
    .by = c('year_month', 'region')
  ) %>%
  mutate(
    range_spacetime = purrr::map(data, mle_range_spacetime)
  ) %>%
  unnest_wider(range_spacetime) %>%
  select(year_month, region, range_spatial, range_temporal, convergence)

log_trace('Writing to {args$output}')
fst::write_fst(output, args$output)

log_trace('Done')
