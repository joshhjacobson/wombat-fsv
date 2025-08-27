library(argparse)
library(dplyr, warn.conflicts = FALSE)

parser <- ArgumentParser()
parser$add_argument('--region-grid')
parser$add_argument('--control-emissions')
parser$add_argument('--output')
args <- parser$parse_args()

region_grid <- readRDS(args$region_grid)
control_emissions <- fst::read_fst(args$control_emissions)

region_grid_df <- NULL
for (latitude_index in seq_along(attr(region_grid$Region01, 'latitude'))) {
  for (longitude_index in seq_along(attr(region_grid$Region01, 'longitude'))) {
    found_any <- FALSE
    for (region_name in names(region_grid)) {
      if (region_grid[[region_name]][longitude_index, latitude_index] > 0) {
        region_grid_df <- rbind(region_grid_df, data.frame(
          longitude = attr(region_grid$Region01, 'longitude')[longitude_index],
          latitude = attr(region_grid$Region01, 'latitude')[latitude_index],
          region = region_name
        ))
        found_any <- TRUE
      }
    }
    if (!found_any) {
      region_grid_df <- rbind(region_grid_df, data.frame(
        longitude = attr(region_grid$Region01, 'longitude')[longitude_index],
        latitude = attr(region_grid$Region01, 'latitude')[latitude_index],
        region = 'Region00'
      ))
    }
  }
}

output <- region_grid_df %>%
  left_join(
    control_emissions %>%
      distinct(longitude, latitude, area),
    by = c('longitude', 'latitude')
  )

fst::write_fst(output, args$output)
