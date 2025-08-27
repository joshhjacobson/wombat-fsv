library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(fst)
library(Rcpp)

source(Sys.getenv('UTILS_PARTIAL'))
sourceCpp(Sys.getenv('UTILS_CPP_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--basis-vectors')
parser$add_argument('--var-climatology-assim')
parser$add_argument('--var-climatology-resp-tot')
parser$add_argument('--var-residual-assim', nargs = '+')
parser$add_argument('--var-residual-resp-tot', nargs = '+')
parser$add_argument('--var-residual-ocean')
parser$add_argument('--output')
args <- parser$parse_args()


basis_vectors <- read_fst(args$basis_vectors)
start_date <- as.Date(paste0(levels(basis_vectors$month)[1], '-01'))

var_climatology_bio <- bind_rows(
  read_fst(args$var_climatology_assim) %>%
    mutate(
      inventory = 'bio_assim',
      aggregate_coefficient = -aggregate_coefficient
    ) %>%
    rename(component = 'variable'),
  read_fst(args$var_climatology_resp_tot) %>%
    mutate(inventory = 'bio_resp_tot') %>%
    rename(component = 'variable')
)

var_residual_bio <- bind_rows(
  lapply(args$var_residual_assim, read_fst) %>%
    bind_rows() %>%
    mutate(
      inventory = 'bio_assim',
      component = 'residual',
      aggregate_coefficient = -aggregate_coefficient
    ),
  lapply(args$var_residual_resp_tot, read_fst) %>%
    bind_rows() %>%
    mutate(
      inventory = 'bio_resp_tot',
      component = 'residual'
    )
) %>%
  filter(year_month >= start_date) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(between(
          year_month,
          as.Date('2020-01-01'),
          as.Date('2020-03-01')
        )) %>%
        mutate(
          year_month = year_month + years(1)
        )
    )
  }

var_residual_ocean <- read_fst(args$var_residual_ocean) %>%
  filter(year_month >= start_date) %>%
  mutate(
    inventory = 'ocean',
    component = 'residual'
  ) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(year(year_month) == 2019) %>%
        mutate(
          year_month = year_month + years(1)
        ),
      x %>%
        filter(between(
          year_month,
          as.Date('2019-01-01'),
          as.Date('2019-03-01')
        )) %>%
        mutate(
          year_month = year_month + years(2)
        )
    )
  }

var_basis <- bind_rows(
  var_climatology_bio,
  var_residual_bio,
  var_residual_ocean
) %>%
  mutate(
    # format month as YYYY-MM
    month = substr(year_month, 1, 7)
  ) %>%
  select(-year_month) %>%
  mutate(
    inventory = factor(inventory),
    component = factor(component),
    region = factor(region),
    month = factor(month)
  ) %>%
  add_basis_vector(basis_vectors)

output <- basis_vectors %>%
  left_join(
    var_basis,
    by = c('inventory', 'component', 'region', 'month', 'basis_vector')
  )

write_fst(output, args$output)
