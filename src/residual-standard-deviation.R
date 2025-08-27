library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented')
parser$add_argument('--delta-precision-diagonals')
parser$add_argument('--region')
parser$add_argument('--output')
args <- parser$parse_args()


perturbations_base <- fst::read_fst(args$perturbations_augmented)
delta_precision_diagonals <- fst::read_fst(args$delta_precision_diagonals)


prior_residual <- perturbations_base %>%
  filter(
    inventory == 'bio_assim',
    component == 'residual',
    region == args$region
  ) %>%
  group_by(time) %>%
  summarise(
    value = KG_M2_S_TO_PGC_MONTH * sum(area * value),
    .groups = 'drop'
  ) %>%
  mutate(
    time = lubridate::floor_date(time, 'month')
  )

delta_sd <- delta_precision_diagonals %>%
  filter(
    inventory == 'bio_assim',
    component == 'residual',
    region == args$region
  ) %>%
  mutate(
    time = as.Date(sprintf('%s-01', month)),
    value_mle = KG_M2_S_TO_PGC_MONTH * sqrt(model_variance_mle),
    value_indep = KG_M2_S_TO_PGC_MONTH * sqrt(model_variance_indep),
    value_exact = KG_M2_S_TO_PGC_MONTH * sqrt(model_variance_exact_corr),
    value_coefficient = KG_M2_S_TO_PGC_MONTH * aggregate_coefficient
  ) %>%
  filter(between(lubridate::year(time), 2015, 2020)) %>%
  select(time, value_exact, value_mle, value_indep) %>%
  tidyr::pivot_longer(
    cols = -time,
    names_to = 'estimate',
    values_to = 'value'
  ) %>%
  mutate(
    estimate = factor(
      estimate,
      levels = c('value_exact', 'value_mle', 'value_indep'),
      labels = c('Perfect correlation', 'MLE correlation', 'Independence')
    )
  )


p1 <- ggplot(prior_residual, aes(x = time, y = abs(value))) +
  geom_line() +
  scale_x_date(date_labels = '%Y-%m') +
  labs(
    title = 'Absolute value',
    x = 'Time',
    y = 'GPP flux [PgC/month]'
  ) +
  theme(
    axis.text.y = element_text(size = 7, colour = '#23373b'),
    axis.title.y = element_text(size = 10, colour = '#23373b')
  )

p2 <- ggplot(delta_sd, aes(x = time, y = value, linetype = estimate, colour = estimate)) +
  geom_line() +
  scale_x_date(date_labels = '%Y-%m') +
  scale_linetype_manual(values = c('41', '22', '11')) +
  scale_colour_manual(values = c('#a6a6a6', '#404040', '#737373')) +
  labs(
    title = 'Standard deviation',
    x = 'Time',
    y = NULL,
    linetype = NULL,
    colour = NULL
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

output <- p1 + p2 +
  plot_layout(guides = 'collect') &
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 0.1, r = 0.1, b = 0, l = 0.1, unit = 'cm'),
    axis.text.x = element_text(size = 8, colour = '#23373b'),
    axis.title.x = element_text(
      size = 10,
      colour = '#23373b',
      margin = margin(t = 0.2, r = 0, b = 0, l = 0, unit = 'cm')
    ),
    legend.position = 'bottom',
    legend.margin = margin(t = -0.2, r = 0, b = 0, l = 0, unit = 'cm'),
    legend.text = element_text(size = 10)
  )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$supplement_full_width,
  height = 7
)
