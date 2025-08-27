library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(Matrix)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented')
parser$add_argument('--samples-alpha')
parser$add_argument('--samples-delta')
parser$add_argument('--design-case')
parser$add_argument('--output')
args <- parser$parse_args()

perturbations_base <- fst::read_fst(args$perturbations_augmented)
samples_alpha <- readRDS(args$samples_alpha)
samples_delta <- readRDS(args$samples_delta)
posterior_name <- sprintf('Fine-scale: %s', args$design_case)

colour_key <- append(
  DISPLAY_SETTINGS$colour_key,
  setNames('#bbcc33', posterior_name)
)
linetype_key <- append(
  DISPLAY_SETTINGS$linetype_key,
  setNames('4121', posterior_name)
)


samples_combined <- add_samples_alpha_delta(samples_alpha, samples_delta)

perturbations_monthly_base <- perturbations_base %>%
  mutate(
    inventory_time = interaction(inventory, time, drop = TRUE)
  )

perturbations_monthly <- perturbations_monthly_base %>%
  group_by(inventory_time, basis_vector) %>%
  summarise(
    value = KG_M2_S_TO_PGC_MONTH * sum(area * value),
    .groups = 'drop'
  ) %>%
  left_join(
    perturbations_monthly_base %>%
      distinct(inventory_time, inventory, time),
    by = 'inventory_time'
  )

X_global_monthly <- with(perturbations_monthly, sparseMatrix(
  i = as.integer(inventory_time),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_time), nlevels(basis_vector))
))

prior_emissions_monthly <- perturbations_monthly %>%
  group_by(inventory_time, inventory, time) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_time) %>%
  mutate(estimate = 'Bottom-up')

posterior_emissions_monthly <- bind_rows(
  compute_posterior(
    prior_emissions_monthly,
    X_global_monthly,
    samples_alpha$alpha_df,
    'v2.S posterior'
  ),
  compute_posterior(
    prior_emissions_monthly,
    X_global_monthly,
    samples_combined$samples_df,
    posterior_name
  )
)

emissions_monthly <- bind_rows(
  prior_emissions_monthly,
  posterior_emissions_monthly
) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
        group_by(estimate, time) %>%
        summarise(
          value = sum(value),
          value_samples = t(colSums(value_samples)),
          .groups = 'drop'
        ) %>%
        mutate(
          inventory = 'nee',
          value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
          value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
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
      levels = c('Bottom-up', 'v2.S posterior', posterior_name),
    )
  )

output <- emissions_monthly %>%
  ggplot(aes(x = time)) +
  geom_ribbon(
    mapping = aes(
      ymin = value_q025,
      ymax = value_q975,
      fill = estimate
    ),
    alpha = 0.3
  ) +
  geom_line(
    mapping = aes(
      y = value,
      colour = estimate,
      linetype = estimate
    ),
    linewidth = 0.5
  ) +
  facet_wrap(vars(inventory), scales = 'free_y', nrow = 2) +
  scale_x_date(date_labels = '%Y-%m') +
  scale_colour_manual(values = colour_key) +
  scale_fill_manual(values = colour_key) +
  scale_linetype_manual(values = linetype_key) +
  guides(fill = 'none') +
  labs(y = 'Flux [PgC/month]', x = NULL, colour = NULL, fill = NULL, linetype = NULL) +
  theme(
    strip.text = element_text(size = 10),
    plot.margin = margin(t = 0, r = 0.1, b = 0, l = 0.1, unit = 'cm'),
    axis.text.x = element_text(size = 8, colour = '#23373b'),
    axis.text.y = element_text(size = 7, colour = '#23373b'),
    axis.title.y = element_text(size = 10, colour = '#23373b'),
    legend.text = element_text(size = 9),
    legend.position = 'bottom',
    legend.margin = margin(t = -0.2, r = 0, b = 0, l = 0, unit = 'cm')
  )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$supplement_full_width,
  height = 8.5
)
