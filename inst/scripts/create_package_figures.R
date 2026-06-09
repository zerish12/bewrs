library(bewrs)
library(ggplot2)

set.seed(123)

n <- 500

example_data <- data.frame(
  up = runif(n, 0.01, 0.95),
  persistence = runif(n, 0, 1),
  deterioration = rnorm(n, 0, 0.25)
)

example_data$dynamic_bewrs <- compute_dynamic_bewrs(
  up = example_data$up,
  persistence = example_data$persistence,
  deterioration = example_data$deterioration,
  alpha = -0.3,
  beta_up = 1.2,
  beta_persistence = 0.8,
  beta_deterioration = 0.6
)

example_data$outcome <- rbinom(n, 1, example_data$dynamic_bewrs)

example_data$risk_group <- risk_stratify(example_data$dynamic_bewrs)

p1 <- plot_calibration(
  observed = example_data$outcome,
  predicted = example_data$dynamic_bewrs,
  groups = 10
)

ggsave(
  filename = "man/figures/calibration_plot.png",
  plot = p1,
  width = 7,
  height = 5,
  dpi = 300
)

p2 <- plot_risk_groups(
  observed = example_data$outcome,
  risk_group = example_data$risk_group
)

ggsave(
  filename = "man/figures/risk_group_plot.png",
  plot = p2,
  width = 7,
  height = 5,
  dpi = 300
)

p3 <- ggplot(example_data, aes(x = up, y = dynamic_bewrs)) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Posterior underperformance probability",
    y = "Dynamic BEWRS",
    title = "Dynamic BEWRS versus posterior risk"
  ) +
  theme_minimal()

ggsave(
  filename = "man/figures/dynamic_bewrs_scatter.png",
  plot = p3,
  width = 7,
  height = 5,
  dpi = 300
)
