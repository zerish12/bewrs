library(bewrs)
library(ggplot2)

set.seed(123)

n <- 800

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

theme_bewrs <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

# ------------------------------------------------------------
# 1. Calibration plot
# ------------------------------------------------------------

cal <- aggregate(
  cbind(outcome, dynamic_bewrs) ~ risk_group,
  data = example_data,
  FUN = mean
)

p1 <- ggplot(cal, aes(x = dynamic_bewrs, y = outcome)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1,
    colour = "grey40"
  ) +
  geom_line(linewidth = 1.3, colour = "#0072B2") +
  geom_point(size = 4.2, colour = "#D55E00") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Calibration of Dynamic BEWRS",
    subtitle = "Observed event rates across ordered risk strata",
    x = "Mean predicted Dynamic BEWRS risk",
    y = "Observed event rate"
  ) +
  theme_bewrs()

ggsave(
  filename = "man/figures/calibration_plot.png",
  plot = p1,
  width = 8,
  height = 5.5,
  dpi = 450,
  bg = "white"
)

# ------------------------------------------------------------
# 2. Risk group plot
# ------------------------------------------------------------

risk_summary <- aggregate(
  outcome ~ risk_group,
  data = example_data,
  FUN = mean
)

risk_summary$n <- as.numeric(table(example_data$risk_group))

p2 <- ggplot(
  risk_summary,
  aes(x = risk_group, y = outcome, fill = risk_group)
) +
  geom_col(width = 0.72, colour = "grey20", linewidth = 0.35) +
  geom_text(
    aes(label = paste0(round(outcome * 100, 1), "%")),
    vjust = -0.45,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Low" = "#009E73",
      "Watchlist" = "#F0E442",
      "High" = "#E69F00",
      "Critical" = "#D55E00"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Risk Stratification Using Dynamic BEWRS",
    subtitle = "Observed event rate increases across risk categories",
    x = "Dynamic BEWRS risk group",
    y = "Observed event rate",
    fill = "Risk group"
  ) +
  theme_bewrs()

ggsave(
  filename = "man/figures/risk_group_plot.png",
  plot = p2,
  width = 8,
  height = 5.5,
  dpi = 450,
  bg = "white"
)

# ------------------------------------------------------------
# 3. Dynamic BEWRS scatter plot
# ------------------------------------------------------------

p3 <- ggplot(
  example_data,
  aes(x = up, y = dynamic_bewrs, colour = risk_group)
) +
  geom_point(alpha = 0.72, size = 2.4) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 1.2,
    colour = "black"
  ) +
  scale_colour_manual(
    values = c(
      "Low" = "#009E73",
      "Watchlist" = "#F0E442",
      "High" = "#E69F00",
      "Critical" = "#D55E00"
    )
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Dynamic BEWRS Versus Posterior Risk",
    subtitle = "Dynamic scoring incorporates posterior risk, persistence, and deterioration",
    x = "Posterior underperformance probability",
    y = "Dynamic BEWRS",
    colour = "Risk group"
  ) +
  theme_bewrs()

ggsave(
  filename = "man/figures/dynamic_bewrs_scatter.png",
  plot = p3,
  width = 8,
  height = 5.5,
  dpi = 450,
  bg = "white"
)

message("High-resolution BEWRS figures saved to man/figures/")
