# Combine FOMC projections with actual federal funds rates
# and create visualizations

setwd("/Users/knimm/Documents/FOMC_Project_Vis")

library(tidyverse)
library(ggplot2)
library(gridExtra)

cat("\nFOMC Projections and Actual Rates\n")
cat("---------------------------------\n")

# Load the data
projections <- read.csv("data/projections.csv") %>%
  mutate(SEP_Date = as.Date(SEP_Date))

fed_funds <- read.csv("data/FEDFUNDS.csv") %>%
  mutate(observation_date = as.Date(observation_date))

cat("Projections:", nrow(projections), "rows\n")
cat("Fed funds data:", nrow(fed_funds), "rows\n")


# Keep the December federal funds rate for each year
fed_funds_december <- fed_funds %>%
  mutate(
    PredictionYear = year(observation_date),
    Month = month(observation_date)
  ) %>%
  filter(Month == 12) %>%
  select(PredictionYear, FEDFUNDS) %>%
  rename(ActualFedFundsRate = FEDFUNDS) %>%
  distinct(PredictionYear, .keep_all = TRUE)

cat("December rates:", nrow(fed_funds_december), "years\n")


# Merge the projections with the actual December rate
combined_data <- projections %>%
  mutate(PredictionYear = as.numeric(Year)) %>%
  left_join(
    fed_funds_december,
    by = "PredictionYear"
  ) %>%
  mutate(
    Actual = ActualFedFundsRate,
    Error = Actual - Projection,
    ErrorBasisPoints = Error * 100,
    ErrorPercent = ifelse(
      Projection != 0,
      (Error / Projection) * 100,
      NA
    )
  ) %>%
  select(
    SEP_Date,
    Year,
    Projection,
    PredictionYear,
    Actual,
    Error,
    ErrorBasisPoints,
    ErrorPercent
  )

cat("Combined data:", nrow(combined_data), "rows\n")
cat("Rows with actual rates:",
    sum(!is.na(combined_data$Actual)), "\n")


# Basic summary of prediction errors
accuracy_summary <- combined_data %>%
  filter(!is.na(Error)) %>%
  summarise(
    MeanError = mean(Error, na.rm = TRUE),
    MedianError = median(Error, na.rm = TRUE),
    StdDevError = sd(Error, na.rm = TRUE),
    MeanErrorBasisPoints = mean(ErrorBasisPoints, na.rm = TRUE)
  )

print(accuracy_summary)


# Save the combined dataset
write.csv(
  combined_data,
  "data/combined_projections_actual.csv",
  row.names = FALSE
)


# Use a simple theme for the plots
theme_set(theme_minimal())


# 1. Disagreement between FOMC participants over time

stats <- combined_data %>%
  group_by(SEP_Date, Year) %>%
  summarise(
    StdDev = sd(Projection, na.rm = TRUE),
    IQR = IQR(Projection, na.rm = TRUE),
    Mean = mean(Projection, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  )

plot_disagreement <- stats %>%
  filter(!is.na(StdDev)) %>%
  ggplot(aes(x = SEP_Date, y = StdDev, color = Year)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.6) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  labs(
    title = "FOMC Participant Disagreement Over Time",
    x = "FOMC Meeting Date",
    y = "Standard Deviation of Projections",
    color = "Prediction Year"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    )
  )

ggsave(
  "output/01_disagreement_over_time.png",
  plot_disagreement,
  width = 14,
  height = 6,
  dpi = 300
)


# 2. Average prediction error by year

error_by_year <- combined_data %>%
  filter(!is.na(Error)) %>%
  group_by(Year) %>%
  summarise(
    MeanError = mean(Error, na.rm = TRUE),
    StdError = sd(Error, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(Year = as.numeric(Year))

plot_error_time <- error_by_year %>%
  ggplot(aes(x = Year, y = MeanError, fill = MeanError > 0)) +
  geom_col(width = 0.8, alpha = 0.7) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +
  geom_vline(
    xintercept = 2022,
    linetype = "dashed",
    color = "orange",
    linewidth = 1,
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = 2022,
    y = 2.8,
    label = "Inflation\nShock",
    color = "orange",
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#E74C3C",
      "TRUE" = "#27AE60"
    )
  ) +
  scale_x_continuous(
    breaks = seq(2012, 2025, by = 1)
  ) +
  labs(
    title = "FOMC Prediction Error Over Time",
    subtitle = "Red = actual rate was lower than predicted; green = actual rate was higher",
    x = "Prediction Year",
    y = "Mean Error (percentage points)"
  ) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "output/02_prediction_error_over_time.png",
  plot_error_time,
  width = 14,
  height = 7,
  dpi = 300
)


# 3. Predicted versus actual rates

plot_scatter <- combined_data %>%
  filter(!is.na(Actual)) %>%
  ggplot(aes(x = Projection, y = Actual, color = Year)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "FOMC Predictions vs Actual Federal Funds Rate",
    subtitle = "Points closer to the diagonal represent smaller prediction errors",
    x = "Predicted Rate (%)",
    y = "Actual December Rate (%)",
    color = "Year"
  ) +
  theme(
    legend.position = "right"
  )

ggsave(
  "output/03_predictions_vs_actual.png",
  plot_scatter,
  width = 12,
  height = 8,
  dpi = 300
)


# 4. Distribution of errors by prediction year

plot_error_distribution <- combined_data %>%
  filter(!is.na(Error)) %>%
  mutate(
    Year = factor(
      Year,
      levels = as.character(sort(unique(as.numeric(Year))))
    )
  ) %>%
  ggplot(aes(x = Year, y = Error, fill = Error > 0)) +
  geom_boxplot(
    alpha = 0.7,
    outlier.alpha = 0.3
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#E74C3C",
      "TRUE" = "#27AE60"
    )
  ) +
  labs(
    title = "Distribution of Prediction Errors by Year",
    x = "Prediction Year",
    y = "Error (Actual - Predicted)"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 9
    ),
    legend.position = "none"
  )

ggsave(
  "output/04_error_distribution.png",
  plot_error_distribution,
  width = 14,
  height = 6,
  dpi = 300
)


# 5. Mean error with 95% confidence intervals

error_stats <- combined_data %>%
  filter(!is.na(Error)) %>%
  group_by(Year) %>%
  summarise(
    MeanError = mean(Error, na.rm = TRUE),
    StdError = sd(Error, na.rm = TRUE),
    N = n(),
    SE = StdError / sqrt(N),
    CI_Lower = MeanError - 1.96 * SE,
    CI_Upper = MeanError + 1.96 * SE,
    .groups = "drop"
  ) %>%
  mutate(Year_Num = as.numeric(Year))

plot_ci <- error_stats %>%
  ggplot(aes(x = Year_Num, y = MeanError)) +
  geom_ribbon(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    alpha = 0.2,
    fill = "blue"
  ) +
  geom_line(
    linewidth = 1,
    color = "blue"
  ) +
  geom_point(
    size = 3,
    color = "blue"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  labs(
    title = "Mean Prediction Error with 95% Confidence Intervals",
    x = "Year",
    y = "Mean Error (percentage points)",
    subtitle = "Shaded area represents the 95% confidence interval"
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave(
  "output/05_error_with_ci.png",
  plot_ci,
  width = 12,
  height = 6,
  dpi = 300
)


# 6. Heatmap of prediction errors

heatmap_data <- combined_data %>%
  filter(!is.na(Error)) %>%
  mutate(Month_Year = format(SEP_Date, "%Y-%m")) %>%
  group_by(Month_Year, Year) %>%
  summarise(
    MeanError = mean(Error, na.rm = TRUE),
    .groups = "drop"
  )

plot_heatmap <- heatmap_data %>%
  ggplot(aes(x = Month_Year, y = Year, fill = MeanError)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E74C3C",
    mid = "white",
    high = "#27AE60",
    midpoint = 0
  ) +
  labs(
    title = "Prediction Error by FOMC Date and Prediction Year",
    x = "FOMC Meeting Date",
    y = "Prediction Year",
    fill = "Mean Error"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 7
    )
  )

ggsave(
  "output/06_heatmap_error.png",
  plot_heatmap,
  width = 14,
  height = 8,
  dpi = 300
)


cat("\nAnalysis complete.\n")
cat("Six visualization files were saved to the output folder.\n")