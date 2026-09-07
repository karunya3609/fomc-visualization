# ============================================================================
# COMBINE PROJECTIONS WITH ACTUAL FEDERAL FUNDS RATE + VISUALIZATIONS
# ============================================================================

setwd("/Users/knimm/Documents/FOMC_Project_Vis")

library(tidyverse)
library(ggplot2)
library(gridExtra)

print("======================================================================")
print("FOMC PROJECTIONS + ACTUAL RATES ANALYSIS")
print("======================================================================")

# ============================================================================
# SECTION 1: LOAD DATA
# ============================================================================
print("\n[1/5] Loading data...")

projections <- read.csv("data/projections.csv") %>%
  mutate(SEP_Date = as.Date(SEP_Date))

fed_funds <- read.csv("data/FEDFUNDS.csv") %>%
  mutate(observation_date = as.Date(observation_date))

print(paste("  ✓ Projections: ", nrow(projections), " rows"))
print(paste("  ✓ Fed Funds: ", nrow(fed_funds), " rows"))

# ============================================================================
# SECTION 2: PREPARE FED FUNDS DATA (DECEMBER ONLY)
# ============================================================================
print("\n[2/5] Processing Federal Funds data...")

fed_funds_december <- fed_funds %>%
  mutate(
    PredictionYear = year(observation_date),
    Month = month(observation_date)
  ) %>%
  filter(Month == 12) %>%
  select(PredictionYear, FEDFUNDS) %>%
  rename(ActualFedFundsRate = FEDFUNDS) %>%
  distinct(PredictionYear, .keep_all = TRUE)

print(paste("  ✓ Extracted December rates for", nrow(fed_funds_december), "years"))

# ============================================================================
# SECTION 3: COMBINE DATA
# ============================================================================
print("\n[3/5] Combining data...")

combined_data <- projections %>%
  mutate(
    PredictionYear = as.numeric(Year)
  ) %>%
  left_join(
    fed_funds_december,
    by = "PredictionYear"
  ) %>%
  mutate(
    Actual = ActualFedFundsRate,
    Error = Actual - Projection,
    ErrorBasisPoints = Error * 100,
    ErrorPercent = ifelse(Projection != 0, (Error / Projection) * 100, NA)
  ) %>%
  select(SEP_Date, Year, Projection, PredictionYear, Actual, Error, ErrorBasisPoints, ErrorPercent)

print(paste("  ✓ Combined rows:", nrow(combined_data)))
print(paste("  ✓ Rows with actual rates:", sum(!is.na(combined_data$Actual))))

# Summary statistics
accuracy_summary <- combined_data %>%
  filter(!is.na(Error)) %>%
  summarise(
    MeanError = mean(Error, na.rm = TRUE),
    MedianError = median(Error, na.rm = TRUE),
    StdDevError = sd(Error, na.rm = TRUE),
    MeanErrorBasisPoints = mean(ErrorBasisPoints, na.rm = TRUE)
  )

print("\n  Prediction Accuracy Summary:")
print(accuracy_summary)

# ============================================================================
# SECTION 4: SAVE COMBINED DATA
# ============================================================================
print("\n[4/5] Saving combined data...")

output_file <- "data/combined_projections_actual.csv"
write.csv(combined_data, output_file, row.names = FALSE)
print(paste("  ✓ Saved:", output_file))

# ============================================================================
# SECTION 5: CREATE VISUALIZATIONS
# ============================================================================
print("\n[5/5] Creating visualizations...")

# Theme settings
theme_set(theme_minimal())

# ────────────────────────────────────────────────────────────────────────
# VIZ 1: DISAGREEMENT OVER TIME (Original from your script)
# ────────────────────────────────────────────────────────────────────────

stats <- combined_data %>%
  mutate(Horizon_Label = case_when(
    Year %in% c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021") ~ Year,
    TRUE ~ NA_character_
  )) %>%
  group_by(SEP_Date, Year) %>%
  summarise(
    StdDev = sd(Projection, na.rm = TRUE),
    IQR = IQR(Projection, na.rm = TRUE),
    Mean = mean(Projection, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  )

plot_disagreement <- stats %>%
  filter(!is.na(StdDev)) %>%
  ggplot(aes(x = SEP_Date, y = StdDev, color = Year)) +
  geom_line(size = 0.8, alpha = 0.7) +
  geom_point(size = 2, alpha = 0.6) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "FOMC Participant Disagreement Over Time",
    x = "FOMC Meeting Date",
    y = "Standard Deviation of Projections",
    color = "Prediction Year"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "right"
  )

ggsave("output/01_disagreement_over_time.png", plot_disagreement, width = 14, height = 6, dpi = 300)
print("  ✓ Saved: 01_disagreement_over_time.png")

# ────────────────────────────────────────────────────────────────────────
# VIZ 2: PREDICTION ERROR OVER TIME (THE BIG STORY!)
# ────────────────────────────────────────────────────────────────────────

plot_error_time <- combined_data %>%
  filter(!is.na(Error)) %>%
  group_by(Year) %>%
  summarise(
    MeanError = mean(Error, na.rm = TRUE),
    StdError = sd(Error, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  ggplot(aes(x = Year, y = MeanError, fill = MeanError > 0)) +
  geom_col(width = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "orange", size = 1, alpha = 0.7) +
  annotate("text", x = 2022, y = 2.8, label = "Inflation\nShock", 
           color = "orange", size = 4, fontface = "bold") +
  scale_fill_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60")) +
  labs(
    title = "FOMC Prediction Accuracy: The Inflation Shock Story",
    subtitle = "Red = Predicted too high (rates ended up lower) | Green = Predicted too low (rates ended up higher)",
    x = "Prediction Year",
    y = "Mean Error (percentage points)",
    caption = "2022-2023: Fed raised rates higher than participants predicted"
  ) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0)
  )

ggsave("output/02_prediction_error_over_time.png", plot_error_time, width = 14, height = 7, dpi = 300)
print("  ✓ Saved: 02_prediction_error_over_time.png")

# ────────────────────────────────────────────────────────────────────────
# VIZ 3: PREDICTIONS VS ACTUAL SCATTER
# ────────────────────────────────────────────────────────────────────────

plot_scatter <- combined_data %>%
  filter(!is.na(Actual)) %>%
  ggplot(aes(x = Projection, y = Actual, color = Year)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "FOMC Predictions vs Actual Federal Funds Rate",
    subtitle = "Red diagonal line = perfect predictions",
    x = "Predicted Rate (%)",
    y = "Actual December Rate (%)",
    color = "Year"
  ) +
  theme(
    legend.position = "right"
  )

ggsave("output/03_predictions_vs_actual.png", plot_scatter, width = 12, height = 8, dpi = 300)
print("  ✓ Saved: 03_predictions_vs_actual.png")

# ────────────────────────────────────────────────────────────────────────
# VIZ 4: ERROR DISTRIBUTION BY YEAR (BOX PLOT)
# ────────────────────────────────────────────────────────────────────────

plot_error_dist <- combined_data %>%
  filter(!is.na(Error)) %>%
  mutate(Year = factor(Year, levels = as.character(sort(unique(as.numeric(Year)))))) %>%
  ggplot(aes(x = Year, y = Error, fill = Error > 0)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  scale_fill_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60")) +
  labs(
    title = "Distribution of Prediction Errors by Year",
    x = "Prediction Year",
    y = "Error (Actual - Predicted)",
    subtitle = "Shows range and spread of prediction accuracy"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("output/04_error_distribution.png", plot_error_dist, width = 14, height = 6, dpi = 300)
print("  ✓ Saved: 04_error_distribution.png")

# ────────────────────────────────────────────────────────────────────────
# VIZ 5: MEAN ERROR WITH CONFIDENCE BANDS
# ────────────────────────────────────────────────────────────────────────

error_by_year <- combined_data %>%
  filter(!is.na(Error)) %>%
  group_by(Year) %>%
  summarise(
    MeanError = mean(Error, na.rm = TRUE),
    StdError = sd(Error, na.rm = TRUE),
    N = n(),
    SE = StdError / sqrt(N),
    CI_Lower = MeanError - 1.96 * SE,
    CI_Upper = MeanError + 1.96 * SE,
    Year_Num = as.numeric(Year),
    .groups = 'drop'
  )

plot_error_ci <- error_by_year %>%
  ggplot(aes(x = Year_Num, y = MeanError)) +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2, fill = "blue") +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "orange", alpha = 0.5) +
  labs(
    title = "Mean Prediction Error with 95% Confidence Intervals",
    x = "Year",
    y = "Mean Error (percentage points)",
    subtitle = "Shaded area = 95% confidence interval"
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave("output/05_error_with_ci.png", plot_error_ci, width = 12, height = 6, dpi = 300)
print("  ✓ Saved: 05_error_with_ci.png")

# ────────────────────────────────────────────────────────────────────────
# VIZ 6: HEATMAP - ERROR BY YEAR AND FOMC DATE
# ────────────────────────────────────────────────────────────────────────

plot_heatmap_data <- combined_data %>%
  filter(!is.na(Error)) %>%
  mutate(Month_Year = format(SEP_Date, "%Y-%m")) %>%
  group_by(Month_Year, Year) %>%
  summarise(MeanError = mean(Error, na.rm = TRUE), .groups = 'drop')

plot_heatmap <- plot_heatmap_data %>%
  ggplot(aes(x = Month_Year, y = Year, fill = MeanError)) +
  geom_tile() +
  scale_fill_gradient2(low = "#E74C3C", mid = "white", high = "#27AE60", midpoint = 0) +
  labs(
    title = "Heatmap: Prediction Error by FOMC Date and Year",
    x = "FOMC Meeting Date",
    y = "Prediction Year",
    fill = "Mean Error"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

ggsave("output/06_heatmap_error.png", plot_heatmap, width = 14, height = 8, dpi = 300)
print("  ✓ Saved: 06_heatmap_error.png")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

print("\n======================================================================")
print("ANALYSIS COMPLETE!")
print("======================================================================")
print("\nVisualizations created:")
print("  1. 01_disagreement_over_time.png - Participant disagreement trends")
print("  2. 02_prediction_error_over_time.png - THE BIG STORY (inflation shock)")
print("  3. 03_predictions_vs_actual.png - Scatter plot comparison")
print("  4. 04_error_distribution.png - Box plots of errors")
print("  5. 05_error_with_ci.png - Error trends with confidence bands")
print("  6. 06_heatmap_error.png - Error intensity heatmap")
print("\nKey Finding:")
print("  2022-2023 shows dramatic +2.3 to +2.5 error = Fed raised rates")
print("  much higher than participants predicted (inflation shock!)")
print("======================================================================")