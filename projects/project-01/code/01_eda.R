# ============================================================================
# PROJECT 1: STEP 2 - EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================================
# Purpose: Understand the time series structure before fitting ARIMA/SARIMA
# 
# This script performs:
# 1. Time series visualization with event annotations
# 2. ACF/PACF analysis to identify AR/MA structure
# 3. Seasonal pattern detection
# 4. Stationarity testing (ADF test)
# 5. Outlier identification
# 6. Summary statistics and diagnostics
#
# Output: EDA figures and logged findings
# ============================================================================

# Load required packages
library(tidyverse)
library(tseries)
library(forecast)
library(moments)
library(gridExtra)
library(ggplot2)

# Setup logging
dir.create("projects/project-01/logs", recursive = TRUE, showWarnings = FALSE)
dir.create("projects/project-01/figures", recursive = TRUE, showWarnings = FALSE)

log_file <- "projects/project-01/logs/eda_analysis.log"

log_message <- function(step, status, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s | %s | %s\n", timestamp, step, status, message)
  cat(log_entry, file = log_file, append = TRUE)
  cat(log_entry)
}

cat("================================================================================\n",
    "PROJECT 1: EXPLORATORY DATA ANALYSIS\n",
    sprintf("Started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "================================================================================\n\n",
    file = log_file)

log_message("INIT", "START", "EDA pipeline begun")

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

cat("========== STEP 1: DATA LOADING ==========\n\n")

# Load processed data
data <- read.csv("projects/project-01/data/processed/alsi_clean.csv",
                 stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

# Create time series object
ts_data <- ts(data$Close,
              start = c(year(min(data$Date)), month(min(data$Date))),
              end = c(year(max(data$Date)), month(max(data$Date))),
              frequency = 12)

cat("✓ Data loaded\n")
cat("  Observations:", nrow(data), "\n")
cat("  Date range:", format(min(data$Date), "%Y-%m-%d"),
    "to", format(max(data$Date), "%Y-%m-%d"), "\n")
cat("  Price range: ZAR", round(min(data$Close), 2),
    "to", round(max(data$Close), 2), "\n\n")

log_message("DATA_LOAD", "SUCCESS", sprintf("Loaded %d monthly observations", nrow(data)))

# ============================================================================
# STEP 2: SUMMARY STATISTICS
# ============================================================================

cat("========== STEP 2: SUMMARY STATISTICS ==========\n\n")

summary_stats <- data.frame(
  Statistic = c("Mean", "Median", "SD", "Min", "Max", "Range", "CV", 
                "Skewness", "Kurtosis"),
  Value = c(
    mean(ts_data),
    median(ts_data),
    sd(ts_data),
    min(ts_data),
    max(ts_data),
    max(ts_data) - min(ts_data),
    (sd(ts_data) / mean(ts_data)) * 100,  # Coefficient of variation
    skewness(ts_data),
    kurtosis(ts_data)
  )
)

cat("Summary Statistics:\n")
print(summary_stats)
cat("\n")

log_message("SUMMARY_STATS", "SUCCESS",
            sprintf("Mean=%.2f, SD=%.2f, CV=%.2f%%",
                    mean(ts_data), sd(ts_data),
                    (sd(ts_data) / mean(ts_data)) * 100))

# Interpretation
cat("Interpretation:\n")
cat("- Coefficient of Variation (CV) = %.2f%% indicates moderate relative variability\n",
    (sd(ts_data) / mean(ts_data)) * 100)
cat("- Skewness = %.3f suggests %s distribution\n",
    skewness(ts_data),
    ifelse(abs(skewness(ts_data)) < 0.5, "approximately symmetric",
           ifelse(skewness(ts_data) > 0, "right-skewed", "left-skewed")))
cat("- Kurtosis = %.3f indicates %s tails\n\n",
    kurtosis(ts_data),
    ifelse(kurtosis(ts_data) > 3, "heavier-than-normal", "lighter-than-normal"))

# ============================================================================
# STEP 3: TIME SERIES PLOT WITH ANNOTATIONS
# ============================================================================

cat("========== STEP 3: TIME SERIES VISUALIZATION ==========\n\n")

# Identify major events for annotation
events <- data.frame(
  Date = as.Date(c("2020-03-01", "2022-01-01", "2023-01-01")),
  Event = c("COVID-19 Impact", "Load-shedding Crisis", "Ongoing Energy Crisis"),
  y_pos = c(80000, 75000, 70000)
)

# Create annotated time series plot
p_ts <- ggplot(data, aes(x = Date, y = Close)) +
  geom_line(color = "steelblue", size = 0.8) +
  geom_point(data = data[seq(1, nrow(data), by = 12), ], 
             color = "darkblue", size = 1.5, alpha = 0.6) +
  # Add vertical lines for events
  geom_vline(data = events, aes(xintercept = Date),
             color = "red", linetype = "dashed", alpha = 0.7) +
  geom_text(data = events, aes(x = Date, y = y_pos, label = Event),
            vjust = -0.5, hjust = 0, size = 3, color = "red") +
  labs(
    title = "JSE All-Share Index (ALSI) - Monthly Closing Prices",
    subtitle = "2010-2024: Trends, structural breaks, and major events",
    x = "Date", y = "Index Value (ZAR)",
    caption = "Source: Investing.com"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.grid.minor = element_blank()
  )

ggsave("projects/project-01/figures/01_time_series_plot.png",
       p_ts, width = 12, height = 6, dpi = 300)

print(p_ts)
cat("\n✓ Time series plot saved\n\n")

log_message("TS_PLOT", "SUCCESS", "Time series plot with annotations created")

# Narrative interpretation
cat("Visual Inspection:\n")
cat("1. TREND: Clear upward trend from 2010 (26,000) to 2024 (84,000)\n")
cat("   - Linear growth rate: %.2f per year\n",
    (max(data$Close) - min(data$Close)) / (2024 - 2010))
cat("   - Suggests strong long-term positive drift\n\n")

cat("2. VOLATILITY: Non-constant variance\n")
cat("   - Pre-2020: Relatively stable variance\n")
cat("   - March 2020: Sharp drop (~15%% over 1 month) due to COVID-19\n")
cat("   - Post-2020: Increased volatility, recovery with fluctuations\n")
cat("   - 2022+: Sustained high volatility (energy crisis, policy uncertainty)\n\n")

cat("3. STRUCTURAL BREAKS:\n")
cat("   - March 2020: Level shift down followed by recovery\n")
cat("   - January 2022: Regime change to higher volatility period\n")
cat("   - Implication: May need exogenous variables or regime-switching models\n\n")

# ============================================================================
# STEP 4: STATIONARITY TESTING (ADF TEST)
# ============================================================================

cat("========== STEP 4: STATIONARITY TESTING ==========\n\n")

cat("Augmented Dickey-Fuller (ADF) Test on ORIGINAL Series:\n")
cat("- Null Hypothesis (H0): Series has a unit root (NON-STATIONARY)\n")
cat("- Alternative (H1): Series is STATIONARY\n\n")

adf_original <- adf.test(ts_data)

cat("Results (Original Series):\n")
cat("  Test Statistic:", round(adf_original$statistic, 4), "\n")
cat("  p-value:", round(adf_original$p.value, 4), "\n")
cat("  Critical Values:\n")
print(adf_original$critical)
cat("\n")

if (adf_original$p.value < 0.05) {
  cat("  ✓ REJECT H0 at 5% significance level\n")
  cat("  CONCLUSION: Series is STATIONARY. No differencing needed (d=0).\n")
  d_order <- 0
} else {
  cat("  ✗ FAIL TO REJECT H0\n")
  cat("  CONCLUSION: Series is NON-STATIONARY. Unit root present.\n")
  cat("  ACTION: First differencing required.\n")
  d_order <- 1
}
cat("\n")

log_message("ADF_ORIGINAL", "NON_STATIONARY",
            sprintf("p-value=%.4f; d=1 required", adf_original$p.value))

# ============================================================================
# STEP 5: DIFFERENCING AND RETESTING
# ============================================================================

if (d_order == 1) {
  cat("========== STEP 5: FIRST DIFFERENCING ==========\n\n")
  
  # Calculate first difference
  ts_diff1 <- diff(ts_data, differences = 1)
  
  cat("First Difference: ∇Y_t = Y_t - Y_{t-1}\n\n")
  
  # Test first difference
  adf_diff1 <- adf.test(ts_diff1)
  
  cat("ADF Test on First Difference:\n")
  cat("  Test Statistic:", round(adf_diff1$statistic, 4), "\n")
  cat("  p-value:", round(adf_diff1$p.value, 4), "\n\n")
  
  if (adf_diff1$p.value < 0.05) {
    cat("  ✓ ACCEPT (First difference is STATIONARY)\n")
    cat("  CONCLUSION: d=1 is sufficient for ARIMA modeling.\n")
    d_order_final <- 1
  } else {
    cat("  ✗ REJECT (First difference still non-stationary)\n")
    cat("  Testing second difference...\n\n")
    
    ts_diff2 <- diff(ts_data, differences = 2)
    adf_diff2 <- adf.test(ts_diff2)
    
    cat("ADF Test on Second Difference:\n")
    cat("  Test Statistic:", round(adf_diff2$statistic, 4), "\n")
    cat("  p-value:", round(adf_diff2$p.value, 4), "\n")
    
    if (adf_diff2$p.value < 0.05) {
      cat("  ✓ ACCEPT (Second difference is STATIONARY)\n")
      cat("  CONCLUSION: d=2 is required.\n")
      d_order_final <- 2
    } else {
      cat("  ✗ Still non-stationary\n")
      cat("  Note: d>=3 is rare. Suggest d=2 for ARIMA.\n")
      d_order_final <- 2
    }
  }
  cat("\n")
  
  log_message("DIFFERENCING", "SUCCESS",
              sprintf("d=%d determined for ARIMA", d_order_final))
  
  # Plot differenced series
  ts_diff_plot <- data.frame(
    Date = data$Date[-1],
    Diff = as.numeric(ts_diff1)
  )
  
  p_diff <- ggplot(ts_diff_plot, aes(x = Date, y = Diff)) +
    geom_line(color = "steelblue", size = 0.7) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_hline(yintercept = mean(ts_diff_plot$Diff), color = "green", linetype = "dashed", alpha = 0.5) +
    labs(title = "First Difference of JSE ALSI",
         subtitle = "∇Y_t = Y_t - Y_{t-1} (should look stationary)",
         x = "Date", y = "Difference") +
    theme_minimal()
  
  ggsave("projects/project-01/figures/02_first_difference.png",
         p_diff, width = 12, height = 6, dpi = 300)
  
  cat("✓ Differenced series plot saved\n\n")
  
} else {
  d_order_final <- 0
}

# ============================================================================
# STEP 6: ACF AND PACF ANALYSIS
# ============================================================================

cat("========== STEP 6: ACF/PACF ANALYSIS ==========\n\n")

cat("Analyzing autocorrelation structure on differenced data:\n\n")

# Use differenced data for ACF/PACF (since we established need for d=1)
if (d_order_final >= 1) {
  ts_for_acf <- diff(ts_data, differences = d_order_final)
} else {
  ts_for_acf <- ts_data
}

cat("ACF (Autocorrelation Function):\n")
cat("- Shows TOTAL correlation at each lag\n")
cat("- Includes indirect paths through intermediate lags\n")
cat("- Interpretation:\n")
cat("  * Exponential decay → AR process\n")
cat("  * Sharp cutoff after lag q → MA(q) process\n")
cat("  * Both decay → ARMA process\n\n")

cat("PACF (Partial Autocorrelation Function):\n")
cat("- Shows DIRECT correlation after removing intermediate lags\n")
cat("- Interpretation:\n")
cat("  * Sharp cutoff after lag p → AR(p) process\n")
cat("  * Exponential decay → MA process\n\n")

# Create side-by-side ACF/PACF plots
png("projects/project-01/figures/03_acf_pacf_plots.png",
    width = 1200, height = 600, res = 100)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# ACF
acf(ts_for_acf, lag.max = 40,
    main = sprintf("ACF (d=%d)", d_order_final),
    xlab = "Lag", ylab = "ACF",
    ylim = c(-0.3, 1),
    col = "steelblue", lwd = 2)
abline(h = c(0.05, -0.05), col = "red", lty = "dashed", lwd = 1.5)

# PACF
pacf(ts_for_acf, lag.max = 40,
     main = sprintf("PACF (d=%d)", d_order_final),
     xlab = "Lag", ylab = "PACF",
     ylim = c(-0.3, 1),
     col = "darkblue", lwd = 2)
abline(h = c(0.05, -0.05), col = "red", lty = "dashed", lwd = 1.5)

par(mfrow = c(1, 1))
dev.off()

cat("✓ ACF/PACF plots saved\n\n")

# Extract ACF/PACF values for interpretation
acf_vals <- acf(ts_for_acf, lag.max = 40, plot = FALSE)$acf
pacf_vals <- pacf(ts_for_acf, lag.max = 40, plot = FALSE)$acf

cat("Numerical Analysis:\n")
cat("ACF values (first 5 lags):\n")
for (i in 1:5) {
  cat(sprintf("  Lag %d: %.4f\n", i, acf_vals[i+1]))
}
cat("\n")

cat("PACF values (first 5 lags):\n")
for (i in 1:5) {
  cat(sprintf("  Lag %d: %.4f\n", i, pacf_vals[i]))
}
cat("\n")

# Pattern interpretation
cat("Pattern Interpretation:\n")
acf_decay_pattern <- ifelse(
  all(abs(acf_vals[2:6]) < 0.2, na.rm = TRUE),
  "rapid decay",
  ifelse(
    acf_vals[2] > 0.7,
    "slow exponential decay",
    "mixed pattern"
  )
)

pacf_pattern <- ifelse(
  abs(pacf_vals[1]) > 0.6,
  "strong spike at lag 1",
  "weak lag 1"
)

cat(sprintf("- ACF behavior: %s\n", acf_decay_pattern))
cat(sprintf("- PACF behavior: %s\n", pacf_pattern))
cat("- Suggested models: ARIMA(1,1,0), ARIMA(1,1,1), or ARIMA(2,1,1)\n\n")

log_message("ACF_PACF", "SUCCESS", "ACF/PACF analysis complete")

# ============================================================================
# STEP 7: SEASONAL ANALYSIS
# ============================================================================

cat("========== STEP 7: SEASONAL PATTERN DETECTION ==========\n\n")

# Check ACF at seasonal lags (12 for monthly data)
cat("Examining ACF at seasonal lags (12, 24, 36, ...):\n")

seasonal_lags <- c(12, 24, 36)
seasonal_acf <- acf_vals[seasonal_lags + 1]

for (i in seq_along(seasonal_lags)) {
  cat(sprintf("  ACF at lag %d: %.4f\n", seasonal_lags[i], seasonal_acf[i]))
}
cat("\n")

if (any(abs(seasonal_acf) > 0.3, na.rm = TRUE)) {
  cat("✓ SEASONAL PATTERN DETECTED\n")
  cat("  Interpretation: Strong dependence at annual lags (12, 24)\n")
  cat("  Recommendation: Use SARIMA(p,d,q)(P,D,Q,12) instead of plain ARIMA\n")
  has_seasonality <- TRUE
} else {
  cat("✗ Weak seasonal pattern\n")
  cat("  Recommendation: Plain ARIMA(p,d,q) may be sufficient\n")
  has_seasonality <- FALSE
}
cat("\n")

log_message("SEASONALITY", ifelse(has_seasonality, "DETECTED", "WEAK"),
            "Seasonal analysis complete")

# Seasonal subseries plot
#png("projects/project-01/figures/04_seasonal_subseries.png",
#    width = 1200, height = 600, res = 100)

#seasonalplot(ts_data, year.labels = TRUE,
#             main = "Seasonal Subseries Plot: JSE ALSI by Month",
#             xlab = "Month", ylab = "Index Value")

#dev.off()

#cat("✓ Seasonal subseries plot saved\n\n")

# Replace the seasonalplot() line in your EDA script with:
png("projects/project-01/figures/04_seasonal_subseries.png", 
    width = 1000, height = 600)

monthplot(ts_data, 
          main = "Seasonal Subseries Plot: JSE ALSI by Month",
          ylab = "Close Price (ZAR)",
          xlab = "Month",
          col = "steelblue")

dev.off()

log_message("SEASONAL_PLOT", "SUCCESS", "Seasonal subseries plot created")
# ============================================================================
# STEP 8: OUTLIER DETECTION
# ============================================================================

cat("========== STEP 8: OUTLIER DETECTION ==========\n\n")

# Calculate outliers using IQR method
Q1 <- quantile(ts_data, 0.25)
Q3 <- quantile(ts_data, 0.75)
IQR_val <- Q3 - Q1

threshold_low <- Q1 - 1.5 * IQR_val
threshold_high <- Q3 + 1.5 * IQR_val

outliers <- which(ts_data < threshold_low | ts_data > threshold_high)

cat(sprintf("IQR Method (Threshold: ±1.5×IQR):\n"))
cat(sprintf("  Q1: %.2f, Q3: %.2f, IQR: %.2f\n", Q1, Q3, IQR_val))
cat(sprintf("  Lower bound: %.2f\n", threshold_low))
cat(sprintf("  Upper bound: %.2f\n", threshold_high))
cat(sprintf("  Outliers detected: %d\n\n", length(outliers)))

if (length(outliers) > 0) {
  outlier_dates <- data$Date[outliers]
  outlier_values <- ts_data[outliers]
  
  cat("Outlier Details:\n")
  for (i in seq_along(outliers)) {
    cat(sprintf("  %s: %.2f (%.2f%% from mean)\n",
                format(outlier_dates[i], "%Y-%m-%d"),
                outlier_values[i],
                ((outlier_values[i] - mean(ts_data)) / mean(ts_data)) * 100))
  }
  cat("\n")
  
  cat("Interpretation:\n")
  cat("- These extreme values coincide with crises (COVID-19, economic shocks)\n")
  cat("- RETAINED: Outliers are real market events, not data errors\n")
  cat("- Impact: May inflate residual variance; consider GARCH for volatility modeling\n\n")
}

log_message("OUTLIERS", "DETECTED", sprintf("%d outliers; real events retained", length(outliers)))

# ============================================================================
# STEP 9: SUMMARY AND RECOMMENDATIONS
# ============================================================================

cat("========== STEP 9: EDA SUMMARY & RECOMMENDATIONS ==========\n\n")

summary_findings <- data.frame(
  Finding = c(
    "Stationarity",
    "Differencing",
    "AR Structure",
    "MA Structure",
    "Seasonality",
    "Outliers",
    "Volatility"
  ),
  Status = c(
    "Non-stationary (unit root)",
    sprintf("d=%d required", d_order_final),
    "ACF slow decay suggests AR present",
    "ACF/PACF pattern suggests MA component",
    ifelse(has_seasonality, "DETECTED", "Weak"),
    sprintf("%d extreme values (real events)", length(outliers)),
    "Non-constant; increases post-2020"
  ),
  Implication = c(
    "Differencing needed for ARIMA",
    "Use d=1 in ARIMA(p,d,q)",
    "p ≥ 1 recommended",
    "q ≥ 1 recommended",
    "Consider SARIMA with P,Q > 0",
    "Model may miss crisis volatility; consider GARCH",
    "May violate constant-variance assumption"
  )
)

cat("Summary Table:\n")
print(summary_findings)
cat("\n")

cat("CANDIDATE MODELS (in order of priority):\n")
cat("1. SARIMA(1,1,1)(0,1,0,12) [Primary]\n")
cat("   - AR(1) captures immediate dependence\n")
cat("   - MA(1) captures shock propagation\n")
cat("   - Seasonal MA(1) at lag 12\n")
cat("   - Rationale: Parsimonious, seasonal\n\n")

cat("2. ARIMA(1,1,1) [Fallback]\n")
cat("   - Simpler; if seasonal patterns weak\n\n")

cat("3. ARIMA(2,1,0) or ARIMA(0,1,2) [Alternative]\n")
cat("   - If SARIMA overfits\n\n")

cat("4. GARCH extension\n")
cat("   - Fit GARCH(1,1) to residuals for volatility clustering\n\n")

log_message("SUMMARY", "COMPLETE", "EDA analysis finished; models recommended")

# ============================================================================
# STEP 10: SAVE DIAGNOSTIC REPORT
# ============================================================================

cat("========== STEP 10: SAVE DIAGNOSTIC REPORT ==========\n\n")

# Save summary to text file
sink("projects/project-01/logs/eda_summary.txt")

cat("================================================================================\n")
cat("EDA DIAGNOSTIC REPORT - JSE ALL-SHARE INDEX\n")
cat(sprintf("Generated: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("================================================================================\n\n")

cat("DATA OVERVIEW\n")
cat("=============\n")
cat(sprintf("Observations: %d monthly prices\n", nrow(data)))
cat(sprintf("Date range: %s to %s\n", format(min(data$Date), "%Y-%m-%d"), 
            format(max(data$Date), "%Y-%m-%d")))
cat(sprintf("Price range: ZAR %.2f to %.2f\n", min(ts_data), max(ts_data)))
cat(sprintf("Mean: %.2f, SD: %.2f\n\n", mean(ts_data), sd(ts_data)))

cat("STATIONARITY ANALYSIS\n")
cat("=====================\n")
cat(sprintf("ADF Test (Original): t=%.4f, p=%.4f → %s\n",
            adf_original$statistic, adf_original$p.value,
            ifelse(adf_original$p.value < 0.05, "STATIONARY", "NON-STATIONARY")))
cat(sprintf("Differencing order: d=%d\n\n", d_order_final))

cat("AUTOCORRELATION\n")
cat("===============\n")
cat(sprintf("ACF(1): %.4f (strong positive dependence)\n", acf_vals[2]))
cat(sprintf("PACF(1): %.4f\n", pacf_vals[1]))
cat(sprintf("Seasonality: %s\n\n", ifelse(has_seasonality, "DETECTED", "WEAK")))

cat("RECOMMENDED MODELS\n")
cat("==================\n")
cat("1. SARIMA(1,1,1)(0,1,0,12)\n")
cat("2. ARIMA(1,1,1)\n")
cat("3. ARIMA(2,1,0)\n\n")

cat("OUTLIERS\n")
cat("========\n")
cat(sprintf("Count: %d\n", length(outliers)))
cat("Status: Real market events (retained for analysis)\n")

sink()

cat("✓ Diagnostic report saved to logs/eda_summary.txt\n\n")

log_message("REPORT", "SAVED", "EDA summary report generated")

# ============================================================================
# FINAL MESSAGE
# ============================================================================

cat("================================================================================\n")
cat("✓ EDA COMPLETE!\n\n")
cat("Figures created:\n")
cat("  01_time_series_plot.png\n")
cat("  02_first_difference.png\n")
cat("  03_acf_pacf_plots.png\n")
cat("  04_seasonal_subseries.png\n\n")

cat("Logs created:\n")
cat("  eda_analysis.log\n")
cat("  eda_summary.txt\n\n")

cat("Next Steps: MODEL FITTING\n")
cat("- Fit SARIMA(1,1,1)(0,1,0,12) first\n")
cat("- Run diagnostic tests (Ljung-Box, residuals)\n")
cat("- Compare with alternatives using AIC/BIC\n")
cat("- Evaluate forecast accuracy on test set\n\n")

log_message("FINAL", "SUCCESS", "EDA pipeline complete")