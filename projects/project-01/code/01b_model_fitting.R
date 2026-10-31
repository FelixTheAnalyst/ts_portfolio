# ============================================================================
# 01B_MODEL_FITTING.R
# ARIMA/GARCH Model Comparison on JSE ALSI Monthly Data
# Fits 4 candidate ARIMA models + GARCH, evaluates via AIC/BIC, 
# Ljung-Box diagnostics, and forecast accuracy metrics
# ============================================================================

library(tidyverse)
library(tseries)
library(forecast)
library(fGarch)  # For GARCH modeling
library(gridExtra)

# ----- Setup: Logging & File Paths -----
log_file <- "projects/project-01/logs/model_fitting.log"
cat("", file = log_file)  # Clear previous log

log_message <- function(step, status, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s | %s | %s\n", timestamp, step, status, message)
  cat(log_entry, file = log_file, append = TRUE)
  cat(log_entry)
}

# Load processed data
data_path <- "projects/project-01/data/processed/alsi_clean.csv"
alsi_df <- read_csv(data_path)
ts_data <- ts(alsi_df$Close, start = c(2010, 1), frequency = 12)
diff_data <- diff(ts_data)  # First difference for ARIMA

log_message("INIT", "START", "Model fitting pipeline begun")

# ============================================================================
# SECTION 1: CANDIDATE ARIMA MODELS
# ============================================================================

# Define 4 candidate models: (p, d=1, q) structures based on ACF/PACF
candidates <- list(
  "ARIMA(1,1,0)" = c(1, 1, 0),
  "ARIMA(1,1,1)" = c(1, 1, 1),
  "ARIMA(2,1,1)" = c(2, 1, 1),
  "ARIMA(0,1,1)" = c(0, 1, 1)
)

# Fit each model
fits <- lapply(candidates, function(order) {
  arima(ts_data, order = order)
})

log_message("ARIMA_FIT", "SUCCESS", "Fitted 4 candidate ARIMA models")

# Extract AIC/BIC for model comparison
model_comparison <- tibble(
  Model = names(candidates),
  AIC = sapply(fits, AIC),
  BIC = sapply(fits, BIC),
  logLik = sapply(fits, logLik),
  RMSE = sapply(fits, function(x) sqrt(mean(x$residuals^2)))
) %>%
  mutate(
    AIC_rank = rank(AIC),
    BIC_rank = rank(BIC),
    Preferred = ifelse(AIC == min(AIC), "✓ BEST", "")
  ) %>%
  arrange(AIC)

cat("\n========== MODEL COMPARISON: AIC/BIC ==========\n")
print(model_comparison %>% select(Model, AIC, BIC, RMSE, AIC_rank, BIC_rank, Preferred))
cat("\n")

log_message("MODEL_COMP", "SUCCESS", 
            sprintf("Best model (AIC): %s", model_comparison$Model[1]))

# ============================================================================
# SECTION 2: DIAGNOSTIC TESTS ON BEST MODEL
# ============================================================================

best_model_name <- model_comparison$Model[1]
best_fit <- fits[[best_model_name]]
residuals_arima <- best_fit$residuals

cat(sprintf("\n========== DIAGNOSTICS: %s ==========\n", best_model_name))

# --- Test 1: Ljung-Box Test for White Noise ---
# H0: Residuals are white noise (no autocorrelation)
# H1: Residuals have autocorrelation (model inadequate)
lb_test <- Box.test(residuals_arima, lag = 12, type = "Ljung-Box")

cat(sprintf("\nLjung-Box Test (H0: residuals ~ white noise):\n"))
cat(sprintf("  Q-statistic: %.4f\n", lb_test$statistic))
cat(sprintf("  p-value: %.4f\n", lb_test$p.value))
cat(sprintf("  Lag: %d\n", 12))

if (lb_test$p.value > 0.05) {
  cat("  ✓ PASS: Residuals appear white noise (model adequate)\n")
  lb_status <- "PASS"
} else {
  cat("  ✗ FAIL: Residuals show autocorrelation (model inadequate)\n")
  lb_status <- "FAIL"
}

log_message("LJUNG_BOX", lb_status, 
            sprintf("p=%.4f; lag=12", lb_test$p.value))

# --- Test 2: Normality of Residuals (Shapiro-Wilk) ---
shapiro_test <- shapiro.test(residuals_arima)

cat(sprintf("\nShapiro-Wilk Test (H0: residuals ~ Normal):\n"))
cat(sprintf("  W-statistic: %.4f\n", shapiro_test$statistic))
cat(sprintf("  p-value: %.4f\n", shapiro_test$p.value))

if (shapiro_test$p.value > 0.05) {
  cat("  ✓ Residuals approximately normal\n")
  sw_status <- "NORMAL"
} else {
  cat("  ✗ Residuals deviate from normality (common in finance)\n")
  sw_status <- "NON_NORMAL"
}

log_message("SHAPIRO_WILK", sw_status, 
            sprintf("p=%.4f", shapiro_test$p.value))

# --- Test 3: ACF of Residuals (visual confirmation) ---
# Should show no significant autocorrelation if model is adequate

acf_res <- acf(residuals_arima, lag.max = 20, plot = FALSE)
significant_acf <- sum(abs(acf_res$acf[-1]) > qnorm(0.975) / sqrt(length(residuals_arima)))

cat(sprintf("\nResidual ACF (lags 1-20):\n"))
cat(sprintf("  Significant lags (5%% level): %d out of 20\n", significant_acf))
if (significant_acf <= 2) {
  cat("  ✓ PASS: Few significant autocorrelations\n")
} else {
  cat("  ✗ WARNING: Multiple significant lags suggest model misspecification\n")
}

# ============================================================================
# SECTION 3: GARCH EXTENSION
# ============================================================================

# GARCH(1,1) models volatility clustering in residuals
# Specification: σ²_t = ω + αε²_{t-1} + βσ²_{t-1}

cat("\n========== GARCH(1,1) ON ARIMA RESIDUALS ==========\n")

# Fit GARCH on demeaned residuals
res_demeaned <- residuals_arima - mean(residuals_arima)

# Using fGarch::garchFit
garch_fit <- fGarch::garchFit(
  formula = ~ garch(1, 1),
  data = res_demeaned,
  trace = FALSE
)

cat(sprintf("\nGARCH(1,1) Estimates:\n"))
#print(summary(garch_fit)@fit$matcoef[, 1:2])

# Extract GARCH parameters
omega_garch <- garch_fit@fit$coef["omega"]
alpha_garch <- garch_fit@fit$coef["alpha1"]
beta_garch <- garch_fit@fit$coef["beta1"]
persistence <- alpha_garch + beta_garch

# Instead, the GARCH summary() output was already printed above,
# and we extract parameters directly:
cat(sprintf("\nGARCH(1,1) Parameter Summary:\n"))
cat(sprintf("  ω (baseline vol²):    %.2f\n", omega_garch))
cat(sprintf("  α₁ (shock response):  %.4f\n", alpha_garch))
cat(sprintf("  β₁ (persistence):     %.4f\n", beta_garch))
cat(sprintf("  Persistence (α+β):    %.4f\n", persistence))

cat(sprintf("\nVolatility Persistence: α + β = %.4f\n", persistence))
if (persistence < 1) {
  cat("  ✓ STATIONARY (variance reverts to long-run level)\n")
  garch_status <- "STATIONARY"
} else if (persistence >= 1) {
  cat("  ✗ UNIT ROOT (persistent volatility, hard to forecast)\n")
  garch_status <- "PERSISTENT"
}

log_message("GARCH_FIT", garch_status, 
            sprintf("ω=%.6f, α=%.4f, β=%.4f, persist=%.4f", 
                    omega_garch, alpha_garch, beta_garch, persistence))

# ============================================================================
# SECTION 4: FORECAST COMPARISON (Train/Test Split)
# ============================================================================

# 80/20 split: first 144 observations for training, last 36 for testing
n_train <- 144  # 12 years
n_test <- 36    # 3 years

ts_train <- ts(ts_data[1:n_train], start = c(2010, 1), frequency = 12)
ts_test <- ts(ts_data[(n_train+1):length(ts_data)], 
              start = c(2022, 1), frequency = 12)

cat(sprintf("\n========== TRAIN/TEST SPLIT ==========\n"))
cat(sprintf("Training period: %d observations (2010-01 to 2021-12)\n", n_train))
cat(sprintf("Test period: %d observations (2022-01 to 2024-12)\n", n_test))

# --- Fit ARIMA on training set ---
fit_train <- arima(ts_train, order = c(1, 1, 1))

# --- Generate h=36 month-ahead forecasts ---
forecast_36 <- forecast(fit_train, h = n_test)
point_forecasts <- forecast_36$mean

# --- Calculate forecast accuracy metrics ---
residuals_fcast <- as.numeric(ts_test) - point_forecasts

mae <- mean(abs(residuals_fcast))
rmse <- sqrt(mean(residuals_fcast^2))
mape <- mean(abs(residuals_fcast / as.numeric(ts_test))) * 100

cat(sprintf("\nForecast Accuracy (36-month ahead):\n"))
cat(sprintf("  MAE:  ZAR %.2f\n", mae))
cat(sprintf("  RMSE: ZAR %.2f\n", rmse))
cat(sprintf("  MAPE: %.2f%%\n", mape))

log_message("FORECAST_ACC", "SUCCESS", 
            sprintf("MAE=%.2f, RMSE=%.2f, MAPE=%.2f%%", mae, rmse, mape))

# ============================================================================
# SECTION 5: SUMMARY TABLE
# ============================================================================

summary_table <- tibble(
  Metric = c(
    "Best ARIMA Model",
    "AIC",
    "Ljung-Box p-value",
    "Residuals Normal?",
    "GARCH α+β (persistence)",
    "Forecast RMSE (36-month)",
    "Forecast MAPE"
  ),
  Value = c(
    best_model_name,
    sprintf("%.2f", model_comparison$AIC[1]),
    sprintf("%.4f", lb_test$p.value),
    sw_status,
    sprintf("%.4f", persistence),
    sprintf("ZAR %.2f", rmse),
    sprintf("%.2f%%", mape)
  )
)

cat("\n========== FINAL SUMMARY ==========\n")
print(summary_table)

log_message("FINAL", "COMPLETE", "Model fitting and diagnostics finished")

cat("\nLog saved to: projects/project-01/logs/model_fitting.log\n")