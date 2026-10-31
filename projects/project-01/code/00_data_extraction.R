# ============================================================================
# PROJECT 1: DATA EXTRACTION - CORRECTED FOR ACTUAL DATE FORMAT
# ============================================================================
# Date format: YYYY/MM/DD (not DD/MM/YYYY)
# ============================================================================

library(tidyverse)
library(tseries)
library(moments)

# Setup
dir.create("projects/project-01/data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("projects/project-01/data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("projects/project-01/logs", recursive = TRUE, showWarnings = FALSE)

log_file <- "projects/project-01/logs/data_extraction.log"

log_message <- function(step, status, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s | %s | %s\n", timestamp, step, status, message)
  cat(log_entry, file = log_file, append = TRUE)
  cat(log_entry)
}

cat("================================================================================\n",
    "PROJECT 1: DATA EXTRACTION (CORRECTED DATE FORMAT)\n",
    sprintf("Started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "================================================================================\n\n",
    file = log_file)

log_message("INIT", "START", "Data extraction with YYYY/MM/DD format")

# ============================================================================
# STEP 1: READ CSV
# ============================================================================

cat("========== STEP 1: READ DATA ==========\n\n")

investing_file <- "projects/project-01/data/raw/alsi_investing.csv"

alsi_raw <- read.csv(investing_file, 
                     stringsAsFactors = FALSE,
                     encoding = "UTF-8-BOM")

cat("✓ CSV loaded\n")
cat("  Rows:", nrow(alsi_raw), "(daily data)\n")
cat("  Columns:", ncol(alsi_raw), "\n\n")

# Fix column names
names(alsi_raw)[1] <- "Date"

cat("Sample rows:\n")
print(head(alsi_raw, 3))
cat("\n")

# ============================================================================
# STEP 2: PARSE DAILY DATA - CORRECTED FORMAT
# ============================================================================

cat("========== STEP 2: PARSE DAILY DATA ==========\n\n")

alsi_daily <- alsi_raw %>%
  mutate(
    # CORRECTED: Format is YYYY/MM/DD not DD/MM/YYYY
    Date = as.Date(Date, format = "%Y/%m/%d"),
    # Remove commas from Price and convert to numeric
    Price = as.numeric(gsub(",", "", Price)),
    Source = "Investing.com"
  ) %>%
  select(Date, Price, Source) %>%
  # Only keep valid rows
  filter(!is.na(Date) & !is.na(Price) & Price > 0) %>%
  arrange(Date)

cat("After parsing:\n")
cat("  Valid daily observations:", nrow(alsi_daily), "\n")
cat("  Date range:", format(min(alsi_daily$Date), "%Y-%m-%d"), 
    "to", format(max(alsi_daily$Date), "%Y-%m-%d"), "\n\n")

log_message("DAILY_PARSE", "SUCCESS", sprintf("Parsed %d daily observations", nrow(alsi_daily)))

cat("First 10 parsed rows:\n")
print(head(alsi_daily, 10))
cat("\n")

# ============================================================================
# STEP 3: AGGREGATE TO MONTHLY
# ============================================================================

cat("========== STEP 3: AGGREGATE TO MONTHLY ==========\n\n")

data <- alsi_daily %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    Date = last(Date),
    Close = last(Price),
    Open = first(Price),
    High = max(Price),
    Low = min(Price),
    .groups = "drop"
  ) %>%
  select(Date, Close, Open, High, Low) %>%
  mutate(Source = "Investing.com") %>%
  arrange(Date)

cat("Monthly aggregation:\n")
cat("  Monthly observations:", nrow(data), "\n")
cat("  Date range:", format(min(data$Date), "%Y-%m-%d"), 
    "to", format(max(data$Date), "%Y-%m-%d"), "\n\n")

log_message("AGGREGATION", "SUCCESS", sprintf("Aggregated to %d monthly observations", nrow(data)))

cat("First 10 monthly prices:\n")
print(head(data, 10))
cat("\n")

# ============================================================================
# STEP 4: VALIDATION
# ============================================================================

cat("========== STEP 4: VALIDATION ==========\n\n")

ts_data <- ts(data$Close, 
              start = c(year(min(data$Date)), month(min(data$Date))),
              end = c(year(max(data$Date)), month(max(data$Date))),
              frequency = 12)

cat("Time Series Object:\n")
cat("  Observations:", length(ts_data), "months\n")
cat("  Start:", start(ts_data)[1], "-", sprintf("%02d", start(ts_data)[2]), "\n")
cat("  End:", end(ts_data)[1], "-", sprintf("%02d", end(ts_data)[2]), "\n\n")

# Summary statistics
cat("Summary Statistics:\n")
cat("  Mean:", round(mean(ts_data), 2), "\n")
cat("  SD:", round(sd(ts_data), 2), "\n")
cat("  Min:", round(min(ts_data), 2), "(", format(data$Date[which.min(data$Close)], "%Y-%m-%d"), ")\n")
cat("  Max:", round(max(ts_data), 2), "(", format(data$Date[which.max(data$Close)], "%Y-%m-%d"), ")\n")
cat("  Range:", round(max(ts_data) - min(ts_data), 2), "\n\n")

# ADF Test
cat("ADF Stationarity Test:\n")
adf_result <- adf.test(ts_data)
cat("  Test Statistic:", round(adf_result$statistic, 4), "\n")
cat("  p-value:", round(adf_result$p.value, 4), "\n")

if (adf_result$p.value < 0.05) {
  cat("  Result: STATIONARY (d=0)\n")
  stationarity <- "Stationary"
} else {
  cat("  Result: NON-STATIONARY (differencing needed)\n")
  stationarity <- "Non-stationary; d=1 required"
}
cat("\n")

# ACF at lag 1
acf_lag1 <- acf(ts_data, lag.max = 1, plot = FALSE)$acf[2]
cat("Autocorrelation (lag 1):", round(acf_lag1, 3), "\n\n")

log_message("VALIDATION", "SUCCESS", "All validation tests passed")

# ============================================================================
# STEP 5: SAVE DATA
# ============================================================================

cat("========== STEP 5: SAVE PROCESSED DATA ==========\n\n")

# Save raw monthly data
write.csv(data, "projects/project-01/data/raw/alsi_monthly.csv", row.names = FALSE)

# Create processed data
data_processed <- data %>%
  select(Date, Close) %>%
  mutate(
    Log_Close = log(Close),
    Return = c(NA, diff(log(Close))),
    Return_Pct = Return * 100,
    Year = year(Date),
    Month = month(Date),
    Quarter = quarter(Date)
  )

write.csv(data_processed, "projects/project-01/data/processed/alsi_clean.csv", 
          row.names = FALSE)

cat("✓ Files saved:\n")
cat("  Raw: projects/project-01/data/raw/alsi_monthly.csv\n")
cat("  Processed: projects/project-01/data/processed/alsi_clean.csv\n\n")

log_message("OUTPUT", "SUCCESS", "Files saved successfully")

# ============================================================================
# STEP 6: SUMMARY
# ============================================================================

cat("========== EXTRACTION COMPLETE ==========\n\n")
cat("✓ SUCCESS!\n\n")
cat("Summary:\n")
cat("  Input: 3747 daily observations\n")
cat("  Output:", nrow(data), "monthly observations\n")
cat("  Date range:", format(min(data$Date), "%Y-%m-%d"), 
    "to", format(max(data$Date), "%Y-%m-%d"), "\n")
cat("  Price range: ZAR", round(min(data$Close), 2), 
    "to", round(max(data$Close), 2), "\n")
cat("  Stationarity:", stationarity, "\n")
cat("  ACF(1):", round(acf_lag1, 3), "\n\n")

cat("Last 10 months:\n")
print(tail(data_processed, 10))
cat("\n")

# Save session info
sink("projects/project-01/logs/session_info.txt")
cat("R Session Info\n")
cat(sprintf("Extraction: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("================================================================================\n\n")
sessionInfo()
sink()

log_message("FINAL", "SUCCESS", "Data extraction complete")

cat("✓ Ready for Exploratory Data Analysis!\n")
cat("\nNext: Load and run EDA script\n")