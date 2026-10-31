# Run ONCE by hand from projects/project-03/. Freezes the only real series and the published anchors.
library(tidyverse)
library(gtrendsR)

# ---- Google Trends: weekly search interest for "two pot" in South Africa ----
g <- gtrendsR::gtrends(keyword = "two pot", geo = "ZA",
                       time = "2024-06-01 2026-10-25", gprop = "web")   # weekly resolution
tr <- g$interest_over_time |>
  transmute(date = as.Date(date) + 1,                # Trends weeks start Sunday; shift to that week's Monday
            hits = as.numeric(ifelse(hits == "<1", 0.5, hits)),   # "<1" is reported as text
            retrieved = Sys.Date())                  # Trends is sampled, so record the pull date
write_csv(tr, "projects/project-03/data/trends_two_pot.csv")

# ---- Published anchors used to calibrate the simulation (check each source before submitting) ----
anchors <- tribble(
  ~date,        ~quantity,                                      ~value,   ~source,
  "2024-09-11", "Applications per day, 1 to ~11 Sep 2024",      17964,    "TimesLive, quoting SARS: https://www.timeslive.co.za/news/south-africa/2024-09-11-two-pot-system-sars-receives-17964-savings-withdrawal-applications-a-day-since-september-1/",
  "2025-01-31", "Cumulative applications received",             2664279,  "SARS media release: https://www.sars.gov.za/media-release/more-than-2-million-taxpayers-withdraw-from-their-savings-pot/",
  "2025-01-31", "Cumulative tax directives approved",           2403379,  "SARS media release (same)",
  "2026-03-08", "Claims in first week of March 2026 (one administrator)", 140000, "Alexforbes, via Moonstone: https://www.moonstone.co.za/two-pot-withdrawals-surge-as-repeat-claims-reshape-savings-behaviour/",
  "2026-03-11", "Claims 1 to 11 March 2026 (one administrator)", 38148,   "Momentum, via Moonstone (same)")
write_csv(anchors, "projects/project-03/data/anchors.csv")

print(range(tr$date)); print(nrow(tr))              # expect about 20 Jun 2024 to Oct 2026, ~125 weeks