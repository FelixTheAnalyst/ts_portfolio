library(tidyverse)
library(lubridate)

# 1. Read the Eskom file; first column is the hourly timestamp
raw <- read_csv("projects/project-02/data/ESK6816.csv", show_col_types = FALSE)
names(raw)[1] <- "stamp"

# 2. Hourly EAF (%) = 100 * (1 - total losses / installed capacity), all in MW
hourly <- raw |>
  transmute(
    stamp = parse_date_time(stamp, "Y/m/d H:M"),
    loss  = `Total PCLF` + `Total UCLF` + `Total OCLF`,
    eaf   = 100 * (1 - loss / `Installed Eskom Capacity`)
  ) |>
  filter(!is.na(eaf))                       # drops the 4,800 NA hours (Sep 2023 onward)

# 3. Daily mean EAF
daily <- hourly |>
  mutate(date = as_date(stamp)) |>
  group_by(date) |>
  summarise(eaf = mean(eaf), n_hours = n(), .groups = "drop")

# 4. Keep only complete weeks (Mon-Sun, 7 full days) so no partial week enters
weekly <- daily |>
  filter(n_hours == 24) |>                  # only complete days
  mutate(date = floor_date(date, "week", week_start = 1)) |>
  group_by(date) |>
  summarise(eaf = mean(eaf), n_days = n(), .groups = "drop") |>
  filter(n_days == 7) |>                    # only complete weeks
  select(date, eaf)

# 5. Checks: 232 weeks, regular 7-day spacing, sensible range
stopifnot(nrow(weekly) == 232, all(diff(weekly$date) == 7))
summary(weekly$eaf)

# 6. Save for the analysis
write_csv(weekly, "projects/project-02/data/eaf.csv")


library(tidyverse)
library(quantmod)

# Try the All Share first; the Top 40 is only a fallback and would need a text change
tickers <- c("^J203.JO", "^J200.JO")

pull <- function(tk) {
  tryCatch({
    z <- quantmod::getSymbols(tk, src = "yahoo",
                              from = "2019-03-01", to = "2023-10-02",   # start in March so week 1 has a return
                              auto.assign = FALSE)
    tibble(date = zoo::index(z), close = as.numeric(quantmod::Cl(z))) |> drop_na()
  }, error = function(e) NULL)                                           # NULL means this ticker failed
}

jse <- NULL
for (tk in tickers) {
  jse <- pull(tk)
  if (!is.null(jse) && nrow(jse) > 1000) { message("Using ticker: ", tk); break }
}
stopifnot(!is.null(jse))                                                 # stop if neither ticker worked

# Checks: coverage must reach the first and last week of the window
print(range(jse$date)); print(nrow(jse))                                 # expect about 1,150 rows
stopifnot(min(jse$date) <= as.Date("2019-03-08"), max(jse$date) >= as.Date("2023-09-08"))

dir.create("data", showWarnings = FALSE)
write_csv(jse, "projects/project-02/data/jse_alsi.csv")                                      # frozen copy used by analysis.qmd
