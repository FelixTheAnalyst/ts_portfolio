library(tidyverse)
library(quantmod)

# One name per series, matching the table above
tick <- c(capitec = "CPI.JO", sbk = "SBK.JO", fsr = "FSR.JO",
          abg = "ABG.JO", ned = "NED.JO", alsi = "^J203.JO")

# Pull one ticker from Yahoo; return NULL if it fails
pull <- function(tk) {
  z <- tryCatch(getSymbols(tk, src = "yahoo", from = "2014-12-01", to = "2026-10-31",
                           auto.assign = FALSE), error = function(e) NULL)
  if (is.null(z)) return(NULL)
  tibble(date = zoo::index(z), close = as.numeric(Cl(z)), adj = as.numeric(Ad(z))) |> drop_na()
}

prices <- imap_dfr(tick, \(tk, nm) {
  d <- pull(tk)
  if (is.null(d)) { warning("FAILED: ", tk); return(NULL) }
  mutate(d, series = nm)
})

# Check: every series should start by early Jan 2015 and reach late Oct 2026
prices |> group_by(series) |>
  summarise(first = min(date), last = max(date), n = n()) |> print()
print(setdiff(names(tick), unique(prices$series)))      # any series that failed to download

write_csv(prices, "projects/project-04/data/prices.csv")                    # frozen copy used by analysis.qmd