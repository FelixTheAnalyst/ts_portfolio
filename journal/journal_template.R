# journal_template.R - builds one template entry per day, 7 Sep to 26 Oct 2026

dir.create("journal/entries", recursive = TRUE, showWarnings = FALSE)
days <- seq(as.Date("2026-09-07"), as.Date("2026-10-26"), by = "day")  # 50 days

# Link targets (relative to journal/entries/) - edit if your filenames differ
lnk <- c(
  p1  = "../../projects/project-01/index.qmd",
  p2  = "../../projects/project-02/index.qmd",
  p3  = "../../projects/project-03/index.qmd",
  p4  = "../../projects/project-04/index.qmd",
  s1  = "../../section-1/section_1.qmd",
  s2  = "../../section-2/section_2.qmd",
  ai  = "../../appendix/ai_use_log.qmd",
  app = "../../appendix/code_and_data.qmd"
)

# Phase table: each phase starts on the date given and runs to the next start
phases <- data.frame(
  start = as.Date(c("2026-09-07", "2026-09-11", "2026-09-21", "2026-09-27",
                    "2026-10-05", "2026-10-12", "2026-10-19")),
  phase = c("Part 1 consolidation", "Exponential smoothing",
            "Regression with ARMA errors", "State-space models",
            "Kalman filter", "Hidden Markov models (1)",
            "Hidden Markov models (2) and assembly"),
  course = c(
    "Part 1: stationarity, operators, ACF/PACF, ARMA/ARIMA/SARIMA, benchmarks and accuracy",
    "Part 2 released 11 Sep: simple, Holt and Holt-Winters smoothing, ETS",
    "Part 2 released 21 Sep: GLS, ARMA errors, transfer functions, VAR",
    "Part 2 released 27-28 Sep: local level and trend models, state-space form",
    "Kalman filter and smoother, likelihood by prediction errors, dlm practical",
    "Part 2 released 12 Oct: HMM definition, forward-backward, Baum-Welch",
    "Part 2 released 19 Oct: Viterbi decoding, state selection, applications"),
  stage = c(
    "Project 1 scoping, data checks, exploratory plots, baseline models",
    "Project 1 model comparison and forecast evaluation; Project 2 data (weekly EAF)",
    "Project 2 spectral and coherence analysis; Section 1 (ARIMA, Whittle, GPH)",
    "Project 3 data decision and design; finish Section 1",
    "Project 3 negative-binomial GLM (IRLS); Project 4 data pull and checks",
    "Project 4 state-space regression; Section 2 (ETS, harmonic regression)",
    "Project 4 write-up; Section 2 HMM; appendix, reflection, AI log"),
  links = c("p1", "p1,p2", "p2,s1", "p3,s1", "p3,p4", "p4,s2", "p4,s2,app,ai"),
  stringsAsFactors = FALSE
)

# Weekday prompt (ISO 1 = Monday ... 7 = Sunday)
focus <- c(
  "Plan the week: what must exist by Sunday, and what is the riskiest part?",
  "Build: code or data work. What ran, and what did the output show?",
  "Theory: which lecture idea or derivation did I work through, and where did it connect to my data?",
  "Debug and validate: what broke, how did I check the fix, and what did I rule out?",
  "Write-up: what did I explain in words, and did the text match the numbers?",
  "Consolidate: what do I now understand that I did not at the start of the week?",
  "Reflect or rest: how has my approach changed since last Sunday?")

for (i in seq_along(days)) {
  d  <- days[i]
  f  <- file.path("journal/entries", paste0(format(d), ".qmd"))
  if (file.exists(f)) next                      # never overwrite real entries
  p  <- phases[findInterval(d, phases$start), ] # phase active on this date
  ks <- strsplit(p$links, ",")[[1]]
  links <- paste0("- [", toupper(ks), "](", lnk[ks], ")", collapse = "\n")
  wd <- as.integer(format(d, "%u"))
  
  txt <- paste0(
    '---
title: "', format(d, "%a %d %b %Y"), '"
date: ', format(d), '
categories: ["', p$phase, '"]
---

::: {.callout-warning}
## Template entry
Replace every prompt with your own record, or delete the entry if you did no work that day. Do not keep wording you cannot stand behind.
:::

**Session / topic:** ', p$course, '

**Project stage:** ', p$stage, '

**Focus for today:** ', focus[wd], '

## What I did
*[Tasks, code written, data handled, files touched, time spent.]*

## What I learned
*[A concept, result or method now understood. Say how your view changed compared with an earlier entry.]*

## Difficulties and open questions
*[Errors, confusing results, assumptions you doubt, questions for the lecturers or tutorial.]*

## What I will do next
*[Concrete next step for tomorrow.]*

## AI assistance
*[None / what you asked, what you used, what you checked. Mirror this in the AI Use Log.]*

## Links
', links, '
')
  writeLines(txt, f)
}

# Index page with a sortable, filterable table of entries
if (!file.exists("journal/index.qmd")) writeLines(
  '---
title: "Evolution of learning"
page-layout: full
listing:
  contents: entries
  type: table
  sort: "date"
  fields: [date, title, categories]
  filter-ui: true
  sort-ui: false
---

A dated record of how my understanding and the four projects developed from 7 September to 26 October 2026. Each entry covers what I did, what I learned, what was difficult and what came next.

*[Add two or three sentences here on the main arc of your learning once the entries are filled in.]*
', "journal/index.qmd")

message("Created ", length(list.files("journal/entries")), " entries")