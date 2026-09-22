# =============================================================================
# The Hidden Cost of Higher Mortgage Rates
# Complete reproducible R pipeline for Blog Post 2
# Author: Scarlett
# Date: 2026-09-21
# =============================================================================

# This script reproduces the data collection, cleaning, calculations, and
# figures used in index.qmd. Run it from the root of the GitHub repository.


# 1. Install and load packages -------------------------------------------------

required_packages <- c(
  "rvest", "xml2", "tidyverse", "lubridate", "janitor",
  "scales", "httr2"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

invisible(
  lapply(required_packages, library, character.only = TRUE)
)

options(scipen = 999, dplyr.summarise.inform = FALSE)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = element_blank()
    )
)


# 2. Create output folders -----------------------------------------------------

# The script is designed to be run from the repository root, next to index.qmd.
project_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

files_dir <- file.path(project_dir, "files")
raw_dir <- file.path(files_dir, "raw")
processed_dir <- file.path(files_dir, "processed")
figure_dir <- files_dir

purrr::walk(
  c(files_dir, raw_dir, processed_dir),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. Set source URLs -----------------------------------------------------------

source_urls <- c(
  fomc = paste0(
    "https://www.federalreserve.gov/newsevents/pressreleases/",
    "monetary20260916a.htm"
  ),
  h15 = "https://www.federalreserve.gov/releases/h15/",
  pmms = "https://www.freddiemac.com/pmms/archive",
  census = "https://www.census.gov/construction/nrs/current/index.html"
)

# FALSE reproduces the analysis from existing dated HTML snapshots.
# Set to TRUE only when intentionally refreshing all webpages.
refresh_data <- FALSE


# 4-5. Scrape sources and save raw HTML ----------------------------------------

read_cached_page <- function(url, cache_file, refresh = refresh_data) {
  cache_path <- file.path(raw_dir, cache_file)
  
  if (refresh || !file.exists(cache_path)) {
    Sys.sleep(1)
    
    response <- httr2::request(url) |>
      httr2::req_user_agent(
        "Scarlett graduate academic project; non-commercial research"
      ) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_timeout(seconds = 30) |>
      httr2::req_perform()
    
    if (httr2::resp_status(response) >= 400) {
      stop(
        "Download failed for ", url,
        " (HTTP status ", httr2::resp_status(response), ").",
        call. = FALSE
      )
    }
    
    page <- response |>
      httr2::resp_body_string() |>
      xml2::read_html()
    
    xml2::write_html(page, cache_path)
  }
  
  xml2::read_html(cache_path)
}

fomc_page <- read_cached_page(
  source_urls[["fomc"]],
  "fomc_2026-09-16.html"
)

h15_page <- read_cached_page(
  source_urls[["h15"]],
  "h15_2026-09-21.html"
)

pmms_page <- read_cached_page(
  source_urls[["pmms"]],
  "pmms_2026-09-21.html"
)

census_page <- read_cached_page(
  source_urls[["census"]],
  "census_nrs_2026-09-21.html"
)


# 6. Clean, transform, and validate data ---------------------------------------

percent_to_decimal <- function(x) {
  readr::parse_number(as.character(x)) / 100
}

# FOMC statement
fomc_text <- fomc_page |>
  html_elements("#article p") |>
  html_text2() |>
  str_squish() |>
  str_c(collapse = " ")

if (!str_detect(fomc_text, "3-3/4 to 4 percent")) {
  stop("The expected target range was not found in the FOMC statement.")
}

# Federal Reserve H.15 table
h15 <- h15_page |>
  html_element("table") |>
  html_table(fill = TRUE) |>
  clean_names()

h15_long <- h15 |>
  pivot_longer(
    -instruments,
    names_to = "date_label",
    values_to = "rate_text"
  ) |>
  mutate(
    date_text = date_label |>
      str_remove("^x") |>
      str_replace_all("_", " "),
    date = parse_date_time(
      date_text,
      orders = "Y b d",
      locale = "C",
      quiet = TRUE
    ) |>
      as_date(),
    rate = parse_number(as.character(rate_text)) / 100
  )

policy_snapshot <- h15_long |>
  filter(
    date %in% ymd(c("2026-09-16", "2026-09-17")),
    str_detect(
      instruments,
      "Federal funds \\(effective\\)|Bank prime loan"
    )
  ) |>
  mutate(
    series = if_else(
      str_detect(instruments, "Federal funds"),
      "Effective federal funds rate",
      "Bank prime rate"
    )
  ) |>
  select(date, series, rate) |>
  arrange(series, date)

if (nrow(policy_snapshot) != 4 || anyNA(policy_snapshot$rate)) {
  stop(
    paste0(
      "The four required H.15 observations were not recovered. ",
      "Use the dated h15_2026-09-21.html snapshot."
    )
  )
}

# Freddie Mac PMMS archive
pmms_dates <- pmms_page |>
  html_elements("p.lead strong") |>
  html_text2() |>
  str_squish() |>
  mdy(locale = "C", quiet = TRUE)

pmms_tables <- pmms_page |>
  html_elements("table.hover") |>
  html_table(fill = TRUE)

if (
  length(pmms_dates) == 0 ||
  length(pmms_dates) != length(pmms_tables) ||
  anyNA(pmms_dates)
) {
  stop("Freddie Mac dates and tables could not be matched reliably.")
}

pmms <- map2_dfr(pmms_dates, pmms_tables, function(obs_date, tab) {
  clean_tab <- clean_names(tab)
  
  if (!all(c("x30_yr_frm", "x15_yr_frm") %in% names(clean_tab))) {
    stop("Expected PMMS mortgage-rate columns were not found.")
  }
  
  clean_tab |>
    transmute(
      date = as_date(obs_date),
      mortgage_30y = percent_to_decimal(x30_yr_frm),
      mortgage_15y = percent_to_decimal(x15_yr_frm)
    )
}) |>
  filter(is.finite(mortgage_30y), is.finite(mortgage_15y)) |>
  distinct(date, .keep_all = TRUE) |>
  arrange(date)

analysis_rates <- pmms |>
  filter(date %in% ymd(c("2026-09-10", "2026-09-17"))) |>
  arrange(date)

if (nrow(analysis_rates) != 2 || anyNA(analysis_rates$mortgage_30y)) {
  stop(
    paste0(
      "The September 10 and September 17 PMMS observations were not found. ",
      "Use the dated pmms_2026-09-21.html snapshot."
    )
  )
}

# Census New Residential Sales page
census_text <- census_page |>
  html_element("body") |>
  html_text2() |>
  str_squish()

median_price_match <- str_match(
  census_text,
  regex(
    "median sales price[^$]{0,250}\\$([0-9,]+)",
    ignore_case = TRUE
  )
)

median_price <- parse_number(median_price_match[1, 2])

if (
  !is.finite(median_price) ||
  median_price <= 0 ||
  !str_detect(census_text, regex("July 2026", ignore_case = TRUE))
) {
  stop("The July 2026 Census median sales price was not extracted reliably.")
}

census_snapshot <- tibble(
  snapshot_date = ymd("2026-09-21"),
  reference_month = ymd("2026-07-01"),
  median_new_home_price = median_price
)


# 7. Save cleaned CSV files ----------------------------------------------------

write_csv(pmms, file.path(processed_dir, "pmms_clean.csv"))
write_csv(policy_snapshot, file.path(processed_dir, "policy_snapshot.csv"))
write_csv(census_snapshot, file.path(processed_dir, "census_price_snapshot.csv"))


# 8. Define fixed-rate mortgage formulas --------------------------------------

mortgage_payment <- function(principal, annual_rate, years = 30) {
  monthly_rate <- annual_rate / 12
  n_payments <- years * 12
  
  principal * monthly_rate * (1 + monthly_rate)^n_payments /
    ((1 + monthly_rate)^n_payments - 1)
}

max_loan <- function(monthly_budget, annual_rate, years = 30) {
  monthly_rate <- annual_rate / 12
  n_payments <- years * 12
  
  monthly_budget * (1 - (1 + monthly_rate)^(-n_payments)) /
    monthly_rate
}


# 9. Calculate monthly mortgage payments --------------------------------------

rate_previous_week <- analysis_rates$mortgage_30y[[1]]
rate_fomc_week <- analysis_rates$mortgage_30y[[2]]
rate_change_bp <- (rate_fomc_week - rate_previous_week) * 10000

effr_before <- policy_snapshot |>
  filter(
    series == "Effective federal funds rate",
    date == ymd("2026-09-16")
  ) |>
  pull(rate)

effr_after <- policy_snapshot |>
  filter(
    series == "Effective federal funds rate",
    date == ymd("2026-09-17")
  ) |>
  pull(rate)

prime_before <- policy_snapshot |>
  filter(series == "Bank prime rate", date == ymd("2026-09-16")) |>
  pull(rate)

prime_after <- policy_snapshot |>
  filter(series == "Bank prime rate", date == ymd("2026-09-17")) |>
  pull(rate)

median_loan <- 0.80 * median_price
payment_previous_week <- mortgage_payment(median_loan, rate_previous_week)
payment_fomc_week <- mortgage_payment(median_loan, rate_fomc_week)
monthly_payment_change <- payment_fomc_week - payment_previous_week
annual_payment_change <- 12 * monthly_payment_change
payment_change_pct <- payment_fomc_week / payment_previous_week - 1

previous_week_label <- paste0(
  "Previous week: ", percent(rate_previous_week, accuracy = 0.01)
)

fomc_week_label <- paste0(
  "FOMC week: ", percent(rate_fomc_week, accuracy = 0.01)
)

result_summary <- tibble(
  measure = c(
    "Effective federal-funds rate",
    "Bank prime rate",
    "30-year fixed mortgage rate"
  ),
  earlier_observation = c(effr_before, prime_before, rate_previous_week),
  later_observation = c(effr_after, prime_after, rate_fomc_week),
  change_basis_points = 10000 *
    (later_observation - earlier_observation),
  timing = c(
    "Sep. 16 to Sep. 17 (daily)",
    "Sep. 16 to Sep. 17 (daily)",
    "Sep. 10 PMMS to Sep. 17 PMMS"
  )
)

payment_scenario <- tibble(
  median_new_home_price = median_price,
  assumed_down_payment_rate = 0.20,
  mortgage_principal = median_loan,
  previous_week_rate = rate_previous_week,
  fomc_week_rate = rate_fomc_week,
  rate_change_basis_points = rate_change_bp,
  previous_week_monthly_payment = payment_previous_week,
  fomc_week_monthly_payment = payment_fomc_week,
  monthly_payment_change = monthly_payment_change,
  annual_payment_change = annual_payment_change,
  payment_change_percent = payment_change_pct
)


# 10. Calculate homebuying power ----------------------------------------------

period_levels <- c(previous_week_label, fomc_week_label)

budget_results <- crossing(
  monthly_budget = c(2000, 2500, 3000),
  period = period_levels
) |>
  mutate(
    period = factor(period, levels = period_levels),
    rate = if_else(
      period == previous_week_label,
      rate_previous_week,
      rate_fomc_week
    ),
    maximum_loan = max_loan(monthly_budget, rate),
    max_home_price = maximum_loan / 0.80
  )

budget_summary <- budget_results |>
  select(monthly_budget, period, max_home_price) |>
  pivot_wider(names_from = period, values_from = max_home_price) |>
  mutate(
    dollar_loss = .data[[fomc_week_label]] -
      .data[[previous_week_label]],
    percent_loss = .data[[fomc_week_label]] /
      .data[[previous_week_label]] - 1
  )

stress_test <- tibble(
  scenario = c(
    "Previous-week PMMS rate",
    "FOMC-week PMMS rate",
    "FOMC-week rate + 50 bp",
    "FOMC-week rate + 100 bp"
  ),
  mortgage_rate = c(
    rate_previous_week,
    rate_fomc_week,
    rate_fomc_week + 0.005,
    rate_fomc_week + 0.010
  )
) |>
  mutate(
    maximum_loan = max_loan(2500, mortgage_rate),
    maximum_home_price = maximum_loan / 0.80
  )

# Save all derived numerical outputs.
write_csv(result_summary, file.path(processed_dir, "rate_summary.csv"))
write_csv(payment_scenario, file.path(processed_dir, "payment_scenario.csv"))
write_csv(budget_results, file.path(processed_dir, "budget_results.csv"))
write_csv(budget_summary, file.path(processed_dir, "budget_summary.csv"))
write_csv(stress_test, file.path(processed_dir, "stress_test.csv"))


# 11. Draw and save the three figures -----------------------------------------

# Figure 1: Weekly 30-year fixed mortgage rate
english_month_breaks <- ymd(sprintf("2026-%02d-01", 1:9))

rate_label_data <- analysis_rates |>
  mutate(
    label_x = date + c(-3, 0),
    label_y = mortgage_30y + c(0.00028, 0.00042)
  )

p1 <- pmms |>
  filter(date >= ymd("2026-01-01"), date <= ymd("2026-09-17")) |>
  ggplot(aes(date, mortgage_30y)) +
  geom_line(linewidth = 0.9, color = "#24496e") +
  geom_vline(
    xintercept = ymd("2026-09-16"),
    linetype = "dashed",
    color = "#b24c3d",
    linewidth = 0.7
  ) +
  geom_point(data = analysis_rates, size = 3.2, color = "#b24c3d") +
  geom_text(
    data = rate_label_data,
    aes(
      x = label_x,
      y = label_y,
      label = percent(mortgage_30y, accuracy = 0.01)
    ),
    inherit.aes = FALSE,
    color = "#8f382d",
    size = 3.6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0.03, 0.13))
  ) +
  scale_x_date(
    breaks = english_month_breaks,
    labels = month.abb[1:9],
    limits = ymd(c("2026-01-01", "2026-09-25")),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = paste0(
      "The FOMC-week PMMS rate was ",
      percent(rate_fomc_week, accuracy = 0.01)
    ),
    subtitle = "Weekly national average, January-September 2026",
    x = NULL,
    y = "30-year fixed mortgage rate"
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 12, r = 22, b = 8, l = 8))

print(p1)

ggsave(
  file.path(figure_dir, "mortgage-rate-trend.png"),
  p1,
  width = 8,
  height = 5,
  dpi = 300
)

# Figure 2: Monthly payment curves
loan_values <- c(300000, 400000, 500000)

payment_grid <- crossing(
  mortgage_rate = seq(0.04, 0.08, by = 0.0005),
  loan = loan_values
) |>
  mutate(monthly_payment = mortgage_payment(loan, mortgage_rate))

payment_label_y <- max(payment_grid$monthly_payment) * 1.025

p2 <- ggplot(
  payment_grid,
  aes(mortgage_rate, monthly_payment, color = factor(loan))
) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = rate_previous_week,
    linetype = "dotted",
    linewidth = 0.8,
    color = "#555555"
  ) +
  geom_vline(
    xintercept = rate_fomc_week,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#b24c3d"
  ) +
  annotate(
    "label",
    x = rate_previous_week,
    y = payment_label_y,
    label = previous_week_label,
    angle = 0,
    hjust = 1.08,
    vjust = 0.5,
    size = 3.1,
    color = "#444444",
    fill = "white",
    label.size = 0
  ) +
  annotate(
    "label",
    x = rate_fomc_week,
    y = payment_label_y,
    label = fomc_week_label,
    angle = 0,
    hjust = -0.08,
    vjust = 0.5,
    size = 3.1,
    color = "#8f382d",
    fill = "white",
    label.size = 0
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_y_continuous(
    labels = dollar_format(),
    expand = expansion(mult = c(0.03, 0.14))
  ) +
  scale_color_manual(
    values = c("#3f7d5c", "#c17b31", "#8d4773"),
    breaks = as.character(loan_values),
    labels = dollar(loan_values)
  ) +
  labs(
    title = "Small rate differences affect every scheduled payment",
    subtitle = "Monthly principal and interest by loan size",
    x = "Mortgage rate",
    y = "Monthly payment",
    color = "Loan amount"
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 12, r = 12, b = 8, l = 8))

print(p2)

ggsave(
  file.path(figure_dir, "payment-curves.png"),
  p2,
  width = 8,
  height = 5,
  dpi = 300
)

# Figure 3: Homebuying power comparison
p3 <- ggplot(
  budget_results,
  aes(factor(monthly_budget), max_home_price, fill = period)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = dollar(max_home_price, accuracy = 1000)),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3.6
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_manual(
    values = c("#7396b8", "#c65d45"),
    breaks = period_levels
  ) +
  labs(
    title = "A higher rate reduces the price supported by a fixed P&I budget",
    subtitle = "Previous-week and FOMC-week PMMS rate scenarios",
    x = "Monthly principal-and-interest budget",
    y = "Home price supported by assumed P&I budget",
    fill = NULL
  ) +
  theme(legend.position = "top")

print(p3)

ggsave(
  file.path(figure_dir, "buying-power.png"),
  p3,
  width = 8,
  height = 5,
  dpi = 300
)


# 12. Print reproducibility information ---------------------------------------

message("Analysis complete.")
message("Raw HTML: ", raw_dir)
message("Processed CSV files: ", processed_dir)
message("Figures: ", figure_dir)

sessionInfo()