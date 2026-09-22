# Blog2

The Hidden Cost of Higher Mortgage Rates

This repository contains a reproducible Quarto and R analysis of mortgage rates and household homebuying power around the September 2026 Federal Open Market Committee meeting.

The project was created by Scarlett for Blog Post 2 in Computational Methods for Economists.

Research question

How did mortgage financing conditions differ between the week before and the week of the September 2026 FOMC meeting, and what would that difference imply for the home price supported by a fixed monthly principal-and-interest budget?

The analysis distinguishes two separate facts:

The September 16 policy decision mechanically raised short-term benchmark rates.

The September 17 Freddie Mac PMMS mortgage-rate observation was higher than the previous week's observation.

The second fact is not interpreted as a causal effect of the announcement. The September 17 PMMS observation largely summarizes applications submitted before the September 16 announcement.

Repository structure

.
├── index.qmd
├── README.md
└── files/
    ├── mortgage-rate-trend.png
    ├── payment-curves.png
    ├── buying-power.png
    ├── raw/
    │   ├── fomc_2026-09-16.html
    │   ├── h15_2026-09-21.html
    │   ├── pmms_2026-09-21.html
    │   └── census_nrs_2026-09-21.html
    └── processed/
        ├── pmms_clean.csv
        ├── policy_snapshot.csv
        └── census_price_snapshot.csv

The folders and generated files are created automatically when index.qmd is rendered. All figures are stored in the files/ directory beside the QMD file.

Data sources

Federal Reserve FOMC statement, September 16, 2026

Federal Reserve H.15 Selected Interest Rates

Freddie Mac Primary Mortgage Market Survey

Freddie Mac PMMS Archive

U.S. Census Bureau New Residential Sales

The Census scenario uses the July 2026 national median sales price of newly built single-family homes. It is an illustrative national scenario, not a measure of a typical resale home or a specific local market.

Software requirements

R 4.4 or later

RStudio, recommended

Quarto

Install the required R packages once in the RStudio Console:

install.packages(
  c(
    "rvest",
    "xml2",
    "tidyverse",
    "lubridate",
    "janitor",
    "scales",
    "httr2",
    "knitr"
  ),
  repos = "https://cloud.r-project.org",
  type = "binary"
)

Reproduce the analysis

Clone the repository and enter its directory:

git clone https://github.com/YOUR-USERNAME/YOUR-REPOSITORY.git
cd YOUR-REPOSITORY

Render the article from RStudio or run:

quarto render index.qmd

The first successful render will:

create the files/, files/raw/, and files/processed/ directories;

download each missing public webpage once;

save dated HTML snapshots;

clean and validate the scraped data;

write the processed CSV files;

generate the three figures; and

create index.html.

After the raw HTML snapshots have been created, leave this setting unchanged in index.qmd:

refresh_data <- FALSE

This ensures that future renders reproduce the September 2026 source vintage. Set it to TRUE only when intentionally refreshing all sources; newer webpage content may no longer contain the historical observations used in the article.

Scraping ethics

The scraper accesses only public, login-free webpages. It does not bypass authentication, CAPTCHAs, paywalls, or access restrictions. Requests use a descriptive academic user agent, a delay, retry limits, and local caching to minimize server load.

Analytical assumptions

The mortgage calculations assume:

a 30-year fixed-rate mortgage;

monthly amortization;

a down payment equal to 20% of the implied purchase price; and

principal and interest only.

Property taxes, homeowners insurance, HOA fees, maintenance, mortgage insurance, closing costs, borrower income, debt-to-income underwriting, and cash-on-hand constraints are not modeled.

Main outputs

files/mortgage-rate-trend.png: weekly 30-year mortgage rates and the FOMC meeting date;

files/payment-curves.png: monthly payment sensitivity across interest rates and loan balances;

files/buying-power.png: home price supported by fixed P&I budgets under the two PMMS rate scenarios.

Interpretation limit

This is a descriptive cash-flow analysis, not a causal monetary-policy study. A causal estimate would require higher-frequency market data and a strategy that isolates the unexpected component of the policy announcement.

License

This repository is intended for academic and educational use. Data remain subject to the terms and attribution requirements of their original providers.
