source("scripts/00_setup.R")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
library(httr)
library(jsonlite)

# BLS limits requests without a free API key to 10 years per query,
# so pull in two chunks and combine them.
pull_cpi_chunk <- function(start_yr, end_yr) {
  resp <- POST(
    url = "https://api.bls.gov/publicAPI/v2/timeseries/data/",
    body = list(
      seriesid  = list("CUUR0000SA0"),
      startyear = as.character(start_yr),
      endyear   = as.character(end_yr)
    ),
    encode = "json",
    config(http_version = 2)   # avoids the HTTP/2 framing bug
  )
  json <- content(resp, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
  json$Results$series$data[[1]] %>%
    as_tibble() %>%
    mutate(year = as.integer(year), value = as.numeric(value)) %>%
    filter(period != "M13")   # drop any pre-built annual-average rows
}

cpi_monthly <- bind_rows(
  pull_cpi_chunk(2005, 2014),
  pull_cpi_chunk(2015, 2024)
)

# average the 12 months into one annual CPI value per year
cpi_annual <- cpi_monthly %>%
  group_by(year) %>%
  summarise(cpi = mean(value), .groups = "drop")

nrow(cpi_annual)    # sanity check - should be 20 (2005-2024)
print(cpi_annual)   # sanity check - CPI should rise steadily year to year

# build a deflator that converts any year's nominal dollars into 2024 dollars
base_cpi <- cpi_annual %>% filter(year == 2024) %>% pull(cpi)

cpi_annual <- cpi_annual %>%
  mutate(deflator_to_2024 = base_cpi / cpi)

write_csv(cpi_annual, "data/cpi_deflator.csv")

# --- apply to faculty salaries -----------------------------------------
faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")

faculty_salaries <- faculty_salaries %>%
  left_join(cpi_annual %>% select(year, deflator_to_2024), by = "year") %>%
  mutate(real_salary_2024 = average_salary * deflator_to_2024)

write_csv(faculty_salaries, "data/faculty_salaries_filtered.csv")

# --- apply to admin salaries ---------------------------------------------
admin_salaries <- read_csv("data/admin_salaries_filtered.csv")

admin_salaries <- admin_salaries %>%
  left_join(cpi_annual %>% select(year, deflator_to_2024), by = "year") %>%
  mutate(real_avg_salary_2024 = avg_salary * deflator_to_2024)

write_csv(admin_salaries, "data/admin_salaries_filtered.csv")