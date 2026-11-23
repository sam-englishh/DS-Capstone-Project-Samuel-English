source("scripts/00_setup.R")

# Each college's exact metro-area (or nonmetro-portion) Regional Price Parity series,
# matched at the county/metro level rather than the whole state. Pomona uses the LA
# metro series, not a California-wide average; Williams uses the Pittsfield, MA
# metro series, not a Massachusetts-wide average.
rpp_series_lookup <- tibble::tribble(
  ~unitid, ~inst_name,                  ~rpp_series,
  161004, "Bowdoin College",            "RPPALL38860",   # Portland-South Portland, ME
  211273, "Bryn Mawr College",          "RPPALL37980",   # Philadelphia-Camden-Wilmington
  173258, "Carleton College",           "MNNMPRPPALL",   # nonmetro MN
  112260, "Claremont McKenna College",  "RPPALL31080",   # Los Angeles-Long Beach-Anaheim
  198385, "Davidson College",           "RPPALL16740",   # Charlotte-Concord-Gastonia
  153384, "Grinnell College",           "IANMPRPPALL",   # nonmetro IA
  115409, "Harvey Mudd College",        "RPPALL31080",
  212911, "Haverford College",          "RPPALL37980",
  230959, "Middlebury College",         "VTNMPRPPALL",   # nonmetro VT
  204501, "Oberlin College",            "RPPALL17460",   # Cleveland-Elyria
  120254, "Occidental College",         "RPPALL31080",
  121257, "Pitzer College",             "RPPALL31080",
  121345, "Pomona College",             "RPPALL31080",
  123165, "Scripps College",            "RPPALL31080",
  167835, "Smith College",              "RPPALL44140",   # Springfield, MA
  216287, "Swarthmore College",         "RPPALL37980",
  168218, "Wellesley College",          "RPPALL14460",   # Boston-Cambridge-Newton
  130697, "Wesleyan University",        "RPPALL25540",   # Hartford-West Hartford-East Hartford
  168342, "Williams College",           "RPPALL38340"    # Pittsfield, MA
)

pull_rpp <- function(series_id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
  df <- read_csv(url, show_col_types = FALSE)
  df %>%
    rename(date = 1, rpp = 2) %>%
    mutate(year = lubridate::year(date), rpp_series = series_id) %>%
    select(rpp_series, year, rpp)
}

rpp_data <- unique(rpp_series_lookup$rpp_series) %>% map_dfr(pull_rpp)

# sanity check - one row per series per year, 2008 onward
rpp_data %>% count(rpp_series) %>% print(n = 20)
range(rpp_data$year)

rpp_final <- rpp_series_lookup %>% left_join(rpp_data, by = "rpp_series")
write_csv(rpp_final, "data/regional_price_parity.csv")

# --- apply to faculty and admin data -----------------------------------
faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")
admin_salaries   <- read_csv("data/admin_salaries_filtered.csv")

faculty_salaries <- faculty_salaries %>%
  left_join(rpp_final %>% select(unitid, year, rpp), by = c("unitid", "year")) %>%
  mutate(real_salary_2024_colAdj = real_salary_2024 / (rpp / 100))

admin_salaries <- admin_salaries %>%
  left_join(rpp_final %>% select(unitid, year, rpp), by = c("unitid", "year")) %>%
  mutate(real_avg_salary_2024_colAdj = real_avg_salary_2024 / (rpp / 100))

faculty_salaries %>% filter(is.na(rpp)) %>% count(year)  # NAs expected only pre-2008

write_csv(faculty_salaries, "data/faculty_salaries_filtered.csv")
write_csv(admin_salaries, "data/admin_salaries_filtered.csv")

rpp_final %>%
  filter(year == 2024) %>%
  select(inst_name, rpp_series, rpp) %>%
  arrange(rpp)