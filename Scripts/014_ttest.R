source("scripts/00_setup.R")
faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")
admin_salaries   <- read_csv("data/admin_salaries_filtered.csv")
cpi_annual       <- read_csv("data/cpi_deflator.csv")

# institution-level faculty average salary (across all ranks)
faculty_inst <- faculty_salaries %>%
  filter(academic_rank %in% c(1, 2, 3), sex == 99, salary_outlays >= 0) %>%
  group_by(year, unitid, academic_rank_label) %>%
  slice_max(instruc_staff_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(year, unitid) %>%
  summarise(avg_salary = mean(real_salary_2024, na.rm = TRUE), .groups = "drop")

# institution-level admin average salary
admin_inst <- admin_salaries %>%
  filter(staff_category %in% c(5, 6), noninstruc_staff_count > 0) %>%
  group_by(year, unitid) %>%
  summarise(
    total_outlays = sum(salary_outlays, na.rm = TRUE),
    total_staff   = sum(noninstruc_staff_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(cpi_annual %>% select(year, deflator_to_2024), by = "year") %>%
  mutate(avg_salary = (total_outlays / total_staff) * deflator_to_2024) %>%
  select(year, unitid, avg_salary)

# percent change across the COVID shock window (2019 -> 2021) per institution
covid_change <- function(df) {
  df %>%
    filter(year %in% c(2019, 2021)) %>%
    pivot_wider(names_from = year, values_from = avg_salary, names_prefix = "y") %>%
    filter(!is.na(y2019), !is.na(y2021)) %>%
    mutate(pct_change = (y2021 - y2019) / y2019 * 100)
}

faculty_change <- covid_change(faculty_inst) %>% select(unitid, faculty_pct_change = pct_change)
admin_change   <- covid_change(admin_inst)   %>% select(unitid, admin_pct_change   = pct_change)

paired_data <- inner_join(faculty_change, admin_change, by = "unitid")
nrow(paired_data)  # sanity check — how many colleges have both?

t.test(paired_data$faculty_pct_change, paired_data$admin_pct_change, paired = TRUE)