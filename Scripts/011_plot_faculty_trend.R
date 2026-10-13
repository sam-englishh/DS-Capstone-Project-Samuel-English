source("scripts/00_setup.R")
faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")
admin_salaries   <- read_csv("data/admin_salaries_filtered.csv")
cpi_annual       <- read_csv("data/cpi_deflator.csv")

# --- faculty: real (2024-dollar) average salary by rank -----------------
trend_data <- faculty_salaries %>%
  filter(
    academic_rank %in% c(1, 2, 3),   # Professor, Associate, Assistant
    sex == 99,                        # collapse sex
    salary_outlays >= 0                # drop -1 placeholder rows
  ) %>%
  group_by(year, unitid, academic_rank_label) %>%
  slice_max(instruc_staff_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(year, academic_rank_label) %>%
  summarise(avg_salary = mean(real_salary_2024, na.rm = TRUE), .groups = "drop")

# sanity check: how many colleges contributed to each rank/year?
faculty_salaries %>%
  filter(academic_rank %in% c(1, 2, 3), sex == 99, salary_outlays >= 0) %>%
  group_by(year, unitid, academic_rank_label) %>%
  slice_max(instruc_staff_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  count(year, academic_rank_label) %>%
  print(n = 30)

# --- admin: real (2024-dollar) average salary, Mgmt + Business/Fin Ops --
admin_trend <- admin_salaries %>%
  filter(staff_category %in% c(5, 6), noninstruc_staff_count > 0) %>%
  group_by(year, unitid) %>%
  summarise(
    total_outlays = sum(salary_outlays, na.rm = TRUE),
    total_staff   = sum(noninstruc_staff_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(cpi_annual %>% select(year, deflator_to_2024), by = "year") %>%
  mutate(college_real_avg = (total_outlays / total_staff) * deflator_to_2024) %>%
  group_by(year) %>%
  summarise(avg_salary = mean(college_real_avg, na.rm = TRUE), .groups = "drop") %>%
  mutate(academic_rank_label = "Administrative (Mgmt + Business/Fin Ops)")

# --- combine and plot -----------------------------------------------------
combined_trend <- bind_rows(trend_data, admin_trend)

ggplot(combined_trend, aes(x = year, y = avg_salary, color = academic_rank_label)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Average Faculty vs. Administrative Salary (Real, 2024 Dollars)",
    subtitle = "Averaged across 19 liberal arts colleges",
    x = "Year", y = "Average Salary (2024 dollars)", color = "Group"
  ) +
  theme_minimal()