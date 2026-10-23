source("scripts/00_setup.R")

faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")
admin_salaries   <- read_csv("data/admin_salaries_filtered.csv")

faculty_headcount <- faculty_salaries %>%
  filter(academic_rank %in% c(1, 2, 3), sex == 99, salary_outlays >= 0) %>%
  group_by(year, unitid, academic_rank_label) %>%
  slice_max(instruc_staff_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(total_headcount = sum(instruc_staff_count, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = "Faculty")

admin_headcount <- admin_salaries %>%
  filter(staff_category %in% c(5, 6), noninstruc_staff_count > 0) %>%
  group_by(year) %>%
  summarise(total_headcount = sum(noninstruc_staff_count, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = "Administrative")

headcount_growth <- bind_rows(faculty_headcount, admin_headcount) %>%
  arrange(group, year) %>%
  group_by(group) %>%
  mutate(
    base_value = total_headcount[year == 2012],
    pct_change = (total_headcount / base_value - 1) * 100
  ) %>%
  ungroup() %>%
  filter(year >= 2012)

ggplot(headcount_growth, aes(x = year, y = pct_change, color = group)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Cumulative Headcount Growth Since 2012: Faculty vs. Administrative Staff",
    subtitle = "Summed across 19 liberal arts colleges",
    x = "Year", y = "Cumulative % Growth Since 2012", color = "Group"
  ) +
  theme_minimal()