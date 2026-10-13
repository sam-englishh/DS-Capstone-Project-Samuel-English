source("scripts/00_setup.R")
ratio_data     <- read_csv("data/student_faculty_ratio.csv")
enrollment     <- read_csv("data/enrollment.csv")
admin_salaries <- read_csv("data/admin_salaries_filtered.csv")

# --- students-per-faculty: already a ready-made column ----------------
sf_trend <- ratio_data %>%
  group_by(year) %>%
  summarise(avg_ratio = mean(student_faculty_ratio, na.rm = TRUE), .groups = "drop") %>%
  mutate(ratio_type = "Students per faculty")

# --- students-per-administrator: build it ourselves --------------------
admin_headcount <- admin_salaries %>%
  filter(staff_category %in% c(5, 6)) %>%   # Management + Business/Fin Ops
  group_by(year, unitid) %>%
  summarise(admin_staff = sum(noninstruc_staff_count, na.rm = TRUE), .groups = "drop")

sa_data <- enrollment %>%
  inner_join(admin_headcount, by = c("year", "unitid")) %>%
  filter(admin_staff > 0) %>%
  mutate(student_admin_ratio = enrollment_fall / admin_staff)

# sanity check: how many colleges contribute to each year's total?
sa_data %>% count(year) %>% print(n = 20)

# use totals (sum/sum), not an average of 19 separate ratios - this
# avoids one outlier college's weird ratio skewing the whole year
sa_trend <- sa_data %>%
  group_by(year) %>%
  summarise(
    avg_ratio = sum(enrollment_fall) / sum(admin_staff),
    .groups = "drop"
  ) %>%
  mutate(ratio_type = "Students per administrator")

# --- combine and plot ---------------------------------------------------
combined_ratios <- bind_rows(sf_trend, sa_trend)

ggplot(combined_ratios, aes(x = year, y = avg_ratio, color = ratio_type)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Students per Faculty vs. Students per Administrator",
    subtitle = "Averaged across 19 liberal arts colleges",
    x = "Year", y = "Students per Staff Member", color = NULL
  ) +
  theme_minimal()