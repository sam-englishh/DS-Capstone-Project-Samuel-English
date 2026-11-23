source("scripts/00_setup.R")
install.packages("fixest")
library(fixest)  # install.packages("fixest") if you don't have it yet

faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")
admin_salaries   <- read_csv("data/admin_salaries_filtered.csv")

# --- Faculty panel: institution-year average real salary across ranks ---
faculty_panel <- faculty_salaries %>%
  filter(academic_rank %in% c(1, 2, 3), sex == 99, salary_outlays >= 0) %>%
  group_by(year, unitid, inst_name, academic_rank_label) %>%
  slice_max(instruc_staff_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(year, unitid, inst_name) %>%
  summarise(real_comp = mean(real_salary_2024, na.rm = TRUE), .groups = "drop") %>%
  mutate(staff_type = "Faculty")

# --- Administrative panel: institution-year payroll-weighted avg real salary ---
admin_panel <- admin_salaries %>%
  filter(staff_category %in% c(5, 6), noninstruc_staff_count > 0) %>%
  group_by(year, unitid, inst_name) %>%
  summarise(
    total_outlays = sum(salary_outlays, na.rm = TRUE),
    total_staff   = sum(noninstruc_staff_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(faculty_salaries %>% distinct(year, deflator_to_2024), by = "year") %>%
  mutate(real_comp = (total_outlays / total_staff) * deflator_to_2024) %>%
  select(year, unitid, inst_name, real_comp) %>%
  mutate(staff_type = "Administrative")

# --- Stack into one long panel, restricted to years both groups exist (2012+) ---
panel <- bind_rows(faculty_panel, admin_panel) %>%
  filter(year >= 2012) %>%
  mutate(
    staff_type  = factor(staff_type, levels = c("Faculty", "Administrative")),
    time_period = factor(if_else(year >= 2020, "Pandemic & After", "2012-2019"),
                         levels = c("2012-2019", "Pandemic & After")),
    unitid      = factor(unitid)
  )

# sanity checks before modeling
panel %>% count(staff_type, time_period)
sum(is.na(panel$real_comp))  # should be 0

# --- Panel regression: institution + year fixed effects, clustered SEs ---
model <- feols(
  real_comp ~ staff_type * time_period | unitid + year,
  data    = panel,
  cluster = ~unitid
)

summary(model)
etable(model)  # cleaner, write-up-ready table