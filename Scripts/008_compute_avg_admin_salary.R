source("scripts/00_setup.R")
admin_salaries <- read_csv("data/admin_salaries_filtered.csv")

admin_salaries <- admin_salaries %>%
  mutate(
    avg_salary = if_else(
      noninstruc_staff_count > 0,
      salary_outlays / noninstruc_staff_count,
      NA_real_
    )
  )

# sanity checks
summary(admin_salaries$avg_salary)
sum(is.na(admin_salaries$avg_salary))          # rows with 0 staff - expected to be NA
sum(admin_salaries$noninstruc_staff_count == 0) # should match the NA count above

write_csv(admin_salaries, "data/admin_salaries_filtered.csv")