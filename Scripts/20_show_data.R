head(faculty_salaries, 6)



faculty_example <- faculty_salaries %>%
  filter(inst_name == "Pomona College", year == 2024,
         academic_rank_label %in% c("Assistant professor", "Associate professor", "Professor"),
         sex == 99, salary_outlays >= 0) %>%
  group_by(academic_rank_label) %>%
  slice_max(instruc_staff_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(inst_name, year, category = academic_rank_label, group = "Faculty", real_salary = real_salary_2024)

admin_example <- admin_salaries %>%
  filter(inst_name == "Pomona College", year == 2024, staff_category %in% c(5, 6)) %>%
  transmute(inst_name, year, category = staff_category_label, group = "Administrative", real_salary = real_avg_salary_2024)

bind_rows(faculty_example, admin_example) %>%
  arrange(group, category)