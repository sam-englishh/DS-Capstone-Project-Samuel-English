source("scripts/00_setup.R")
college_lookup <- read_csv("data/college_lookup.csv")

# Test pull: try filtering by BOTH year and unitid at once, since this
# topic might behave differently than the salary endpoints did.
ratio_test <- get_education_data(
  level   = "college-university",
  source  = "ipeds",
  topic   = "student-faculty-ratio",
  filters = list(year = 2020, unitid = college_lookup$unitid)
)

nrow(ratio_test)
glimpse(ratio_test)
print(ratio_test)

ratio_all <- get_education_data(
  level   = "college-university",
  source  = "ipeds",
  topic   = "student-faculty-ratio",
  filters = list(year = 2009:2024, unitid = college_lookup$unitid)
)

nrow(ratio_all)
glimpse(ratio_all)
sort(unique(ratio_all$year))


# join in readable college name and save
ratio_final <- ratio_all %>%
  left_join(college_lookup, by = "unitid")

write_csv(ratio_final, "data/student_faculty_ratio.csv")