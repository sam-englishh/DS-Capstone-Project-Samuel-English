enrollment_test2 <- get_education_data(
  level    = "college-university",
  source   = "ipeds",
  topic    = "fall-enrollment",
  subtopic = c("race", "sex"),
  filters  = list(year = 2009:2024, unitid = college_lookup$unitid)
)

nrow(enrollment_test2)
glimpse(enrollment_test2)
distinct(enrollment_test2, race)
distinct(enrollment_test2, sex)

# check for a clean grand-total row again
enrollment_test2 %>%
  filter(race == 99, sex == 99) %>%
  nrow()

enrollment_test2 %>%
  filter(
    ftpt           == 99,
    level_of_study == 99,
    degree_seeking == 99,
    class_level    == 99,
    sex            == 99,
    race           == 99
  ) %>%
  nrow()

enrollment_final <- enrollment_test2 %>%
  filter(
    ftpt           == 99,
    level_of_study == 99,
    degree_seeking == 99,
    class_level    == 99,
    sex            == 99,
    race           == 99
  ) %>%
  select(unitid, year, enrollment_fall) %>%
  left_join(college_lookup, by = "unitid")

# sanity check: do the actual numbers look like real total enrollment
# (hundreds to low thousands), not tiny freshman-by-state numbers?
enrollment_final %>% arrange(unitid, year) %>% head(20)

write_csv(enrollment_final, "data/enrollment.csv")