source("scripts/00_setup.R")

# Pull the same single year twice with identical filters: once raw,
# once with add_labels = TRUE, so we can line up the codes with
# their meanings (same trick as academic_rank).

staff_codes_raw <- get_education_data(
  level   = "college-university",
  source  = "ipeds",
  topic   = "salaries-noninstructional-staff",
  filters = list(year = 2020)
)

staff_codes_labeled <- get_education_data(
  level   = "college-university",
  source  = "ipeds",
  topic   = "salaries-noninstructional-staff",
  filters = list(year = 2020),
  add_labels = TRUE
)

# sanity check FIRST - must match before trusting the row-lineup below
nrow(staff_codes_raw)
nrow(staff_codes_labeled)

staff_category_lookup <- tibble(
  staff_category       = staff_codes_raw$staff_category,
  staff_category_label = staff_codes_labeled$staff_category
) %>%
  distinct() %>%
  arrange(staff_category)

print(staff_category_lookup)

# --- Apply it to your real dataset ------------------------------------
admin_salaries <- read_csv("data/admin_salaries_filtered.csv")

admin_salaries <- admin_salaries %>%
  left_join(staff_category_lookup, by = "staff_category")

# sanity check: no NAs
sum(is.na(admin_salaries$staff_category_label))

write_csv(admin_salaries, "data/admin_salaries_filtered.csv")