source("scripts/00_setup.R")

# Pull the same single year twice with identical filters: once with raw
# numeric codes (like all your other pulls), once with add_labels = TRUE
# so the codes come back as text instead. Same filters = same rows in
# the same order, just formatted differently — so we can line them up.

codes_raw <- get_education_data(
  level   = "college-university",
  source  = "ipeds",
  topic   = "salaries-instructional-staff",
  filters = list(year = 2020)
)

codes_labeled <- get_education_data(
  level   = "college-university",
  source  = "ipeds",
  topic   = "salaries-instructional-staff",
  filters = list(year = 2020),
  add_labels = TRUE
)

# sanity check FIRST - these must match or the row-lineup trick below
# doesn't work
nrow(codes_raw)
nrow(codes_labeled)

# pair up the numeric code with whatever showed up in the same row
# position once labeled, then collapse to the unique pairs
rank_lookup <- tibble(
  academic_rank       = codes_raw$academic_rank,
  academic_rank_label = codes_labeled$academic_rank
) %>%
  distinct() %>%
  arrange(academic_rank)

print(rank_lookup)


# --- Apply the mapping to your real filtered dataset -----------------
faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")

faculty_salaries <- faculty_salaries %>%
  left_join(rank_lookup, by = "academic_rank")

# sanity check: every row should now have a label, none should be NA
sum(is.na(faculty_salaries$academic_rank_label))

write_csv(faculty_salaries, "data/faculty_salaries_filtered.csv")

# --- Build lookup tables for sex and contract_length, same trick -----
sex_lookup <- tibble(
  sex       = codes_raw$sex,
  sex_label = codes_labeled$sex
) %>%
  distinct() %>%
  arrange(sex)

print(sex_lookup)

contract_length_lookup <- tibble(
  contract_length       = codes_raw$contract_length,
  contract_length_label = codes_labeled$contract_length
) %>%
  distinct() %>%
  arrange(contract_length)

print(contract_length_lookup)

# --- Apply both to your real dataset ----------------------------------
faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")

faculty_salaries <- faculty_salaries %>%
  left_join(sex_lookup, by = "sex") %>%
  left_join(contract_length_lookup, by = "contract_length")

# sanity check: no NAs in either new label column
sum(is.na(faculty_salaries$sex_label))
sum(is.na(faculty_salaries$contract_length_label))

write_csv(faculty_salaries, "data/faculty_salaries_filtered.csv")