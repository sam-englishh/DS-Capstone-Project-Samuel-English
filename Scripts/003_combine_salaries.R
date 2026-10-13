source("scripts/00_setup.R")
college_lookup <- read_csv("data/college_lookup.csv")

# --- Step 1: combine every yearly file into one big table ---------
year_files <- list.files("data", pattern = "^faculty_salaries_\\d{4}\\.csv$", full.names = TRUE)
length(year_files)   # sanity check: should be ~20 (one per year you saved)

faculty_salaries_raw <- year_files %>% map_dfr(read_csv)

nrow(faculty_salaries_raw)
glimpse(faculty_salaries_raw)

# --- Step 2: filter down to just the 19 target colleges -----------
faculty_salaries_filtered <- faculty_salaries_raw %>%
  filter(unitid %in% college_lookup$unitid)

nrow(faculty_salaries_filtered)

# sanity check: how many of the 19 unitids actually show up?
n_distinct(faculty_salaries_filtered$unitid)

# sanity check: which years are actually present?
sort(unique(faculty_salaries_filtered$year))

# --- Step 3: join in the readable college name ---------------------
faculty_salaries_final <- faculty_salaries_filtered %>%
  left_join(college_lookup, by = "unitid")

glimpse(faculty_salaries_final)

# --- Step 4: save ----------------------------------------------------
write_csv(faculty_salaries_final, "data/faculty_salaries_filtered.csv")
