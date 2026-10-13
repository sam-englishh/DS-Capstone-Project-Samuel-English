source("scripts/00_setup.R")
college_lookup <- read_csv("data/college_lookup.csv")

# --- Step 1: combine every yearly admin salary file into one table --
year_files <- list.files("data", pattern = "^admin_salaries_\\d{4}\\.csv$", full.names = TRUE)
length(year_files)   # sanity check: should match however many years actually succeeded

admin_salaries_raw <- year_files %>% map_dfr(read_csv)

nrow(admin_salaries_raw)
glimpse(admin_salaries_raw)

# --- Step 2: filter down to just the 19 target colleges -------------
admin_salaries_filtered <- admin_salaries_raw %>%
  filter(unitid %in% college_lookup$unitid)

nrow(admin_salaries_filtered)

# sanity check: how many of the 19 unitids actually show up?
n_distinct(admin_salaries_filtered$unitid)

# sanity check: which years are actually present?
sort(unique(admin_salaries_filtered$year))

# --- Step 3: join in the readable college name -----------------------
admin_salaries_final <- admin_salaries_filtered %>%
  left_join(college_lookup, by = "unitid")

glimpse(admin_salaries_final)

# --- Step 4: save ------------------------------------------------------
write_csv(admin_salaries_final, "data/admin_salaries_filtered.csv")