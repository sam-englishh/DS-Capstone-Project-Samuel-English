source("scripts/00_setup.R")
target_colleges <- c(
  "Bowdoin College",
  "Bryn Mawr College",
  "Carleton College",
  "Claremont McKenna College",
  "Davidson College",
  "Grinnell College",
  "Harvey Mudd College",
  "Haverford College",
  "Middlebury College",
  "Oberlin College",
  "Occidental College",
  "Pitzer College",
  "Pomona College",
  "Scripps College",
  "Smith College",
  "Swarthmore College",
  "Wellesley College",
  "Wesleyan University",
  "Williams College"
)
directory_raw <- get_education_data(
  level   = "college-university",
  source  = "ipeds",
  topic   = "directory",
  filters = list(year = 2022)
)

glimpse(directory_raw)

college_lookup <- directory_raw |>
  filter(inst_name %in% target_colleges) |>
  select(unitid, inst_name)

print(college_lookup)
nrow(college_lookup)

write_csv(college_lookup, "data/college_lookup.csv")
