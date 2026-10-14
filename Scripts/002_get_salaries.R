source("scripts/00_setup.R")
college_lookup <- read_csv("data/college_lookup.csv")

years_needed <- 2005:2024

for (yr in years_needed) {
  out_file <- paste0("data/faculty_salaries_", yr, ".csv")
  
  if (file.exists(out_file)) {
    message("Year ", yr, " already saved, skipping.")
    next
  }
  
  message("Pulling year ", yr, "...")
  
  result <- tryCatch(
    get_education_data(
      level   = "college-university",
      source  = "ipeds",
      topic   = "salaries-instructional-staff",
      filters = list(year = yr)
    ),
    error = function(e) {
      message("FAILED on year ", yr, ": ", conditionMessage(e))
      NULL
    }
  )
  
  if (!is.null(result)) {
    write_csv(result, out_file)
    message("Saved year ", yr)
  }
}
