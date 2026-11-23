source("scripts/00_setup.R")
library(httr)
library(jsonlite)

college_names <- c(
  "Bowdoin College", "Bryn Mawr College", "Carleton College", "Claremont McKenna College",
  "Davidson College", "Grinnell College", "Harvey Mudd College", "Haverford College",
  "Middlebury College", "Oberlin College", "Occidental College", "Pitzer College",
  "Pomona College", "Scripps College", "Smith College", "Swarthmore College",
  "Wellesley College", "Wesleyan University", "Williams College"
)

lookup_ein <- function(name) {
  url <- paste0("https://projects.propublica.org/nonprofits/api/v2/search.json?q=",
                URLencode(name))
  res <- GET(url)
  parsed <- fromJSON(content(res, "text", encoding = "UTF-8"))$organizations
  match <- parsed[parsed$name == name, ]
  if (nrow(match) == 0) return(tibble(inst_name = name, ein = NA, strein = NA))
  tibble(inst_name = name, ein = match$ein[1], strein = match$strein[1])
}

ein_lookup <- map_dfr(college_names, lookup_ein)
print(ein_lookup)  # eyeball this before patching

# Five colleges are registered under a different legal name than the common
# one, so the exact-match search above misses them:
#   Claremont McKenna  -> "Claremont Mckenna College"
#   Grinnell            -> "The Trustees Of Grinnell College"
#   Haverford            -> "Corporation Of Haverford College"
#   Middlebury           -> "President & Fellows Of Middlebury College"
#   Smith                -> "Trustees Of The Smith College"
# Patched in manually below, verified against city/state on ProPublica.

ein_lookup <- ein_lookup %>%
  mutate(
    ein = case_when(
      inst_name == "Claremont McKenna College" ~ 951664101,
      inst_name == "Grinnell College"           ~ 420680387,
      inst_name == "Haverford College"          ~ 236002304,
      inst_name == "Middlebury College"         ~ 30179298,
      inst_name == "Smith College"              ~ 41843040,
      TRUE ~ ein
    ),
    strein = case_when(
      inst_name == "Claremont McKenna College" ~ "95-1664101",
      inst_name == "Grinnell College"           ~ "42-0680387",
      inst_name == "Haverford College"          ~ "23-6002304",
      inst_name == "Middlebury College"         ~ "03-0179298",
      inst_name == "Smith College"              ~ "04-1843040",
      TRUE ~ strein
    )
  )

sum(is.na(ein_lookup$ein))  # should be 0

write_csv(ein_lookup, "data/college_eins.csv")