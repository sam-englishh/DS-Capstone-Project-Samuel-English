source("scripts/00_setup.R")
faculty_salaries <- read_csv("data/faculty_salaries_filtered.csv")

professor_trend <- faculty_salaries %>%
  filter(academic_rank_label == "Professor", sex == 99, salary_outlays >= 0) %>%
  group_by(year, unitid, inst_name) %>%
  slice_max(instruc_staff_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(year, inst_name, real_salary_2024, real_salary_2024_colAdj) %>%
  mutate(highlight = inst_name %in% c("Pomona College", "Williams College", "Claremont McKenna College"))

y_range <- range(c(professor_trend$real_salary_2024, professor_trend$real_salary_2024_colAdj), na.rm = TRUE)

highlight_colors <- c(
  "Pomona College" = "#2C7BB6",
  "Williams College" = "#D7191C",
  "Claremont McKenna College" = "#1A9641"
)

plot_highlighted <- function(data, y_var, title, subtitle, y_lab) {
  y_var <- rlang::sym(y_var)
  
  label_data <- data %>%
    filter(highlight, !is.na(!!y_var)) %>%
    group_by(inst_name) %>%
    filter(year == max(year)) %>%
    ungroup()
  
  ggplot(data, aes(x = year, y = !!y_var, group = inst_name)) +
    geom_line(data = filter(data, !highlight), color = "grey80", linewidth = 0.5) +
    geom_line(data = filter(data, highlight), aes(color = inst_name), linewidth = 1.2) +
    geom_text(data = label_data, aes(label = inst_name, color = inst_name),
              hjust = 0, nudge_x = 0.3, fontface = "bold", size = 3.5, show.legend = FALSE) +
    coord_cartesian(ylim = y_range, clip = "off") +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.22))) +
    scale_y_continuous(labels = scales::label_dollar()) +
    scale_color_manual(values = highlight_colors) +
    labs(title = title, subtitle = subtitle, x = "Year", y = y_lab) +
    theme_minimal() +
    theme(legend.position = "none", plot.margin = margin(5.5, 90, 5.5, 5.5))
}

# --- Chart 1: unadjusted ---
plot_unadjusted <- plot_highlighted(
  professor_trend, "real_salary_2024",
  "Professor Salary: Pomona, CMC & Williams (with 19-College Context)",
  "Real 2024 dollars, no cost-of-living adjustment",
  "Average Professor Salary"
)
ggsave("plot_unadjusted.png", plot = plot_unadjusted, width = 8, height = 5, dpi = 300, bg = "white")

# --- Chart 2: cost-of-living adjusted ---
plot_adjusted <- plot_highlighted(
  professor_trend, "real_salary_2024_colAdj",
  "Professor Salary: Pomona, CMC & Williams (with 19-College Context)",
  "Real 2024 dollars, adjusted for local cost of living",
  "Average Professor Salary (COL-Adjusted)"
)
ggsave("plot_adjusted.png", plot = plot_adjusted, width = 8, height = 5, dpi = 300, bg = "white")