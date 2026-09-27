# Xylem pressure and air temperature: manuscript figure
# Year: 2026
# Script author: Theresa Weinspach
# Manuscript authors: enter the final author list before sharing.
# Purpose: reproduce the final two-panel temperature/pressure figure.
# Run from the root of the CicTen RStudio Project.
# Inputs: data/Pressure bomb .csv and data/Weather data.csv
# Outputs: the figure and two hourly summary tables in output/.

###-### Chapter 1: Packages and reproducibility ###-###
# Package versions are written to output/package_versions_xylem.txt.

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(patchwork)

dir.create("output", showWarnings = FALSE, recursive = TRUE)

packages_used <- c(
  "readr", "dplyr", "tidyr", "stringr", "lubridate",
  "ggplot2", "patchwork"
)

writeLines(
  c(
    paste("R ==", getRversion()),
    paste(
      packages_used,
      vapply(packages_used, function(pkg) as.character(packageVersion(pkg)), ""),
      sep = " == "
    )
  ),
  "output/package_versions_xylem.txt"
)

# Use a single artificial date for both 24-hour axes. The actual weather
# dates are retained during filtering, then collapsed by hour of day.
dummy_date <- "2000-01-01"
next_date <- "2000-01-02"
clock_time <- function(x, date = dummy_date) {
  as.POSIXct(paste(date, x), format = "%Y-%m-%d %H:%M", tz = "UTC")
}

axis_limits <- c(clock_time("00:00"), clock_time("01:00", next_date))
cols_all <- palette.colors(12, palette = "Paired")
window_colors <- c(
  Morning = cols_all[5], Afternoon = cols_all[9],
  Evening = cols_all[3], Night = cols_all[1]
)

#----------------------------
###-### Chapter 2: Pressure data and hourly summary ###-###

# Read all columns as text before parsing numbers. The pressure-bomb sheet
# holds replicate readings from Control, Liriope and Muhly. The paper figure
# pools these groups at each clock time, as in the original plotting code.
pressure_raw <- read_csv(
  "data/Pressure bomb .csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  rename_with(str_trim)

pressure_long <- pressure_raw %>%
  pivot_longer(-time, names_to = "sample", values_to = "reading") %>%
  transmute(
    time = str_trim(time),
    treatment = word(sample, 1),
    pressure_bar = -abs(parse_number(reading))
  ) %>%
  filter(!is.na(time), !is.na(pressure_bar))

# Values in the source sheet are pressure magnitudes in bar. The original
# analysis assigned a negative sign to express xylem tension. One bar is
# 0.1 MPa, so negative values are retained during conversion.
# The plotted bars show the MINIMUM and MAXIMUM individual reading at each
# clock time; they are neither standard errors nor confidence intervals.
pressure_hourly <- pressure_long %>%
  group_by(time) %>%
  summarise(
    n_readings = n(),
    mean_mpa = mean(pressure_bar) * 0.1,
    min_mpa = min(pressure_bar) * 0.1,
    max_mpa = max(pressure_bar) * 0.1,
    .groups = "drop"
  ) %>%
  mutate(clock = clock_time(substr(time, 1, 5))) %>%
  arrange(clock)

if (!nrow(pressure_hourly) || anyNA(pressure_hourly$clock)) {
  stop("Check time and pressure fields in data/Pressure bomb .csv.")
}

write_csv(pressure_hourly, "output/xylem_pressure_hourly_mpa.csv")

#----------------------------
###-### Chapter 3: Weather data and hourly summary ###-###

# The source station reports degrees Fahrenheit. Only May 20-22, 2025 are
# used here. Each clock hour pools observations from those three dates;
# the min/max bars show the range of those observations. Consequently the
# plot describes a mean daily pattern, not one continuous 24-hour record.
weather_raw <- read_delim(
  "data/Weather data.csv",
  delim = ";",
  col_types = cols(.default = col_character()),
  trim_ws = TRUE,
  show_col_types = FALSE
)

weather <- weather_raw %>%
  transmute(
    datetime = dmy_hm(Timestamp, tz = "UTC"),
    temperature_f = parse_number(
      `Wireless ET Station: B: Temperature`,
      locale = locale(decimal_mark = ",")
    )
  ) %>%
  mutate(temperature_c = (temperature_f - 32) * 5 / 9) %>%
  filter(
    !is.na(datetime), !is.na(temperature_c),
    datetime >= clock_time("00:00", "2025-05-20"),
    datetime < clock_time("00:00", "2025-05-23")
  )

temperature_hourly <- weather %>%
  mutate(hour_of_day = format(floor_date(datetime, "hour"), "%H:00")) %>%
  group_by(hour_of_day) %>%
  summarise(
    n_readings = n(),
    mean_c = mean(temperature_c),
    min_c = min(temperature_c),
    max_c = max(temperature_c),
    .groups = "drop"
  ) %>%
  mutate(clock = clock_time(hour_of_day)) %>%
  arrange(clock)

if (!nrow(temperature_hourly) || anyNA(temperature_hourly$clock)) {
  stop("Check timestamp and temperature fields in data/Weather data.csv.")
}

write_csv(temperature_hourly, "output/temperature_hourly_c.csv")

#----------------------------
###-### Chapter 4: Sampling-window bands ###-###

# These bands show when the cicadas were sampled. They are annotations,
# not time periods inferred from the pressure or temperature measurements.
# Dec/Dem windows span the full day; Cas windows follow the original figure.
dec_dem_windows <- data.frame(
  time_window = c("Night", "Morning", "Afternoon", "Evening", "Night"),
  xmin = c(
    clock_time("00:00"), clock_time("07:00"), clock_time("10:00"),
    clock_time("17:00"), clock_time("20:00")
  ),
  xmax = c(
    clock_time("07:00"), clock_time("10:00"), clock_time("17:00"),
    clock_time("20:00"), clock_time("00:00", next_date)
  ),
  species_group = "Dec + Dem"
)

cas_windows <- data.frame(
  time_window = c("Morning", "Afternoon", "Evening"),
  xmin = c(clock_time("08:00"), clock_time("12:00"), clock_time("18:00")),
  xmax = c(clock_time("11:00"), clock_time("17:00"), clock_time("20:00")),
  species_group = "Cas"
)

sampling_windows <- bind_rows(dec_dem_windows, cas_windows) %>%
  mutate(
    time_window = factor(
      time_window, levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    label = if_else(species_group == "Cas" & time_window == "Evening",
                    "Eve", as.character(time_window)),
    ymin = if_else(species_group == "Dec + Dem", -2.05, -1.83),
    ymax = if_else(species_group == "Dec + Dem", -1.85, -1.68),
    y_label = (ymin + ymax) / 2,
    x_label = xmin + (xmax - xmin) / 2
  )

#----------------------------
###-### Chapter 5: Final two-panel figure ###-###

temperature_panel <- ggplot(temperature_hourly, aes(x = clock)) +
  geom_line(aes(y = mean_c), linewidth = 1, color = "black") +
  geom_point(aes(y = mean_c), size = 2.3, color = "black") +
  geom_errorbar(
    aes(ymin = min_c, ymax = max_c),
    width = 20 * 60, linewidth = 0.5, color = "black"
  ) +
  scale_x_datetime(
    date_breaks = "2 hours", date_labels = "%H:%M",
    limits = axis_limits, expand = c(0, 0), timezone = "UTC"
  ) +
  labs(x = NULL, y = "Temperature (°C)") +
  theme_classic(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  )

pressure_panel <- ggplot() +
  geom_rect(
    data = sampling_windows,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = time_window),
    color = "black", linewidth = 0.4, alpha = 0.55
  ) +
  geom_text(
    data = sampling_windows,
    aes(x = x_label, y = y_label, label = label), size = 3
  ) +
  geom_line(
    data = pressure_hourly,
    aes(x = clock, y = mean_mpa), linewidth = 1, color = "black"
  ) +
  geom_point(
    data = pressure_hourly,
    aes(x = clock, y = mean_mpa), size = 2, color = "black"
  ) +
  geom_errorbar(
    data = pressure_hourly,
    aes(x = clock, ymin = min_mpa, ymax = max_mpa),
    width = 20 * 60, color = "black"
  ) +
  annotate(
    "text", x = clock_time("00:20", next_date), y = -1.925,
    label = "Dec + Dem", hjust = 0, size = 3.5
  ) +
  annotate(
    "text", x = clock_time("00:20", next_date), y = -1.785,
    label = "Cas", hjust = 0, size = 3.5
  ) +
  scale_x_datetime(
    date_breaks = "2 hours", date_labels = "%H:%M",
    limits = axis_limits, expand = c(0, 0), timezone = "UTC"
  ) +
  scale_y_reverse(breaks = seq(0, -2.0, by = -0.2)) +
  coord_cartesian(ylim = c(0, -2.0), clip = "off") +
  scale_fill_manual(values = window_colors, drop = FALSE) +
  labs(x = "Time of day", y = "Xylem pressure (MPa)") +
  theme_classic() +
  theme(
    legend.position = "none", panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    plot.margin = margin(5.5, 90, 5.5, 5.5)
  )

combined_temp_xylem <- temperature_panel / pressure_panel +
  plot_layout(heights = c(1, 1.15)) +
  plot_annotation(tag_levels = "A", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 16, face = "bold"))

print(combined_temp_xylem)
ggsave(
  "output/Figure_temperature_xylem_AB.png",
  combined_temp_xylem, width = 9, height = 8, dpi = 300
)

# Interpretation: Panel A is the mean temperature by clock hour across the
# selected dates. Panel B pools all available pressure replicates by clock
# time. Both panels show observed min/max ranges, not uncertainty about the
# mean. The aligned 24-hour axes allow visual comparison, but these pooled
# summaries alone do not test an association between pressure and temperature.
