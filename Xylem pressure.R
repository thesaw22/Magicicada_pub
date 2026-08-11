library(readr)
Pressure_bomb_ <- read_csv("~/Desktop/Cicada/Tennessee/Tennessee R/Pressure bomb .csv", 
                           col_types = cols(`Control 2` = col_number(), 
                                            `Liriope 4` = col_number(), `Muhly 5` = col_number(), 
                                            `Control 10` = col_number(), `Liriope 12` = col_number(), 
                                            `Muhly 3` = col_number(), `Control 26` = col_number(), 
                                            `Liriope 9` = col_number(), `Muhly 12` = col_number(), 
                                            `Control 8` = col_number(), `Liriope 10` = col_number(), 
                                            `Muhly 4` = col_number()))
View(Pressure_bomb_)

# Color theme for this paper: 
cols_all <- palette.colors(12, palette = "Paired")

barplot(rep(1, length(cols_all)),
        col = cols_all,
        names.arg = 1:length(cols_all),
        border = NA,
        main = "Paired palette (indexed)")

#> Writing the data into long format
#> not adding a plot_iD 
#> also changing the numbers into negative -> negative pressure 

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

pressure_tidy <- Pressure_bomb_ %>%
  rename_with(str_trim) %>%
  rename(time = `time`) %>%
  pivot_longer(
    cols = -time,
    names_to = "sample",
    values_to = "pressure"
  ) %>%
  mutate(
    treatment = word(sample, 1),
    time = trimws(time)
  ) %>%
  select(time, treatment, pressure) %>%
  drop_na(pressure)

pressure_tidy <- pressure_tidy %>%
  mutate(pressure = -abs(pressure))

pressure_tidy

#Saving the long format as file
write_csv(pressure_tidy, "~/Desktop/Cicada/Tennessee R/CicTen/pressure_tidy.csv")

#> Plotting the three different groups separate
#> calculating the average of the groups on the hour first 
#> plotting as a line 

library(dplyr)
library(ggplot2)

pressure_summary <- pressure_tidy %>%
  group_by(time, treatment) %>%
  summarise(
    mean_pressure = mean(pressure, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(pressure_summary, aes(x = as.POSIXct(time, format = "%H:%M"), 
                             y = mean_pressure, 
                             color = treatment)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  labs(x = "Time", y = "Xylem Pressure (bar)", color = "Treatment") +
  theme_minimal()

#> combining the Treaatments 
#> aadding min and max error bars 

library(dplyr)
library(ggplot2)

pressure_summary_all <- pressure_tidy %>%
  group_by(time) %>%
  summarise(
    mean_pressure = mean(pressure, na.rm = TRUE),
    min_pressure = min(pressure, na.rm = TRUE),
    max_pressure = max(pressure, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(pressure_summary_all,
       aes(x = as.POSIXct(time, format = "%H:%M"),
           y = mean_pressure)) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(color = "black") +
  geom_errorbar(aes(ymin = min_pressure, ymax = max_pressure),
                width = 0.05,
                color = "black") +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  scale_y_reverse(
    breaks = seq(0, -20, by = -2)
  ) + 
  labs(
    x = "Time",
    y = "Xylem Pressure (bar)"
  ) +
  theme_minimal()

#> adding colorful backround to give a timewindow of morning, noon, and evening 
#> step 1: giving a color palette for timeslot
#> step 2: creating the time windows 
#>  morning: 8-10
#>  afternoon: 10-5
#>  evening: 5-8
#>  night> 8-8
#>step 3: adding background shading to the plot 

paired <- palette.colors(12, "Paired")

bg_cols <- c(
  "morning" = paired[5],
  "afternoon" = paired[9],
  "evening" = paired[3],
  "night" = paired[1]
)

time_blocks <- data.frame(
  period = c("night", "morning", "afternoon", "evening", "night"),
  xmin = as.POSIXct(c("05:00", "07:00", "10:00", "17:00", "20:00"), format = "%H:%M"),
  xmax = as.POSIXct(c("08:00", "10:00", "17:00", "20:00", "21:00"), format = "%H:%M"),
  ymin = -Inf,
  ymax = Inf
)

ggplot() +
  # background shading (NO legend)
  geom_rect(data = time_blocks,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = period),
            alpha = 0.2,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  
  geom_line(data = pressure_summary_all,
            aes(x = as.POSIXct(time, format = "%H:%M"),
                y = mean_pressure),
            linewidth = 1,
            color = "black") +
  
  geom_point(data = pressure_summary_all,
             aes(x = as.POSIXct(time, format = "%H:%M"),
                 y = mean_pressure),
             color = "black") +
  
  geom_errorbar(data = pressure_summary_all,
                aes(x = as.POSIXct(time, format = "%H:%M"),
                    ymin = min_pressure,
                    ymax = max_pressure),
                width = 0.05,
                color = "black") +
  
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  
  scale_y_reverse(
    breaks = seq(0, -20, by = -2)
  ) +
  
  scale_fill_manual(values = bg_cols) +
  
  labs(
    x = "Time",
    y = "Xylem Pressure (bar)"
  ) +
  
  theme_minimal()

#> getting the stats 
#> generating a full summary table with the importaant information 
#> stats more for the time and not all the meassurments 

library(dplyr)

pressure_stats_combined <- pressure_tidy %>%
  group_by(time) %>%
  summarise(
    n = n(),
    mean = mean(pressure, na.rm = TRUE),
    sd = sd(pressure, na.rm = TRUE),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, df = n - 1) * se,
    ci_upper = mean + qt(0.975, df = n - 1) * se,
    min = min(pressure, na.rm = TRUE),
    max = max(pressure, na.rm = TRUE),
    median = median(pressure, na.rm = TRUE),
    .groups = "drop"
  )

pressure_stats_combined

library(readr)

write_csv(pressure_stats_combined,
          "~/Desktop/Cicada/Tennessee R/CicTen/pressure_stats_combined.csv")


#### labels as A and B and to the left for paper ####

library(ggplot2)
library(dplyr)
library(patchwork)

# Color theme
cols_all <- palette.colors(12, palette = "Paired")

bg_cols <- c(
  "morning" = cols_all[5],
  "noon"    = cols_all[9],
  "evening" = cols_all[3],
  "night"   = cols_all[1]
)

# Re-create time_posix safely
pressure_summary_all <- pressure_summary_all %>%
  mutate(
    time_posix = as.POSIXct(time, format = "%H:%M")
  )

# Background windows for A
time_blocks_A <- data.frame(
  period = c("night", "morning", "noon", "evening", "night"),
  xmin = as.POSIXct(c("05:00", "07:00", "10:00", "17:00", "20:00"), format = "%H:%M"),
  xmax = as.POSIXct(c("07:00", "10:00", "17:00", "20:00", "21:00"), format = "%H:%M"),
  ymin = -Inf,
  ymax = Inf
)

# Background windows for B
time_blocks_B <- data.frame(
  period = c("morning", "noon", "evening"),
  xmin = as.POSIXct(c("08:00", "12:00", "18:00"), format = "%H:%M"),
  xmax = as.POSIXct(c("11:00", "17:00", "20:00"), format = "%H:%M"),
  ymin = -Inf,
  ymax = Inf
)

make_pressure_plot <- function(blocks, panel_label, x_label = NULL) {
  
  ggplot() +
    geom_rect(
      data = blocks,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = period),
      alpha = 0.25,          # ← ADD THIS (main fix)
      color = NA,            # ← ensure no borders at all
      inherit.aes = FALSE,
      show.legend = FALSE
    )+
    geom_line(
      data = pressure_summary_all,
      aes(x = time_posix, y = mean_pressure),
      linewidth = 1,
      color = "black"
    ) +
    geom_point(
      data = pressure_summary_all,
      aes(x = time_posix, y = mean_pressure),
      size = 2.5,
      color = "black"
    ) +
    geom_errorbar(
      data = pressure_summary_all,
      aes(x = time_posix, ymin = min_pressure, ymax = max_pressure),
      width = 0.05,
      color = "black",
      linewidth = 0.5
    ) +
    scale_x_datetime(
      date_breaks = "2 hours",
      date_labels = "%H:%M",
      limits = as.POSIXct(c("05:00", "21:00"), format = "%H:%M"),
      expand = c(0, 0)
    ) +
    scale_y_reverse(
      breaks = seq(0, -20, by = -2)
    ) +
    scale_fill_manual(values = bg_cols) +
    labs(
      title = panel_label,
      x = x_label,
      y = "Xylem Pressure (bar)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}

pA <- make_pressure_plot(time_blocks_A, "A", x_label = NULL)
pB <- make_pressure_plot(time_blocks_B, "B", x_label = "Time")

pA

pB

pA / pB

#### Plot with horizontal timewindow blocks ####

library(ggplot2)
library(dplyr)

# Color theme
cols_all <- palette.colors(12, palette = "Paired")

bg_cols <- c(
  "Morning"   = cols_all[5],
  "Afternoon" = cols_all[9],
  "Evening"   = cols_all[3],
  "Night"     = cols_all[1]
)

# Convert pressure from bar → MPa
pressure_summary_all_mpa <- pressure_summary_all %>%
  mutate(
    mean_pressure_mpa = mean_pressure * 0.1,
    min_pressure_mpa  = min_pressure * 0.1,
    max_pressure_mpa  = max_pressure * 0.1
  )

# Time blocks
time_blocks_A <- data.frame(
  time_window = c("Night", "Morning", "Afternoon", "Evening", "Night"),
  xmin = as.POSIXct(c("05:00", "07:00", "10:00", "17:00", "20:00"), format = "%H:%M"),
  xmax = as.POSIXct(c("07:00", "10:00", "17:00", "20:00", "21:00"), format = "%H:%M")
)

time_blocks_B <- data.frame(
  time_window = c("Morning", "Afternoon", "Evening"),
  xmin = as.POSIXct(c("08:00", "12:00", "18:00"), format = "%H:%M"),
  xmax = as.POSIXct(c("11:00", "17:00", "20:00"), format = "%H:%M")
)

# Combine blocks
time_blocks_combined <- bind_rows(
  time_blocks_A %>% mutate(species_group = "Dec + Dem"),
  time_blocks_B %>% mutate(species_group = "Cas")
) %>%
  mutate(
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    
    label = case_when(
      species_group == "Cas" & time_window == "Evening" ~ "Eve",
      TRUE ~ as.character(time_window)
    ),
    
    ymin = case_when(
      species_group == "Dec + Dem" ~ -1.98,
      species_group == "Cas"       ~ -1.84
    ),
    
    ymax = case_when(
      species_group == "Dec + Dem" ~ -1.87,
      species_group == "Cas"       ~ -1.73
    ),
    
    y_label = (ymin + ymax) / 2,
    x_label = xmin + (xmax - xmin) / 2
  )

# Plot
p_xylem <- ggplot() +
  
  geom_rect(
    data = time_blocks_combined,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = time_window
    ),
    color = "black",
    linewidth = 0.4,
    alpha = 0.55
  ) +
  
  geom_text(
    data = time_blocks_combined,
    aes(x = x_label, y = y_label, label = label),
    size = 3
  ) +
  
  geom_line(
    data = pressure_summary_all_mpa,
    aes(x = time_posix, y = mean_pressure_mpa),
    linewidth = 1,
    color = "black"
  ) +
  
  geom_point(
    data = pressure_summary_all_mpa,
    aes(x = time_posix, y = mean_pressure_mpa),
    size = 2,
    color = "black"
  ) +
  
  geom_errorbar(
    data = pressure_summary_all_mpa,
    aes(
      x = time_posix,
      ymin = min_pressure_mpa,
      ymax = max_pressure_mpa
    ),
    width = 0.05,
    color = "black"
  ) +
  
  annotate(
    "text",
    x = as.POSIXct("21:20", format = "%H:%M"),
    y = -1.925,
    label = "Dec + Dem",
    hjust = 0,
    size = 3.5
  ) +
  
  annotate(
    "text",
    x = as.POSIXct("21:20", format = "%H:%M"),
    y = -1.785,
    label = "Cas",
    hjust = 0,
    size = 3.5
  ) +
  
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M"
  ) +
  
  scale_y_reverse(
    breaks = seq(0, -2.0, by = -0.2)
  ) +
  
  coord_cartesian(
    xlim = as.POSIXct(c("05:00", "21:00"), format = "%H:%M"),
    ylim = c(0, -2.0),
    clip = "off"
  ) +
  
  scale_fill_manual(
    values = bg_cols,
    drop = FALSE
  ) +
  
  labs(
    x = "Time",
    y = "Xylem Pressure (MPa)"
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    plot.margin = margin(5.5, 90, 5.5, 5.5)
  )

p_xylem

#### Statistics for pressure ####

library(dplyr)

pressure_window_stats <- pressure_tidy %>%
  mutate(
    time_parsed = as.POSIXct(time, format = "%H:%M"),
    hour = as.numeric(format(time_parsed, "%H")) +
      as.numeric(format(time_parsed, "%M")) / 60,
    
    period = case_when(
      hour >= 7  & hour < 10 ~ "morning",
      hour >= 10 & hour < 17 ~ "afternoon",
      hour >= 17 & hour < 20 ~ "evening",
      hour >= 20 | hour < 7  ~ "night",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # First calculate one mean for each sampling time
  group_by(period, time) %>%
  summarise(
    time_mean_pressure = mean(pressure, na.rm = TRUE),
    n_measurements = n(),
    .groups = "drop"
  ) %>%
  
  # Then summarize the time-point means within each period
  group_by(period) %>%
  summarise(
    n_timepoints = n(),
    n_measurements = sum(n_measurements),
    mean_pressure = mean(time_mean_pressure, na.rm = TRUE),
    sd = sd(time_mean_pressure, na.rm = TRUE),
    se = sd / sqrt(n_timepoints),
    ci_lower = mean_pressure -
      qt(0.975, df = n_timepoints - 1) * se,
    ci_upper = mean_pressure +
      qt(0.975, df = n_timepoints - 1) * se,
    min = min(time_mean_pressure, na.rm = TRUE),
    max = max(time_mean_pressure, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(
      period,
      levels = c("morning", "afternoon", "evening", "night")
    )
  ) %>%
  arrange(period)

pressure_window_stats
#> Mean of time point measurements
#>  period    n_timepoints   n_measurements  mean_pressure    sd    se     ci_lower  ci_upper    min    max
#<fct>            <int>          <int>         <dbl>        <dbl>   <dbl>    <dbl>    <dbl>    <dbl>  <dbl>
#1 morning              3             36         -5.87      2.72    1.57    -12.6     0.894    -8.81  -3.43
#2 afternoon            7             51        -12.3       1.13    0.425   -13.4    -11.3     -13.7  -10.4 
#3 evening              3             27         -8.37      2.44    1.41    -14.4    -2.32    -10.7   -5.87
#4 night                3             15         -2.43      1.16.   0.667    -5.30    0.442    -3.76  -1.63

#### Putiing it into 24h to match temperature ####

library(ggplot2)
library(dplyr)
library(lubridate)

# Color theme
cols_all <- palette.colors(12, palette = "Paired")

bg_cols <- c(
  "Morning"   = cols_all[5],
  "Afternoon" = cols_all[9],
  "Evening"   = cols_all[3],
  "Night"     = cols_all[1]
)

# Dummy date for 24 h axis
dummy_date <- "2000-01-01"

# Convert pressure from bar → MPa
pressure_summary_all_mpa <- pressure_summary_all %>%
  mutate(
    mean_pressure_mpa = mean_pressure * 0.1,
    min_pressure_mpa  = min_pressure * 0.1,
    max_pressure_mpa  = max_pressure * 0.1,
    
    time_only = format(time_posix, "%H:%M"),
    time_posix_24h = as.POSIXct(
      paste(dummy_date, time_only),
      format = "%Y-%m-%d %H:%M"
    )
  )

# Time blocks A: Dec + Dem
time_blocks_A <- data.frame(
  time_window = c("Night", "Morning", "Afternoon", "Evening", "Night"),
  xmin = as.POSIXct(
    paste(dummy_date, c("00:00", "07:00", "10:00", "17:00", "20:00")),
    format = "%Y-%m-%d %H:%M"
  ),
  xmax = as.POSIXct(
    c(
      paste(dummy_date, c("07:00", "10:00", "17:00", "20:00")),
      paste("2000-01-02", "00:00")
    ),
    format = "%Y-%m-%d %H:%M"
  )
)

# Time blocks B: Cas
time_blocks_B <- data.frame(
  time_window = c("Morning", "Afternoon", "Evening"),
  xmin = as.POSIXct(
    paste(dummy_date, c("08:00", "12:00", "18:00")),
    format = "%Y-%m-%d %H:%M"
  ),
  xmax = as.POSIXct(
    paste(dummy_date, c("11:00", "17:00", "20:00")),
    format = "%Y-%m-%d %H:%M"
  )
)

# Combine blocks
time_blocks_combined <- bind_rows(
  time_blocks_A %>% mutate(species_group = "Dec + Dem"),
  time_blocks_B %>% mutate(species_group = "Cas")
) %>%
  mutate(
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    
    label = case_when(
      species_group == "Cas" & time_window == "Evening" ~ "Eve",
      TRUE ~ as.character(time_window)
    ),
    
    ymin = case_when(
      species_group == "Dec + Dem" ~ -2.05,
      species_group == "Cas"       ~ -1.83
    ),
    
    ymax = case_when(
      species_group == "Dec + Dem" ~ -1.85,
      species_group == "Cas"       ~ -1.68
    ),
    
    y_label = (ymin + ymax) / 2,
    x_label = xmin + (xmax - xmin) / 2
  )

# Plot
p_xylem_24h <- ggplot() +
  
  geom_rect(
    data = time_blocks_combined,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = time_window
    ),
    color = "black",
    linewidth = 0.4,
    alpha = 0.55
  ) +
  
  geom_text(
    data = time_blocks_combined,
    aes(x = x_label, y = y_label, label = label),
    size = 3
  ) +
  
  geom_line(
    data = pressure_summary_all_mpa,
    aes(x = time_posix_24h, y = mean_pressure_mpa),
    linewidth = 1,
    color = "black"
  ) +
  
  geom_point(
    data = pressure_summary_all_mpa,
    aes(x = time_posix_24h, y = mean_pressure_mpa),
    size = 2,
    color = "black"
  ) +
  
  geom_errorbar(
    data = pressure_summary_all_mpa,
    aes(
      x = time_posix_24h,
      ymin = min_pressure_mpa,
      ymax = max_pressure_mpa
    ),
    width = 20 * 60,
    color = "black"
  ) +
  
  annotate(
    "text",
    x = as.POSIXct(paste("2000-01-02", "00:20"), format = "%Y-%m-%d %H:%M"),
    y = -1.925,
    label = "Dec + Dem",
    hjust = 0,
    size = 3.5
  ) +
  
  annotate(
    "text",
    x = as.POSIXct(paste("2000-01-02", "00:20"), format = "%Y-%m-%d %H:%M"),
    y = -1.785,
    label = "Cas",
    hjust = 0,
    size = 3.5
  ) +
  
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    limits = as.POSIXct(
      c(
        paste(dummy_date, "00:00"),
        paste("2000-01-02", "01:00")
      ),
      format = "%Y-%m-%d %H:%M"
    ),
    expand = c(0, 0)
  ) +
  
  scale_y_reverse(
    breaks = seq(0, -2.0, by = -0.2)
  ) +
  
  coord_cartesian(
    ylim = c(0, -2.0),
    clip = "off"
  ) +
  
  scale_fill_manual(
    values = bg_cols,
    drop = FALSE
  ) +
  
  labs(
    x = "Time of day",
    y = "Xylem Pressure (MPa)"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "none",
    
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    
    panel.grid = element_blank(),   # ← removes all grid lines
    
    plot.margin = margin(5.5, 90, 5.5, 5.5)
  )

p_xylem_24h
####___________________________________________________________####
#### Temperature data ####

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Load data 

weather_data <- read_delim(
  "data/Weather data.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(
    `Wireless ET Station: B: Temperature` = col_number()
  ),
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE
)

# Rename, convert Fahrenheit to Celsius, filter dates 

weather_data <- weather_data %>%
  rename(
    datetime = Timestamp,
    temperature_F = `Wireless ET Station: B: Temperature`
  ) %>%
  mutate(
    datetime = dmy_hm(datetime),
    temperature_C = (temperature_F - 32) * 5 / 9
  ) %>%
  filter(
    datetime >= as.POSIXct("2025-05-20 00:00"),
    datetime <  as.POSIXct("2025-05-23 00:00")
  )

# Color theme 

cols_all <- palette.colors(12, palette = "Paired")

bg_cols <- c(
  "Morning"   = cols_all[5],
  "Afternoon" = cols_all[9],
  "Evening"   = cols_all[3],
  "Night"     = cols_all[1]
)

# Collapse all dates onto one 24 h day 

dummy_date <- "2000-01-01"

temperature_summary <- weather_data %>%
  filter(!is.na(datetime), !is.na(temperature_C)) %>%
  mutate(
    hour_only = floor_date(datetime, unit = "hour"),
    time_only = format(hour_only, "%H:00"),
    time_posix = as.POSIXct(
      paste(dummy_date, time_only),
      format = "%Y-%m-%d %H:%M"
    )
  ) %>%
  group_by(time_posix) %>%
  summarise(
    mean_temperature = mean(temperature_C, na.rm = TRUE),
    min_temperature  = min(temperature_C, na.rm = TRUE),
    max_temperature  = max(temperature_C, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Time blocks
# Dec + Dem: full 24 h with Night split
# Cas: only its active time windows

time_blocks_A <- data.frame(
  time_window = c("Night", "Morning", "Afternoon", "Evening", "Night"),
  xmin = as.POSIXct(
    paste(dummy_date, c("00:00", "08:00", "12:00", "17:00", "20:00")),
    format = "%Y-%m-%d %H:%M"
  ),
  xmax = as.POSIXct(
    c(
      paste(dummy_date, c("08:00", "12:00", "17:00", "20:00")),
      paste("2000-01-02", "00:00")
    ),
    format = "%Y-%m-%d %H:%M"
  )
)

time_blocks_B <- data.frame(
  time_window = c("Morning", "Afternoon", "Evening"),
  xmin = as.POSIXct(
    paste(dummy_date, c("08:00", "12:00", "18:00")),
    format = "%Y-%m-%d %H:%M"
  ),
  xmax = as.POSIXct(
    paste(dummy_date, c("11:00", "17:00", "20:00")),
    format = "%Y-%m-%d %H:%M"
  )
)

# Combine two block rows 

y_min_plot <- min(temperature_summary$min_temperature, na.rm = TRUE)

time_blocks_combined <- bind_rows(
  time_blocks_A %>% mutate(species_group = "Dec + Dem"),
  time_blocks_B %>% mutate(species_group = "Cas")
) %>%
  mutate(
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    
    ymin = case_when(
      species_group == "Dec + Dem" ~ y_min_plot - 5.2,
      species_group == "Cas"       ~ y_min_plot - 3.0
    ),
    
    ymax = case_when(
      species_group == "Dec + Dem" ~ y_min_plot - 3.6,
      species_group == "Cas"       ~ y_min_plot - 1.4
    ),
    
    y_label = (ymin + ymax) / 2,
    x_label = xmin + (xmax - xmin) / 2
  )

# Plot 

temperature_plot <- ggplot() +
  
  # Time blocks
  geom_rect(
    data = time_blocks_combined,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = time_window
    ),
    color = "black",
    linewidth = 0.4,
    alpha = 0.55
  ) +
  
  # Labels inside blocks
  geom_text(
    data = time_blocks_combined,
    aes(
      x = x_label,
      y = y_label,
      label = time_window
    ),
    size = 3
  ) +
  
  # Temperature line
  geom_line(
    data = temperature_summary,
    aes(x = time_posix, y = mean_temperature),
    linewidth = 1,
    color = "black"
  ) +
  
  geom_point(
    data = temperature_summary,
    aes(x = time_posix, y = mean_temperature),
    size = 2.3,
    color = "black"
  ) +
  
  # Hourly min/max error bars
  geom_errorbar(
    data = temperature_summary,
    aes(
      x = time_posix,
      ymin = min_temperature,
      ymax = max_temperature
    ),
    width = 20 * 60,
    color = "black",
    linewidth = 0.5
  ) +
  
  # Species labels outside plot
  annotate(
    "text",
    x = as.POSIXct(paste("2000-01-02", "00:20"), format = "%Y-%m-%d %H:%M"),
    y = y_min_plot - 4.4,
    label = "Dec + Dem",
    hjust = 0,
    size = 3.5
  ) +
  
  annotate(
    "text",
    x = as.POSIXct(paste("2000-01-02", "00:20"), format = "%Y-%m-%d %H:%M"),
    y = y_min_plot - 2.2,
    label = "Cas",
    hjust = 0,
    size = 3.5
  ) +
  
  scale_fill_manual(values = bg_cols) +
  
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    limits = as.POSIXct(
      c(
        paste(dummy_date, "00:00"),
        paste("2000-01-02", "01:00")
      ),
      format = "%Y-%m-%d %H:%M"
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Time of day",
    y = "Temperature (°C)"
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic() +
  theme(
    legend.position = "none",
    
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    
    panel.grid = element_blank(),   # ← removes all grid lines
    
    plot.margin = margin(5.5, 90, 5.5, 5.5)
  )

temperature_plot

#### without block for combination ####

temperature_plot_no_blocks <- ggplot() +
  
  geom_line(
    data = temperature_summary,
    aes(x = time_posix, y = mean_temperature),
    linewidth = 1,
    color = "black"
  ) +
  
  geom_point(
    data = temperature_summary,
    aes(x = time_posix, y = mean_temperature),
    size = 2.3,
    color = "black"
  ) +
  
  geom_errorbar(
    data = temperature_summary,
    aes(
      x = time_posix,
      ymin = min_temperature,
      ymax = max_temperature
    ),
    width = 20 * 60,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    limits = as.POSIXct(
      c(
        paste(dummy_date, "00:00"),
        paste("2000-01-02", "01:00")
      ),
      format = "%Y-%m-%d %H:%M"
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    x = NULL,
    y = "Temperature (°C)"
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank()
  )

temperature_plot_no_blocks
####____________________________________________________________####
#### Combining Xylem pressure and Temperature into AB ####

library(patchwork)


combined_temp_xylem <- temperature_plot_no_blocks / p_xylem_24h +
  plot_layout(heights = c(1, 1.15)) +
  plot_annotation(
    tag_levels = "A",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(size = 16, face = "bold")
  )
combined_temp_xylem

    