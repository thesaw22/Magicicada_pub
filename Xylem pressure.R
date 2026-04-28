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
#> aslo changing the numbers into negative -> negative pressure 

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
    aes(x = x_label, y = y_label, label = time_window),
    size = 3
  ) +
  
  # Xylem pressure
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
  
  # Species labels OUTSIDE plot
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
  
  # Axes
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M"
  ) +
  
  scale_y_reverse(
    breaks = seq(0, -2.0, by = -0.2)
  ) +
  
  # Zoom without removing annotations
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
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin = margin(5.5, 90, 5.5, 5.5)
  )

p_xylem
