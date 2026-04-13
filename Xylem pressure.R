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
  "morning" = paired[7],
  "afternoon" = paired[5],
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

#### creating A) and B) for DecDem and Cas timewindows ####

pressure_summary_all <- pressure_tidy %>%
  group_by(time) %>%
  summarise(
    mean_pressure = mean(pressure, na.rm = TRUE),
    min_pressure = min(pressure, na.rm = TRUE),
    max_pressure = max(pressure, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(time_posix = as.POSIXct(time, format = "%H:%M"))

# Duplicate same pressure data for panels A and B

pressure_summary_panels <- bind_rows(
  pressure_summary_all %>% mutate(panel = "M. decula and decim"),
  pressure_summary_all %>% mutate(panel = "M. cassini")
) %>%
  mutate(panel = factor(panel, levels = c("M. decula and decim", "M. cassini")))

time_blocks_all <- bind_rows(time_blocks_A, time_blocks_B) %>%
  mutate(panel = factor(panel, levels = c("M. decula and decim", "M. cassini")))

ggplot() +
  geom_rect(
    data = time_blocks_all,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = period),
    alpha = 0.2,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  
  geom_line(
    data = pressure_summary_panels,
    aes(x = time_posix, y = mean_pressure),
    linewidth = 1,
    color = "black"
  ) +
  
  geom_point(
    data = pressure_summary_panels,
    aes(x = time_posix, y = mean_pressure),
    color = "black"
  ) +
  
  geom_errorbar(
    data = pressure_summary_panels,
    aes(x = time_posix, ymin = min_pressure, ymax = max_pressure),
    width = 0.05,
    color = "black"
  ) +
  
  facet_wrap(~panel, ncol = 1) +
  
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M"
  ) +
  
  scale_y_reverse(
    breaks = seq(0, -20, by = -2)
  ) +
  
  scale_fill_manual(values = bg_cols) +
  
  labs(
    x = "Time",
    y = "Xylem Pressure (bar)"
  ) +
  
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12)
  )

#### labels as A and B and to the left for paper ####

library(ggplot2)
library(dplyr)
library(patchwork)

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

# Plot A

pA <- ggplot() +
  geom_rect(
    data = time_blocks_A,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = period),
    alpha = 0.2,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_line(
    data = pressure_summary_all,
    aes(x = time_posix, y = mean_pressure),
    linewidth = 1,
    color = "black"
  ) +
  geom_point(
    data = pressure_summary_all,
    aes(x = time_posix, y = mean_pressure),
    color = "black"
  ) +
  geom_errorbar(
    data = pressure_summary_all,
    aes(x = time_posix, ymin = min_pressure, ymax = max_pressure),
    width = 0.05,
    color = "black"
  ) +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    limits = as.POSIXct(c("05:00", "21:00"), format = "%H:%M")
  ) +
  scale_y_reverse(
    breaks = seq(0, -20, by = -2)
  ) +
  scale_fill_manual(values = bg_cols) +
  labs(
    title = "A",
    x = NULL,
    y = "Xylem Pressure (bar)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.title.position = "plot"
  )

# Plot B

pB <- ggplot() +
  geom_rect(
    data = time_blocks_B,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = period),
    alpha = 0.2,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_line(
    data = pressure_summary_all,
    aes(x = time_posix, y = mean_pressure),
    linewidth = 1,
    color = "black"
  ) +
  geom_point(
    data = pressure_summary_all,
    aes(x = time_posix, y = mean_pressure),
    color = "black"
  ) +
  geom_errorbar(
    data = pressure_summary_all,
    aes(x = time_posix, ymin = min_pressure, ymax = max_pressure),
    width = 0.05,
    color = "black"
  ) +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    limits = as.POSIXct(c("05:00", "21:00"), format = "%H:%M")
  ) +
  scale_y_reverse(
    breaks = seq(0, -20, by = -2)
  ) +
  scale_fill_manual(values = bg_cols) +
  labs(
    title = "B",
    x = "Time",
    y = "Xylem Pressure (bar)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.title.position = "plot"
  )

# Combine vertically

pA / pB
