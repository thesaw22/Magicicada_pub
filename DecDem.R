library(readr)
CicTen <- read_delim("CicTen.csv", delim = ";", 
                     escape_double = FALSE, col_types = cols(Number = col_number(), 
                                                             `time of first action` = col_time(format = "%H:%M")), 
                     trim_ws = TRUE)
View(CicTen)

# Color theme for this paper: 
cols_all <- palette.colors(12, palette = "Paired")

barplot(rep(1, length(cols_all)),
        col = cols_all,
        names.arg = 1:length(cols_all),
        border = NA,
        main = "Paired palette (indexed)")

#> This needs a lot of tidyness:
#>  keep only species in Dec, Dem
#>  keep only purpose == "Excretion"
#>  drop pressure bomb and notes
#>  preserve the event-to-weight mapping:
#>      action 1 ↔ weight 1
#>      action 2 ↔ no weight
#>      action 3 ↔ weight 2
#>      action 4 ↔ weight 3
#>      action 5 ↔ weight 4

# ============================================================
##### CicTen Data Tidying Script ####
# ============================================================

# Load packages ------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(hms)

# Tidy data ----------------------------------------------------

cicten_tidy <- CicTen %>%
  
  # clean column names
  clean_names() %>%
  
  # trim whitespace
  mutate(across(where(is.character), str_squish)) %>%
  
  # convert weights to numeric (handle commas)
  mutate(
    across(starts_with("weight"),
           ~ as.numeric(str_replace(as.character(.), ",", ".")))
  ) %>%
  
  # keep only relevant rows
  filter(
    species %in% c("Dec", "Dem"),
    purpose == "Excretion"
  ) %>%
  
  # remove unused columns
  select(
    -notes,
    -pressure_bomb_bar
  ) %>%
  
  # rename date/time columns for consistency
  rename(
    date_1 = date_of_action,
    date_2 = date_of_second_action,
    date_3 = date_of_third_action,
    date_4 = date_of_fourth_action,
    date_5 = date_of_fith_action,
    
    time_1 = time_of_first_action,
    time_2 = time_of_second_action,
    time_3 = time_of_third_action,
    time_4 = time_of_fourth_action,
    time_5 = time_of_fith_action
  ) %>%
  
  # realign weights to match event numbers
  mutate(
    weight_5 = weight_4,
    weight_4 = weight_3,
    weight_3 = weight_2,
    weight_2 = NA_real_
  ) %>%
  
  # pivot into long format (events stay grouped correctly)
  pivot_longer(
    cols = c(
      action_1, date_1, time_1, weight_1,
      action_2, date_2, time_2, weight_2,
      action_3, date_3, time_3, weight_3,
      action_4, date_4, time_4, weight_4,
      action_5, date_5, time_5, weight_5
    ),
    names_to = c(".value", "event"),
    names_pattern = "(action|date|time|weight)_(\\d)"
  ) %>%
  
  # clean event index
  mutate(event = as.integer(event)) %>%
  
  # remove empty rows
  filter(!(is.na(action) & is.na(date) & is.na(time) & is.na(weight))) %>%
  
  # order rows
  arrange(number, event) %>%
  
  # optional: reorder columns nicely
  select(
    number,
    species,
    sex,
    purpose,
    sampling_date,
    event,
    action,
    date,
    time,
    weight,
    everything()
  )

# View result --------------------------------------------------

View(cicten_tidy)

# Quick check --------------------------------------------------

cicten_tidy %>%
  select(number, event, action, date, time, weight) %>%
  print(n = 50)

# Optional: save to file --------------------------------------

write_csv(cicten_tidy, "cicten_tidy.csv")

##### Calculating the total Input ####

# Input=(total pee mass+final bodyweight)−lab weight

# Summerizing per cicada
input_summary <- cicten_tidy %>%
  group_by(number, species, sex) %>%
  summarise(
    
    lab_weight = first(weight[action == "lab"]),
    
    total_pee = sum(weight[action == "pee"], na.rm = TRUE),
    
    final_bodyweight = last(weight[action == "bodyweight"]),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    input = (total_pee + final_bodyweight) - lab_weight
  )

input_summary

##### Combining tiem and Input #####

time_summary <- cicten_tidy %>%
  group_by(number, species, sex) %>%
  summarise(
    lab_time = first(time[action == "lab"]),
    first_pee_time = first(time[action == "pee"]),
    final_bodyweight_time = last(time[action == "bodyweight"]),
    .groups = "drop"
  )

input_with_time <- input_summary %>%
  left_join(time_summary, by = c("number", "species", "sex"))

input_with_time


##### final summary of data ##### 
# includes input, time. and duration
final_cicten <- cicten_tidy %>%
  mutate(
    datetime = as.POSIXct(paste(date, time), format = "%d.%m.%Y %H:%M:%S")
  ) %>%
  group_by(number, species, sex) %>%
  summarise(
    lab_weight = first(weight[action == "lab"]),
    total_pee = sum(weight[action == "pee"], na.rm = TRUE),
    final_bodyweight = last(weight[action == "bodyweight"]),
    lab_datetime = first(datetime[action == "lab"]),
    first_pee_datetime = first(datetime[action == "pee"]),
    final_bodyweight_datetime = last(datetime[action == "bodyweight"]),
    .groups = "drop"
  ) %>%
  mutate(
    input = (total_pee + final_bodyweight) - lab_weight,
    duration_to_pee_mins = as.numeric(difftime(first_pee_datetime, lab_datetime, units = "mins")),
    duration_to_end_mins = as.numeric(difftime(final_bodyweight_datetime, lab_datetime, units = "mins"))
  )

final_cicten

write_csv(final_cicten,
          "~/Desktop/Cicada/Tennessee R/CicTen/final_cicten.csv")

##### Plotting #####
library(dplyr)
library(ggplot2)
library(lubridate)

plot_data <- final_cicten %>%
  mutate(
    duration_hours = duration_to_pee_mins / 60,
    input_g_hr = input / duration_hours,
    
    lab_hour = hour(lab_datetime),
    
    time_window = case_when(
      lab_hour >= 7  & lab_hour < 10 ~ "Morning",
      lab_hour >= 10 & lab_hour < 17 ~ "Afternoon",
      lab_hour >= 17 & lab_hour < 20 ~ "Evening",
      TRUE ~ "Night"
    ),
    
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    
    x_pos = case_when(
      time_window == "Morning" ~ 7,
      time_window == "Afternoon" ~ 10,
      time_window == "Evening" ~ 17,
      time_window == "Night" ~ 20
    )
  )


ggplot(plot_data, aes(x = x_pos, y = input_g_hr)) +
  
# BACKGROUND BANDS 
geom_rect(
  data = tibble(
    xmin = c(0.5, 1.5, 2.5, 3.5),
    xmax = c(1.5, 2.5, 3.5, 4.5),
    ymin = -Inf,
    ymax = Inf,
    time_window = c("Morning", "Afternoon", "Evening", "Night")
  ),
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = time_window),
  inherit.aes = FALSE,
  alpha = 0.2
) +
  
# DATA POINTS 
geom_jitter(
  aes(
    x = c(1, 2, 3, 4)[as.numeric(time_window)],
    color = time_window,
    shape = species
  ),
  width = 0.15,
  height = 0,
  size = 3,
  alpha = 0.9
) +
  
# AXIS 
scale_x_continuous(
  breaks = c(1, 2, 3, 4),
  labels = c("Morning", "Afternoon", "Evening", "Night"),
  expand = c(0, 0)
) +
  
# POINT COLORS 
scale_color_manual(
  values = c(
    "Morning" = cols_all[8],
    "Afternoon" = cols_all[6],
    "Evening" = cols_all[4],
    "Night" = cols_all[2]
  )
) +
  
# BAND COLORS 
scale_fill_manual(
  values = c(
    "Morning" = cols_all[7],
    "Afternoon" = cols_all[5],
    "Evening" = cols_all[3],
    "Night" = cols_all[1]
  )
) +
  
# SHAPES 
scale_shape_manual(
  values = c(
    "Dec" = 16,
    "Dem" = 17
  )
) +
  
  labs(
    x = "Time window",
    y = "Input (g/hr)",
    color = "Time window",
    shape = "Species"
  ) +
  
  guides(fill = "none") +
  theme_classic() +
  theme(legend.position = "none")
