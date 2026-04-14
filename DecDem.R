library(readr)
CicTen <- read_delim("data/CicTen.csv", delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE)
View(CicTen)


# Color theme for this paper: 
cols_all <- palette.colors(12, palette = "Paired")

barplot(rep(1, length(cols_all)),
        col = cols_all,
        names.arg = 1:length(cols_all),
        border = NA,
        main = "Paired palette (indexed)")

#### CicTen Data Tidying Script ####

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(hms)

# IDs to remove
remove_ids <- c(
  24,
  25,
  30,
  31,
  39:42,
  44,
  47, 48,
  63,
  84:86,
  112,
  117:120,
  122,
  137, 138,
  145,
  148, 149,
  151,
  161, 162
)

cicten_tidy <- CicTen %>%
  
  # clean column names
  clean_names() %>%
  
  # trim whitespace
  mutate(across(where(is.character), str_squish)) %>%
  
  # make sure number is truly numeric
  mutate(
    number = parse_number(as.character(number))
  ) %>%
  
  # convert weights to numeric
  mutate(
    across(
      starts_with("weight"),
      ~ as.numeric(str_replace(as.character(.), ",", "."))
    )
  ) %>%
  
  # keep only relevant rows
  filter(
    species %in% c("Dec", "Dem"),
    purpose == "Excretion"
  ) %>%
  
  # remove biologically wrong individuals
  filter(!number %in% remove_ids) %>%
  
  # remove unused columns
  select(
    -notes,
    -pressure_bomb_bar
  ) %>%
  
  # rename date/time columns
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
  
  # realign weights to match events
  mutate(
    weight_5 = weight_4,
    weight_4 = weight_3,
    weight_3 = weight_2,
    weight_2 = NA_real_
  ) %>%
  
  # pivot to long format
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
  
  mutate(event = as.integer(event)) %>%
  
  filter(!(is.na(action) & is.na(date) & is.na(time) & is.na(weight))) %>%
  
  arrange(number, event) %>%
  
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

# checking for number 24
cicten_tidy %>%
  filter(number %in% remove_ids) %>%
  distinct(number)

24 %in% cicten_tidy$number

write_csv(cicten_tidy, "cicten_tidy.csv")

#### CicTen Input Rate + Plots ####

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(tibble)

# 1. Final summary per cicada

final_cicten <- cicten_tidy %>%
  mutate(
    datetime = as.POSIXct(
      paste(date, time),
      format = "%d.%m.%y %H:%M"
    )
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
    
    duration_to_pee_mins = as.numeric(
      difftime(first_pee_datetime, lab_datetime, units = "mins")
    ),
    
    duration_to_end_mins = as.numeric(
      difftime(final_bodyweight_datetime, lab_datetime, units = "mins")
    ),
    
    duration_hours = duration_to_pee_mins / 60,
    
    input_g_hr = case_when(
      is.na(duration_hours) ~ NA_real_,
      duration_hours <= 0 ~ NA_real_,
      TRUE ~ input / duration_hours
    )
  )

final_cicten

# Optional save
write_csv(final_cicten, "final_cicten.csv")


#### 2. Add time-window grouping for plotting ####

plot_data <- final_cicten %>%
  mutate(
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
    
    x_cat = case_when(
      time_window == "Morning" ~ 1,
      time_window == "Afternoon" ~ 2,
      time_window == "Evening" ~ 3,
      time_window == "Night" ~ 4
    ),
    
    x_species = x_cat + ifelse(species == "Dec", -0.15, 0.15)
  ) %>%
  filter(!is.na(input_g_hr))

plot_data

#### 3. Check species differences within each time window ####

library(dplyr)
library(ggplot2)
library(ggpubr)

# make sure species order is consistent
plot_data_species <- plot_data %>%
  mutate(
    species = factor(species, levels = c("Dec", "Dem")),
    time_window = factor(time_window,
                         levels = c("Morning", "Afternoon", "Evening", "Night"))
  )

# test Dec vs Dem separately within each time window
species_tests <- plot_data_species %>%
  group_by(time_window) %>%
  summarise(
    p_value = tryCatch(
      wilcox.test(input_g_hr ~ species)$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

species_tests
# A tibble: 4 × 2
#time_window p_value
#<fct>         <dbl>
#1 Morning       0.8  
#2 Afternoon     0.894
#3 Evening       0.937
#4 Night         0.107

#### Figure: species comparison within each time window ####

fig_species_by_time <- ggplot(
  plot_data_species,
  aes(x = species, y = input_g_hr)
) +
  
  geom_violin(
    aes(fill = time_window),
    alpha = 0.45,
    color = NA,
    trim = FALSE
  ) +
  
  geom_jitter(
    aes(color = time_window, shape = species),
    width = 0.08,
    size = 2.6,
    alpha = 0.9
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3.5,
    color = "black"
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.12,
    color = "black",
    linewidth = 0.5
  ) +
  
  facet_wrap(~ time_window, nrow = 2) +
  
  stat_compare_means(
    comparisons = list(c("Dec", "Dem")),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Dec" = 16,
      "Dem" = 17
    )
  ) +
  
  labs(
    x = "Species",
    y = "Input rate (g/hr)"
  ) +
  
  guides(
    color = "none",
    fill = "none",
    shape = "none"
  ) +
  
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

fig_species_by_time

#### 3.1. fully separate plots #####

plot_morning   <- plot_data_species %>% filter(time_window == "Morning")
plot_afternoon <- plot_data_species %>% filter(time_window == "Afternoon")
plot_evening   <- plot_data_species %>% filter(time_window == "Evening")
plot_night     <- plot_data_species %>% filter(time_window == "Night")

make_plot <- function(data, title_label, fill_col, line_col) {
  ggplot(data, aes(x = species, y = input_g_hr)) +
    
    geom_violin(fill = fill_col, alpha = 0.45, color = NA, trim = FALSE) +
    
    geom_jitter(
      aes(shape = species),
      width = 0.08,
      size = 2.6,
      alpha = 0.9,
      color = line_col
    ) +
    
    stat_summary(
      fun = mean,
      geom = "point",
      size = 3.5,
      color = "black"
    ) +
    
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.12,
      color = "black"
    ) +
    
    stat_compare_means(
      comparisons = list(c("Dec", "Dem")),
      method = "wilcox.test",
      label = "p.signif"
    ) +
    
    scale_shape_manual(values = c("Dec" = 16, "Dem" = 17)) +
    
    labs(
      title = title_label,
      x = "Species",
      y = "Input rate (g/hr)"
    ) +
    
    theme_classic() +
    theme(legend.position = "none")
}

pA <- make_plot(plot_morning,   "A) Morning",   cols_all[7], cols_all[8])
pB <- make_plot(plot_afternoon, "B) Afternoon", cols_all[5], cols_all[6])
pC <- make_plot(plot_evening,   "C) Evening",   cols_all[3], cols_all[4])
pD <- make_plot(plot_night,     "D) Night",     cols_all[1], cols_all[2])

# each as a single plot
pA
pB
pC
pD

# combining them into one plot 
library(patchwork)

(pA | pB) / (pC | pD)

#### 4. Figure 1: raw points + mean ± SE ####

fig1 <- ggplot(plot_data, aes(x = x_cat, y = input_g_hr)) +
  
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
  
  geom_point(
    aes(
      x = x_species,
      color = time_window,
      shape = species
    ),
    size = 3,
    alpha = 0.9
  ) +
  
  stat_summary(
    aes(
      x = x_species,
      group = interaction(time_window, species)
    ),
    fun = mean,
    geom = "point",
    size = 4,
    color = "black"
  ) +
  
  stat_summary(
    aes(
      x = x_species,
      group = interaction(time_window, species)
    ),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.06,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c("Morning", "Afternoon", "Evening", "Night"),
    expand = c(0, 0)
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Dec" = 16,
      "Dem" = 17
    )
  ) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)",
    shape = "Species"
  ) +
  
  guides(
    color = "none",
    fill = "none",
    shape = guide_legend(title = "Species")
  ) +
  
  theme_classic() +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank()
  )

fig1

#### 4.1. Figure 1: combining Dec and Dem ####

fig1_pooled <- ggplot(plot_data, aes(x = x_cat, y = input_g_hr)) +
  
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
  
  geom_jitter(
    aes(color = time_window),
    width = 0.12,
    size = 3,
    alpha = 0.9
  ) +
  
  stat_summary(
    aes(group = time_window),
    fun = mean,
    geom = "point",
    size = 4,
    color = "black"
  ) +
  
  stat_summary(
    aes(group = time_window),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.08,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c("Morning", "Afternoon", "Evening", "Night"),
    expand = c(0, 0)
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)"
  ) +
  
  guides(
    color = "none",
    fill = "none"
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  
  coord_cartesian(ylim = c(0, NA)) +
  
  theme_classic()

fig1_pooled

#### 5. Figure 2: violin + raw points + mean ± SE ####

library(ggpubr)

fig2 <- ggplot(plot_data, aes(x = time_window, y = input_g_hr)) +
  
  geom_violin(
    aes(fill = time_window),
    alpha = 0.5,
    color = NA,
    trim = FALSE
  ) +
  
  geom_point(
    aes(
      x = as.numeric(time_window) + ifelse(species == "Dec", -0.12, 0.12),
      color = time_window,
      shape = species
    ),
    size = 2.8,
    alpha = 0.9
  ) +
  
  stat_summary(
    aes(
      x = as.numeric(time_window) + ifelse(species == "Dec", -0.12, 0.12),
      group = interaction(time_window, species)
    ),
    fun = mean,
    geom = "point",
    size = 3.5,
    color = "black"
  ) +
  
  stat_summary(
    aes(
      x = as.numeric(time_window) + ifelse(species == "Dec", -0.12, 0.12),
      group = interaction(time_window, species)
    ),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.05,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Dec" = 16,
      "Dem" = 17
    )
  ) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)",
    shape = "Species"
  ) +
  
  guides(
    color = "none",
    fill = "none",
    shape = guide_legend(title = "Species")
  ) +
  
  theme_classic() +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank()
  )

fig2

#### 5.1. Figure 2: violin with combination of Dec and Dem ####
library(ggpubr)
library(ggplot2)

fig2_pooled <- ggplot(plot_data, aes(x = time_window, y = input_g_hr)) +
  
  geom_violin(
    aes(fill = time_window),
    alpha = 0.5,
    color = NA,
    trim = FALSE
  ) +
  
  geom_jitter(
    aes(color = time_window),
    width = 0.12,
    size = 2.8,
    alpha = 0.9
  ) +
  
  stat_summary(
    aes(group = time_window),
    fun = mean,
    geom = "point",
    size = 3.5,
    color = "black"
  ) +
  
  stat_summary(
    aes(group = time_window),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.08,
    color = "black",
    linewidth = 0.5
  ) +
  
  stat_compare_means(
    comparisons = list(
      c("Morning", "Night"),
      c("Evening", "Night")
    ),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = TRUE,
    label.y = c(
      max(plot_data$input_g_hr, na.rm = TRUE) * 1.05,
      max(plot_data$input_g_hr, na.rm = TRUE) * 1.12
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)"
  ) +
  
  guides(
    color = "none",
    fill = "none"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.12))
  ) +
  
  theme_classic()

fig2_pooled

#### Statistic tests ####

#Summary
plot_data %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE)
  )


# by species 
plot_data %>%
  group_by(species) %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE),
    .groups = "drop"
  )

# by time window 
plot_data %>%
  group_by(time_window) %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE),
    .groups = "drop"
  )

# Anova
model <- lm(input_g_hr ~ species * time_window, data = plot_data)
anova(model)

summary(model)


library(emmeans)
emmeans(model, pairwise ~ species | time_window)


library(effectsize)
eta_squared(model)



kruskal.test(input_g_hr ~ time_window, data = plot_data)


#----------------------------------------------------------------
#### CicTen Input Rate + Plots - WITHOUT NEG ####

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(tibble)
library(ggpubr)

# 1. Final summary per cicada

final_cicten_pos <- cicten_tidy %>%
  mutate(
    datetime = as.POSIXct(
      paste(date, time),
      format = "%d.%m.%y %H:%M"
    )
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
    
    duration_to_pee_mins = as.numeric(
      difftime(first_pee_datetime, lab_datetime, units = "mins")
    ),
    
    duration_hours = duration_to_pee_mins / 60,
    
    input_g_hr = case_when(
      is.na(duration_hours) ~ NA_real_,
      duration_hours <= 0 ~ NA_real_,
      TRUE ~ input / duration_hours
    )
  ) %>%
  
  # 🔥 FILTER DIRECTLY HERE
  filter(
    !is.na(input_g_hr),
    input_g_hr >= 0
  )

final_cicten_pos

# Optional save
write_csv(final_cicten_pos, "final_cicten_pos.csv")

#### 2. Add time-window grouping for plotting ####

# IMPORTANT:
# for plotting, keep only non-missing and non-negative input rates

plot_data_pos <- final_cicten_pos %>%
  mutate(
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
    
    x_cat = case_when(
      time_window == "Morning" ~ 1,
      time_window == "Afternoon" ~ 2,
      time_window == "Evening" ~ 3,
      time_window == "Night" ~ 4
    ),
    
    x_species = x_cat + ifelse(species == "Dec", -0.15, 0.15)
  )

plot_data_pos

#### 3. Check species differences within each time window ####

plot_data_species_pos <- plot_data_pos %>%
  mutate(
    species = factor(species, levels = c("Dec", "Dem")),
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    )
  )

species_tests_pos <- plot_data_species_pos %>%
  group_by(time_window) %>%
  summarise(
    p_value = tryCatch(
      wilcox.test(input_g_hr ~ species)$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

species_tests_pos
# A tibble: 4 × 2
#time_window p_value
#<fct>         <dbl>
#1 Morning       0.8  
#2 Afternoon     0.720
#3 Evening       0.352
#4 Night         0.167

#### Figure: species comparison within each time window ####

fig_species_by_time_pos <- ggplot(
  plot_data_species_pos,
  aes(x = species, y = input_g_hr)
) +
  
  geom_violin(
    aes(fill = time_window),
    alpha = 0.45,
    color = NA,
    trim = FALSE
  ) +
  
  geom_jitter(
    aes(color = time_window, shape = species),
    width = 0.08,
    size = 2.6,
    alpha = 0.9
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3.5,
    color = "black"
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.12,
    color = "black",
    linewidth = 0.5
  ) +
  
  facet_wrap(~ time_window, nrow = 2) +
  
  stat_compare_means(
    comparisons = list(c("Dec", "Dem")),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Dec" = 16,
      "Dem" = 17
    )
  ) +
  
  labs(
    x = "Species",
    y = "Input rate (g/hr)"
  ) +
  
  guides(
    color = "none",
    fill = "none",
    shape = "none"
  ) +
  
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

fig_species_by_time_pos


#### 3.1. Fully separate plots #####

plot_morning_pos   <- plot_data_species_pos %>% filter(time_window == "Morning")
plot_afternoon_pos <- plot_data_species_pos %>% filter(time_window == "Afternoon")
plot_evening_pos   <- plot_data_species_pos %>% filter(time_window == "Evening")
plot_night_pos     <- plot_data_species_pos %>% filter(time_window == "Night")

make_plot_pos <- function(data, title_label, fill_col, line_col) {
  ggplot(data, aes(x = species, y = input_g_hr)) +
    
    geom_violin(fill = fill_col, alpha = 0.45, color = NA, trim = FALSE) +
    
    geom_jitter(
      aes(shape = species),
      width = 0.08,
      size = 2.6,
      alpha = 0.9,
      color = line_col
    ) +
    
    stat_summary(
      fun = mean,
      geom = "point",
      size = 3.5,
      color = "black"
    ) +
    
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.12,
      color = "black"
    ) +
    
    stat_compare_means(
      comparisons = list(c("Dec", "Dem")),
      method = "wilcox.test",
      label = "p.signif"
    ) +
    
    scale_shape_manual(values = c("Dec" = 16, "Dem" = 17)) +
    
    labs(
      title = title_label,
      x = "Species",
      y = "Input rate (g/hr)"
    ) +
    
    theme_classic() +
    theme(legend.position = "none")
}

pApos <- make_plot_pos(plot_morning_pos,   "A) Morning",   cols_all[7], cols_all[8])
pBpos <- make_plot_pos(plot_afternoon_pos, "B) Afternoon", cols_all[5], cols_all[6])
pCpos <- make_plot_pos(plot_evening_pos,   "C) Evening",   cols_all[3], cols_all[4])
pDpos <- make_plot_pos(plot_night_pos,     "D) Night",     cols_all[1], cols_all[2])

pApos
pBpos
pCpos
pDpos

library(patchwork)
(pApos | pBpos) / (pCpos | pDpos)


#### 4. Figure 1: raw points + mean ± SE ####

fig1_pos <- ggplot(plot_data_pos, aes(x = x_cat, y = input_g_hr)) +
  
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
  
  geom_point(
    aes(
      x = x_species,
      color = time_window,
      shape = species
    ),
    size = 3,
    alpha = 0.9
  ) +
  
  stat_summary(
    aes(
      x = x_species,
      group = interaction(time_window, species)
    ),
    fun = mean,
    geom = "point",
    size = 4,
    color = "black"
  ) +
  
  stat_summary(
    aes(
      x = x_species,
      group = interaction(time_window, species)
    ),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.06,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c("Morning", "Afternoon", "Evening", "Night"),
    expand = c(0, 0)
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Dec" = 16,
      "Dem" = 17
    )
  ) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)",
    shape = "Species"
  ) +
  
  guides(
    color = "none",
    fill = "none",
    shape = guide_legend(title = "Species")
  ) +
  
  theme_classic() +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank()
  )

fig1_pos


#### 4.1. Figure 1: combining Dec and Dem ####

fig1_pos_pooled <- ggplot(plot_data, aes(x = x_cat, y = input_g_hr)) +
  
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
  
  geom_jitter(
    aes(color = time_window),
    width = 0.12,
    size = 3,
    alpha = 0.9
  ) +
  
  stat_summary(
    aes(group = time_window),
    fun = mean,
    geom = "point",
    size = 4,
    color = "black"
  ) +
  
  stat_summary(
    aes(group = time_window),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.08,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c("Morning", "Afternoon", "Evening", "Night"),
    expand = c(0, 0)
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)"
  ) +
  
  guides(
    color = "none",
    fill = "none"
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  
  theme_classic()

fig1_pos_pooled


#### 5. Figure 2: violin + raw points + mean ± SE ####

fig2_pos <- ggplot(plot_data, aes(x = time_window, y = input_g_hr)) +
  
  geom_violin(
    aes(fill = time_window),
    alpha = 0.5,
    color = NA,
    trim = FALSE
  ) +
  
  geom_point(
    aes(
      x = as.numeric(time_window) + ifelse(species == "Dec", -0.12, 0.12),
      color = time_window,
      shape = species
    ),
    size = 2.8,
    alpha = 0.9
  ) +
  
  stat_summary(
    aes(
      x = as.numeric(time_window) + ifelse(species == "Dec", -0.12, 0.12),
      group = interaction(time_window, species)
    ),
    fun = mean,
    geom = "point",
    size = 3.5,
    color = "black"
  ) +
  
  stat_summary(
    aes(
      x = as.numeric(time_window) + ifelse(species == "Dec", -0.12, 0.12),
      group = interaction(time_window, species)
    ),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.05,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Dec" = 16,
      "Dem" = 17
    )
  ) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)",
    shape = "Species"
  ) +
  
  guides(
    color = "none",
    fill = "none",
    shape = guide_legend(title = "Species")
  ) +
  
  theme_classic() +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank()
  )

fig2_pos


#### 5.1. Figure 2: violin with combination of Dec and Dem ####

fig2_pos_pooled <- ggplot(plot_data, aes(x = time_window, y = input_g_hr)) +
  
  geom_violin(
    aes(fill = time_window),
    alpha = 0.5,
    color = NA,
    trim = FALSE
  ) +
  
  geom_jitter(
    aes(color = time_window),
    width = 0.12,
    size = 2.8,
    alpha = 0.9
  ) +
  
  stat_summary(
    aes(group = time_window),
    fun = mean,
    geom = "point",
    size = 3.5,
    color = "black"
  ) +
  
  stat_summary(
    aes(group = time_window),
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.08,
    color = "black",
    linewidth = 0.5
  ) +
  
  stat_compare_means(
    comparisons = list(
      c("Morning", "Night"),
      c("Evening", "Night")
    ),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = TRUE,
    label.y = c(
      max(plot_data$input_g_hr, na.rm = TRUE) * 1.05,
      max(plot_data$input_g_hr, na.rm = TRUE) * 1.12
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[8],
      "Afternoon" = cols_all[6],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[7],
      "Afternoon" = cols_all[5],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    )
  ) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)"
  ) +
  
  guides(
    color = "none",
    fill = "none"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.12))
  ) +
  
  theme_classic()

fig2_pos_pooled

#### Statistics: POSITIVE #####

#Summary
plot_data_pos %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE)
  )
 
#by species 
plot_data_pos %>%
  group_by(species) %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE),
    .groups = "drop"
  )

# Anova
model <- lm(input_g_hr ~ species * time_window, data = plot_data_pos)
anova(model)

summary(model)

library(emmeans)
emmeans(model, pairwise ~ species | time_window)

library(effectsize)
eta_squared(model)

kruskal.test(input_g_hr ~ time_window, data = plot_data_pos)

#> tests what time windows differ 
#> important because Kruskal only says that it does 
#> install.packages("FSA")
library(FSA)
dunnTest(input_g_hr ~ time_window, data = plot_data_pos, method = "bh")
#> Evening - Night and Morning - Night are significant 
#> Afternoon - Night has a trend 
#> Post hoc Dunn tests with BH correction

#----------------------------------------------------------------
#### 1. Excretion probability ####

library(dplyr)
library(lubridate)
library(ggplot2)
library(emmeans)
library(pwr)
library(patchwork)

cols_all <- palette.colors(12, palette = "Paired")

time_cols <- c(
  "Morning"   = cols_all[7],
  "Afternoon" = cols_all[5],
  "Evening"   = cols_all[3],
  "Night"     = cols_all[1]
)

pee_data <- final_cicten %>%
  mutate(
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
    
    peed = ifelse(!is.na(total_pee) & total_pee > 0, 1, 0)
  )

#### 1: Excretion probability #####

excretion_rate <- pee_data %>%
  group_by(time_window) %>%
  summarise(
    n = n(),
    n_peed = sum(peed, na.rm = TRUE),
    prop = n_peed / n,
    .groups = "drop"
  )

prob_plot <- ggplot(excretion_rate, aes(x = time_window, y = prop, fill = time_window)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(
    aes(label = paste0(n_peed, "/", n)),
    vjust = -0.5,
    size = 5
  ) +
  scale_fill_manual(values = time_cols) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Time window",
    y = "Excretion probability"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )

prob_plot

#### 2: Logistic regression #####

#install.packages("logistf")
library(logistf)

glm_firth <- logistf(peed ~ time_window, data = pee_data)
summary(glm_firth)


# overall test for time window
anova(glm_pee, test = "Chisq")
#> p is significant 
#> there is an overall effect of time window on peeing probability


# pairwise comparisons
emmeans(glm_pee, pairwise ~ time_window, type = "response")

#### 3: Distribution + sample size #####

pee_data %>%
  group_by(time_window) %>%
  summarise(n = n(), .groups = "drop")

#### 4: Approximate power #####

pwr.anova.test(
  k = 4,
  n = 15,
  f = 0.25,
  sig.level = 0.05
)

pwr.anova.test(
  k = 4,
  f = 0.25,
  sig.level = 0.05,
  power = 0.8
)



#----------------------------------------------------------------
#### 1: comparing weight and g/hr

input_bw_data <- final_cicten_pos %>%
  select(number, species, sex, input_g_hr, lab_weight) %>%
  filter(!is.na(input_g_hr), !is.na(lab_weight))

input_bw_data <- input_bw_data %>%
  mutate(input_mass_specific = input_g_hr / lab_weight)

wilcox.test(input_mass_specific ~ species, data = input_bw_data)

#Wilcoxon rank sum exact test

#data:  input_mass_specific by species
#W = 489, p-value = 0.3664
#alternative hypothesis: true location shift is not equal to 0

model_bw <- lm(input_g_hr ~ lab_weight * species, data = input_bw_data)
summary(model_bw)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.031553 -0.017614 -0.006477  0.007912  0.144443 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)
#(Intercept)            0.03140    0.01914   1.641    0.107
#lab_weight            -0.01330    0.03311  -0.402    0.690 # Input does not scale with size in your dataset.
#speciesDem            -0.00549    0.02677  -0.205    0.838 # No baseline difference between species.
#lab_weight:speciesDem  0.02308    0.03897   0.592    0.556 # No difference in slope between species.

#Residual standard error: 0.03044 on 55 degrees of freedom
#Multiple R-squared:  0.03619,	Adjusted R-squared:  -0.01638 
#F-statistic: 0.6884 on 3 and 55 DF,  p-value: 0.563

cols_all <- palette.colors(12, palette = "Paired")

ggplot(input_bw_data, aes(x = lab_weight, y = input_g_hr, color = species)) +
  
  geom_point(
    size = 3,
    alpha = 0.85
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  
  scale_color_manual(
    values = c(
      "Dec" = cols_all[9],
      "Dem" = cols_all[12]
    )
  ) +
  
  labs(
    x = "Bodyweight (g)",
    y = "Input (g/hr)",
    color = "Species"
  ) +
  
  theme_classic()

# Despite differences in body size, 
#cicadas show no scaling of intake with mass and no species-specific intake strategy.
