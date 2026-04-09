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

##### CicTen Data Tidying Script ####

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

##### CicTen Input Rate + Plots ####

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

##### 3. Check species differences within each time window ####

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

##### Figure: species comparison within each time window ####

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

##### 3.1. fully separate plots #####

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

##### Statistic tests ####

#Summary
plot_data %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE)
  )

# A tibble: 1 × 5
#n     min_input   max_input   mean_input  sd_input
#<int>     <dbl>     <dbl>      <dbl>       <dbl>
#69    -0.0238       0.176     0.0250      0.0312


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

# A tibble: 2 × 6
#species     n min_input max_input mean_input sd_input
#<chr>   <int>     <dbl>     <dbl>      <dbl>    <dbl>
#1 Dec        36  -0.0238      0.133     0.0212   0.0253
#2 Dem        33  -0.00825     0.176     0.0292   0.0365

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

# A tibble: 4 × 6
#time_window     n min_input max_input mean_input sd_input
#<fct>       <int>     <dbl>     <dbl>      <dbl>    <dbl>
#1 Morning         6   0.00234    0.0267     0.0120  0.00930
#2 Afternoon      26  -0.0238     0.0876     0.0196  0.0259 
#3 Evening        12  -0.00224    0.0779     0.0144  0.0231 
#4 Night          25  -0.00249    0.176      0.0389  0.0382 

# Anova
model <- lm(input_g_hr ~ species * time_window, data = plot_data)
anova(model)
#Analysis of Variance Table

#Response: input_g_hr
#Df   Sum Sq    Mean Sq F value  Pr(>F)  
#species              1 0.001112 0.00111177  1.2087 0.27591  
#time_window          3 0.008219 0.00273973  2.9785 0.03833 *
#species:time_window  3 0.000698 0.00023272  0.2530 0.85890  
#Residuals           61 0.056109 0.00091983                  
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(model)
#Call:
#lm(formula = input_g_hr ~ species * time_window, data = plot_data)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.039809 -0.016367 -0.007473  0.008080  0.127671 

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)                      1.299e-02  1.516e-02   0.856    0.395
#speciesDem                      -2.955e-03  2.627e-02  -0.113    0.911
#time_windowAfternoon             2.987e-03  1.751e-02   0.171    0.865
#time_windowEvening              -7.867e-05  1.958e-02  -0.004    0.997
#time_windowNight                 1.860e-02  1.719e-02   1.082    0.284
#speciesDem:time_windowAfternoon  9.762e-03  2.885e-02   0.338    0.736
#speciesDem:time_windowEvening    5.847e-03  3.157e-02   0.185    0.854
#speciesDem:time_windowNight      1.965e-02  2.897e-02   0.678    0.500

#Residual standard error: 0.03033 on 61 degrees of freedom
#Multiple R-squared:  0.1516,	Adjusted R-squared:  0.05429 
#F-statistic: 1.558 on 7 and 61 DF,  p-value: 0.1654

library(emmeans)
emmeans(model, pairwise ~ species | time_window)
#$emmeans
#time_window = Morning:
#species emmean      SE df lower.CL upper.CL
#Dec     0.0130 0.01520 61 -0.01734   0.0433
#Dem     0.0100 0.02140 61 -0.03285   0.0529

#time_window = Afternoon:
#species emmean      SE df lower.CL upper.CL
#Dec     0.0160 0.00876 61 -0.00153   0.0335
#Dem     0.0228 0.00811 61  0.00657   0.0390

#time_window = Evening:
#species emmean      SE df lower.CL upper.CL
#Dec     0.0129 0.01240 61 -0.01185   0.0377
#Dem     0.0158 0.01240 61 -0.00896   0.0406

#time_window = Night:
#species emmean      SE df lower.CL upper.CL
#Dec     0.0316 0.00811 61  0.01538   0.0478
#Dem     0.0483 0.00914 61  0.03000   0.0666

#Confidence level used: 0.95 

#$contrasts
#time_window = Morning:
#contrast  estimate     SE df t.ratio p.value
#Dec - Dem  0.00296 0.0263 61   0.113  0.9108

#time_window = Afternoon:
#contrast  estimate     SE df t.ratio p.value
#Dec - Dem -0.00681 0.0119 61  -0.570  0.5704

#time_window = Evening:
#contrast  estimate     SE df t.ratio p.value
#Dec - Dem -0.00289 0.0175 61  -0.165  0.8694

#time_window = Night:
#contrast  estimate     SE df t.ratio p.value
#Dec - Dem -0.01669 0.0122 61  -1.366  0.1769

library(effectsize)
eta_squared(model)

# Effect Size for ANOVA (Type I)
#Parameter           | Eta2 (partial) |       95% CI
---------------------------------------------------
#species             |           0.02 | [0.00, 1.00]
#time_window         |           0.13 | [0.00, 1.00]
#species:time_window |           0.01 | [0.00, 1.00]

#- One-sided CIs: upper bound fixed at [1.00].

kruskal.test(input_g_hr ~ time_window, data = plot_data)

#Kruskal-Wallis rank sum test

#data:  input_g_hr by time_window
#Kruskal-Wallis chi-squared = 12.453, df = 3, p-value = 0.005983

#----------------------------------------------------------------
##### CicTen Input Rate + Plots - WITHOUT NEG ####

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
    
    duration_to_end_mins = as.numeric(
      difftime(final_bodyweight_datetime, lab_datetime, units = "mins")
    ),
    
    duration_hours = duration_to_pee_mins / 60,
    
    input_g_hr = case_when(
      is.na(duration_hours) ~ NA_real_,
      duration_hours <= 0 ~ NA_real_,
      TRUE ~ input / duration_hours
    ),
    
    # flag negative input rates
    negative_input_rate = !is.na(input_g_hr) & input_g_hr < 0
  )

final_cicten_pos

# optional: inspect which individuals have negative rates
negative_cases <- final_cicten_pos %>%
  filter(negative_input_rate) %>%
  select(
    number, species, sex,
    lab_weight, total_pee, final_bodyweight,
    input, duration_hours, input_g_hr
  )

negative_cases

# Optional save
write_csv(final_cicten_pos, "final_cicten_pos.csv")


#### 2. Add time-window grouping for plotting ####

# IMPORTANT:
# for plotting, keep only non-missing and non-negative input rates
plot_data_pos <- final_cicten_pos %>%
  filter(!is.na(input_g_hr), input_g_hr >= 0) %>%
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

# optional check: how many negative values were excluded?
final_cicten_pos %>%
  summarise(
    total_n = n(),
    negative_n = sum(negative_input_rate, na.rm = TRUE),
    plotted_n = sum(!is.na(input_g_hr) & input_g_hr >= 0, na.rm = TRUE)
  )


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
#2 Afternoon     0.912
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
