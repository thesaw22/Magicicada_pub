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

##### Combining time and Input #####

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


library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)

# prepare plotting data
plot_data <- final_cicten %>%
  mutate(
    duration_hours = duration_to_pee_mins / 60,
    input_g_hr = case_when(
      is.na(duration_hours) ~ NA_real_,
      duration_hours <= 0 ~ NA_real_,
      TRUE ~ input / duration_hours
    ),
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
    )
  )

# plot
ggplot(plot_data, aes(x = x_cat, y = input_g_hr)) +
  
  # background bands
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
  
  # data points: Dec left, Dem right
  geom_point(
    aes(
      x = x_cat + ifelse(species == "Dec", -0.15, 0.15),
      color = time_window,
      shape = species
    ),
    size = 3,
    alpha = 0.9
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
    y = "Input (g/hr)"
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

##### Adding spice to the plot ####
library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)

plot_data <- final_cicten %>%
  mutate(
    duration_hours = duration_to_pee_mins / 60,
    input_g_hr = case_when(
      is.na(duration_hours) ~ NA_real_,
      duration_hours <= 0 ~ NA_real_,
      TRUE ~ input / duration_hours
    ),
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

## Figure with mean and SE

fig1 <- ggplot(plot_data, aes(x = x_cat, y = input_g_hr)) +
  
  # background bands
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
  
  # raw data
  geom_point(
    aes(
      x = x_species,
      color = time_window,
      shape = species
    ),
    size = 3,
    alpha = 0.9
  ) +
  
  # mean point
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
  
  # SE bars
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
    y = "Input (g/hr)",
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

## Figure 2: Violoin for distribution 

install.packages("ggpubr")  
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
      color = time_window,
      shape = species,
      x = as.numeric(time_window) + ifelse(species == "Dec", -0.12, 0.12)
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
    y = "Input (g/hr)",
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

fig2 +
  stat_compare_means(
    comparisons = list(
      c("Morning", "Night"),
      c("Evening", "Night")
    ),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = TRUE,
    label.y = c(-0.075, -0.09)   # bottom placement
  )


##### Statistics ####

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
#n      min_input   max_input mean_input sd_input
#<int>     <dbl>     <dbl>      <dbl>    <dbl>
# 84   -0.0238      0.176     0.0219    0.0295

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
#1 Dec        42   -0.0238     0.133     0.0188   0.0242
#2 Dem        42   -0.0118     0.176     0.0251   0.0341

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
#1 Morning        14  -0.0118     0.0267    0.00804  0.00929
#2 Afternoon      28  -0.0238     0.0876    0.0206   0.0255 
#3 Evening        16  -0.00224    0.0779    0.0115   0.0205 
#4 Night          26  -0.00478    0.176     0.0373   0.0384 

# Anova
model <- lm(input_g_hr ~ species * time_window, data = plot_data)
anova(model)

#Analysis of Variance Table

#Response: input_g_hr
#Df   Sum Sq   Mean Sq F value   Pr(>F)   
#species              1 0.000846 0.0008464  1.0661 0.305109   
#time_window          3 0.010642 0.0035472  4.4680 0.006062 **
#species:time_window  3 0.000632 0.0002108  0.2655 0.850043   
#Residuals           76 0.060338 0.0007939                    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(model)
#Call:
#lm(formula = input_g_hr ~ species * time_window, data = plot_data)

#Residuals:
#Min        1Q    Median        3Q       Max 
#-0.048641 -0.013489 -0.005458  0.005444  0.132093 

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)                      0.0097152  0.0099619   0.975   0.3325  
#speciesDem                      -0.0039094  0.0152170  -0.257   0.7979  
#time_windowAfternoon             0.0062584  0.0128608   0.487   0.6279  
#time_windowEvening              -0.0001122  0.0140882  -0.008   0.9937  
#time_windowNight                 0.0218755  0.0124879   1.752   0.0839 .
#speciesDem:time_windowAfternoon  0.0120605  0.0186370   0.647   0.5195  
#speciesDem:time_windowEvening    0.0078004  0.0207373   0.376   0.7079  
#speciesDem:time_windowNight      0.0161814  0.0188262   0.860   0.3928  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.02818 on 76 degrees of freedom
#Multiple R-squared:  0.1673,	Adjusted R-squared:  0.09058 
#F-statistic: 2.181 on 7 and 76 DF,  p-value: 0.04516

#significance between time windows 
# mo significance in the species 
# more test for time window 

library(emmeans)

#emmeans(model, pairwise ~ time_window)

#$emmeans
#time_window  emmean      SE df lower.CL upper.CL
#Morning     0.00776 0.00761 76 -0.00739   0.0229
#Afternoon   0.02005 0.00538 76  0.00933   0.0308
#Evening     0.01155 0.00704 76 -0.00248   0.0256
#Night       0.03773 0.00554 76  0.02669   0.0488

#Results are averaged over the levels of: species 
#Confidence level used: 0.95 

#$contrasts
#contrast            estimate      SE df t.ratio p.value
#Morning - Afternoon -0.01229 0.00932 76  -1.319  0.5540
#Morning - Evening   -0.00379 0.01040 76  -0.365  0.9832
#Morning - Night     -0.02997 0.00941 76  -3.183  0.0111
#Afternoon - Evening  0.00850 0.00886 76   0.959  0.7730
#Afternoon - Night   -0.01768 0.00772 76  -2.289  0.1097
#Evening - Night     -0.02618 0.00896 76  -2.921  0.0233

#Results are averaged over the levels of: species 
#P value adjustment: tukey method for comparing a family of 4 estimates 

# species witihn each t ime window 
emmeans(model, pairwise ~ species | time_window)

#$emmeans
#time_window = Morning:
#species  emmean      SE df  lower.CL upper.CL
#Dec     0.00972 0.00996 76 -0.010126   0.0296
#Dem     0.00581 0.01150 76 -0.017104   0.0287

#time_window = Afternoon:
#species  emmean      SE df  lower.CL upper.CL
#Dec     0.01597 0.00813 76 -0.000226   0.0322
#Dem     0.02412 0.00704 76  0.010095   0.0382

#time_window = Evening:
#species  emmean      SE df  lower.CL upper.CL
#Dec     0.00960 0.00996 76 -0.010238   0.0294
#Dem     0.01349 0.00996 76 -0.006347   0.0333

#time_window = Night:
#species  emmean      SE df  lower.CL upper.CL
#Dec     0.03159 0.00753 76  0.016592   0.0466
#Dem     0.04386 0.00813 76  0.027663   0.0601

#Confidence level used: 0.95 

#$contrasts
#time_window = Morning:
#contrast  estimate     SE df t.ratio p.value
#Dec - Dem  0.00391 0.0152 76   0.257  0.7979

#time_window = Afternoon:
#contrast  estimate     SE df t.ratio p.value
#Dec - Dem -0.00815 0.0108 76  -0.758  0.4511

#time_window = Evening:
#contrast  estimate     SE df t.ratio p.value
#Dec - Dem -0.00389 0.0141 76  -0.276  0.7832

#time_window = Night:
#contrast  estimate     SE df t.ratio p.value
#Dec - Dem -0.01227 0.0111 76  -1.107  0.2717

# checking model assumption 
library(effectsize)

eta_squared(model)

# Effect Size for ANOVA (Type I)

#Parameter           | Eta2 (partial) |       95% CI
#---
#species             |           0.01 | [0.00, 1.00]
#time_window         |           0.15 | [0.03, 1.00]
#species:time_window |           0.01 | [0.00, 1.00]

#- One-sided CIs: upper bound fixed at [1.00].

# non paraametic confirmation 
kruskal.test(input_g_hr ~ time_window, data = plot_data)

#Kruskal-Wallis rank sum test

#data:  input_g_hr by time_window
#Kruskal-Wallis chi-squared = 16.466, df = 3, p-value = 0.0009098


