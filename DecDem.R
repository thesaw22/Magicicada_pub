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
library(stringr)
library(janitor)
library(lubridate)

remove_ids <- c(
  24,
  25,
  30,
  31,
  39:44,
  47, 48,
  57:61,
  63,68,
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

#### Clean data ####

cicten_clean <- CicTen %>%
  clean_names() %>%
  
  mutate(
    across(where(is.character), str_squish)
  ) %>%
  
  mutate(
    across(where(is.character), ~ na_if(., ""))
  ) %>%
  
  mutate(
    across(where(is.character), ~ na_if(., "nan"))
  ) %>%
  
  mutate(
    number = parse_number(as.character(number))
  ) %>%
  
  mutate(
    across(
      starts_with("weight"),
      ~ as.numeric(str_replace(as.character(.), ",", "."))
    )
  ) %>%
  
  mutate(
    across(
      starts_with("action"),
      ~ str_to_lower(as.character(.))
    ),
    time_slot_of_third_action = str_to_lower(as.character(time_slot_of_third_action))
  ) %>%
  
  filter(
    species %in% c("Dec", "Dem"),
    purpose == "Excretion"
  ) %>%
  
  filter(!number %in% remove_ids)

#### Final summary per cicada ####
# Important structure in your sheet:
# weight_1 belongs to action_1
# weight_2 belongs to action_3
# weight_3 belongs to action_4
# weight_4 belongs to action_5

final_cicten <- cicten_clean %>%
  transmute(
    number,
    species,
    sex,
    sampling_date,
    
    lab_weight = weight_1,
    
    total_pee =
      coalesce(if_else(action_3 == "pee", weight_2, 0), 0) +
      coalesce(if_else(action_4 == "pee", weight_3, 0), 0) +
      coalesce(if_else(action_5 == "pee", weight_4, 0), 0),
    
    final_bodyweight = case_when(
      action_5 == "bodyweight" ~ weight_4,
      action_4 == "bodyweight" ~ weight_3,
      action_3 == "bodyweight" ~ weight_2,
      TRUE ~ NA_real_
    ),
    
    lab_datetime = as.POSIXct(
      paste(date_of_action, time_of_first_action),
      format = "%d.%m.%y %H:%M"
    ),
    
    third_datetime = as.POSIXct(
      paste(date_of_third_action, time_of_third_action),
      format = "%d.%m.%y %H:%M"
    ),
    
    time_window = case_when(
      time_slot_of_third_action == "morning" ~ "Morning",
      time_slot_of_third_action == "noon" ~ "Afternoon",
      time_slot_of_third_action == "evening" ~ "Evening",
      time_slot_of_third_action == "night" ~ "Night",
      TRUE ~ NA_character_
    )
  ) %>%
  
  mutate(
    input = (total_pee + final_bodyweight) - lab_weight,
    
    duration_to_third_mins = as.numeric(
      difftime(third_datetime, lab_datetime, units = "mins")
    ),
    
    duration_hours = duration_to_third_mins / 60,
    
    input_g_hr = case_when(
      is.na(duration_hours) ~ NA_real_,
      duration_hours <= 0 ~ NA_real_,
      is.na(final_bodyweight) ~ NA_real_,
      is.na(lab_weight) ~ NA_real_,
      TRUE ~ input / duration_hours
    ),
    
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    )
  )

final_cicten

#### Plot data ####

DecDem_ <- final_cicten %>%
  filter(!is.na(input_g_hr), !is.na(time_window))

DecDem_

#### Save as csv ####

write_csv(final_cicten, "final_cicten.csv")
write_csv(plot_data, "DecDem_.csv")

#### figure #####
library(ggplot2)
library(ggpubr)
library(dplyr)

fig_species_by_time <- ggplot(
  final_cicten %>% filter(!is.na(input_g_hr), !is.na(time_window)),
  aes(x = species, y = input_g_hr)
) +
  geom_violin(
    aes(fill = time_window),
    alpha = 0.45,
    color = NA,
    trim = FALSE
  ) +
  geom_jitter(
    aes(color = time_window),
    width = 0.08,
    size = 2.6,
    alpha = 0.9
  ) +
  stat_compare_means(
    comparisons = list(c("Dec", "Dem")),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = FALSE
  ) +
  facet_wrap(~time_window, nrow = 2, drop = FALSE) +
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    ),
    drop = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    ),
    drop = FALSE
  ) +
  labs(
    x = "Species",
    y = "Input rate (g/hr)"
  ) +
  guides(
    fill = "none",
    color = "none"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

fig_species_by_time


#### 3.1. fully separate plots #####

library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)

plot_morning <- DecDem_ %>% filter(time_window == "Morning")
plot_noon    <- DecDem_ %>% filter(time_window == "Afternoon")
plot_evening <- DecDem_ %>% filter(time_window == "Evening")
plot_night   <- DecDem_ %>% filter(time_window == "Night")

y_range <- range(DecDem_$input_g_hr, na.rm = TRUE)
y_limits <- y_range + c(-0.1, 0.1) * diff(y_range)

make_plot <- function(data, title_label, fill_col, line_col, y_limits) {
  ggplot(data, aes(x = species, y = input_g_hr)) +
    
    geom_violin(
      fill = fill_col,
      alpha = 0.45,
      color = NA,
      trim = FALSE
    ) +
    
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
    
   # stat_summary(
    #  fun.data = mean_se,
     # geom = "errorbar",
      #width = 0.12,
      #color = "black"
    #) +
    
    #stat_compare_means(
     # comparisons = list(c("Dec", "Dem")),
      #method = "wilcox.test",
      #label = "p.signif"
    #) +
    
    scale_shape_manual(
      values = c("Dec" = 16, "Dem" = 17)
    ) +
    
    coord_cartesian(ylim = y_limits) +
    
    labs(
      title = title_label,
      x = "Species",
      y = "Input rate (g/hr)"
    ) +
    
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
}

pA <- make_plot(plot_morning, "A) Morning", cols_all[5], cols_all[6], y_limits)
pB <- make_plot(plot_noon,    "B) Afternoon", cols_all[9], cols_all[10], y_limits)
pC <- make_plot(plot_evening, "C) Evening", cols_all[3], cols_all[4], y_limits)
pD <- make_plot(plot_night,   "D) Night", cols_all[1], cols_all[2], y_limits)

pA
pB
pC
pD

(pA | pB) / (pC | pD)

#### 4. Figure 1: raw points + mean ± SE ####

plot_data_fig1 <- final_cicten %>%
  filter(!is.na(input_g_hr), !is.na(time_window)) %>%
  mutate(
    time_window = recode(as.character(time_window), "Noon" = "Afternoon"),
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

fig1 <- ggplot(plot_data_fig1, aes(x = x_cat, y = input_g_hr)) +
  
  geom_rect(
    data = tibble(
      xmin = c(0.5, 1.5, 2.5, 3.5),
      xmax = c(1.5, 2.5, 3.5, 4.5),
      ymin = -Inf,
      ymax = Inf,
      time_window = factor(
        c("Morning", "Afternoon", "Evening", "Night"),
        levels = c("Morning", "Afternoon", "Evening", "Night")
      )
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
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    ),
    drop = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    ),
    drop = FALSE
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

library(dplyr)
library(ggplot2)
library(tibble)

plot_data_pooled <- final_cicten %>%
  filter(!is.na(input_g_hr), !is.na(time_window)) %>%
  mutate(
    time_window = recode(as.character(time_window), "Noon" = "Afternoon"),
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

fig1_pooled <- ggplot(plot_data_pooled, aes(x = x_cat, y = input_g_hr)) +
  
  geom_rect(
    data = tibble(
      xmin = c(0.5, 1.5, 2.5, 3.5),
      xmax = c(1.5, 2.5, 3.5, 4.5),
      ymin = -Inf,
      ymax = Inf,
      time_window = factor(
        c("Morning", "Afternoon", "Evening", "Night"),
        levels = c("Morning", "Afternoon", "Evening", "Night")
      )
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
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    ),
    drop = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    ),
    drop = FALSE
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
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  
  coord_cartesian(ylim = c(0, NA)) +
  
  theme_classic()

fig1_pooled

#### 5. Figure 2: violin + raw points + mean ± SE ####

plot_data_fig2 <- final_cicten %>%
  filter(!is.na(input_g_hr), !is.na(time_window)) %>%
  mutate(
    time_window = recode(as.character(time_window), "Noon" = "Afternoon"),
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    )
  )

fig2 <- ggplot(plot_data_fig2, aes(x = time_window, y = input_g_hr)) +
  
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
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    ),
    drop = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    ),
    drop = FALSE
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
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

fig2

#### 5.1. Figure 2: violin with combination of Dec and Dem ####

plot_data_fig2_pooled <- final_cicten %>%
  filter(!is.na(input_g_hr), !is.na(time_window)) %>%
  mutate(
    time_window = recode(as.character(time_window), "Noon" = "Afternoon"),
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    )
  )

y_max <- max(plot_data_fig2_pooled$input_g_hr, na.rm = TRUE)

fig2_pooled <- ggplot(plot_data_fig2_pooled, aes(x = time_window, y = input_g_hr)) +
  
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
      c("Morning", "Afternoon")
    ),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = TRUE,
    label.y = c(
      y_max * 1.05,
      y_max * 1.12
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    ),
    drop = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
      "Evening" = cols_all[3],
      "Night" = cols_all[1]
    ),
    drop = FALSE
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

## 5.1.1 statistics to this 
pairwise.wilcox.test(
  x = plot_data_fig2_pooled$input_g_hr,
  g = plot_data_fig2_pooled$time_window,
  p.adjust.method = "BH"
)

#Afternoon x Morning: p = 0.022
#Night x Morning: p = 0.015

#### Statistic tests ####
library(dplyr)

## 1. Overall summary

DecDem_ %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE)
  )

# Interpretation:
# Across all Dec and Dem observations combined (n = 58),
# mean input rate was 0.0272 g/hr (SD = 0.0319),
# with values ranging from -0.0238 to 0.176 g/hr.
# This indicates substantial variability among observations,
# including some negative values.


## 2. Summary by species

DecDem_ %>%
  group_by(species) %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE),
    .groups = "drop"
  )

# Interpretation:
# Dec: mean input = 0.0237 g/hr (SD = 0.0262, n = 31)
# Dem: mean input = 0.0311 g/hr (SD = 0.0375, n = 27)
#
# Dem showed a somewhat higher average input rate than Dec,
# but variability was high in both species,
# suggesting that any species difference may be weak.


## 3. Summary by time window

DecDem_ %>%
  group_by(time_window) %>%
  summarise(
    n = n(),
    min_input = min(input_g_hr, na.rm = TRUE),
    max_input = max(input_g_hr, na.rm = TRUE),
    mean_input = mean(input_g_hr, na.rm = TRUE),
    sd_input = sd(input_g_hr, na.rm = TRUE),
    .groups = "drop"
  )

# Interpretation:
# Morning:   mean = 0.0389 g/hr (SD = 0.0382, n = 25)
# Afternoon: mean = 0.0120 g/hr (SD = 0.00930, n = 6)
# Evening:   mean = 0.0239 g/hr (SD = 0.0263, n = 15)
# Night:     mean = 0.0144 g/hr (SD = 0.0231, n = 12)
#
# Input appeared highest in the Morning and lowest in the Afternoon,
# with intermediate values in the Evening and Night.
# However, sample sizes were uneven, especially in the Afternoon (n = 6),
# so these patterns should be interpreted cautiously.


## 4. Linear model: species * time_window

model <- lm(input_g_hr ~ species * time_window, data = DecDem_)
anova(model)

# Interpretation of ANOVA:
# species: F = 0.803, p = 0.374
# → No significant main effect of species
# → Dec and Dem do not differ significantly in input rate overall
#
# time_window: F = 2.380, p = 0.080 
# → No statistically significant main effect of time window at alpha = 0.05
# → There is only a weak trend suggesting that input may vary across time
#
# species × time_window: F = 0.301, p = 0.825
# → No interaction between species and time window
# → The temporal pattern in input does not differ between Dec and Dem


## 5. Model coefficients

summary(model)

# Interpretation:
# The intercept (0.0316, p < 0.001) represents the estimated mean input rate
# for the reference category, i.e. Dec in the Morning.
#
# speciesDem: estimate = 0.0167, p = 0.193
# → Dem tends to have slightly higher Morning input than Dec,
#   but this difference is not significant
#
# time_windowAfternoon: estimate = -0.0186, p = 0.301
# → In Dec, Afternoon input tends to be lower than Morning,
#   but not significantly so
#
# time_windowEvening: estimate = -0.00814, p = 0.581
# → In Dec, Evening input does not differ significantly from Morning
#
# time_windowNight: estimate = -0.0187, p = 0.233
# → In Dec, Night input tends to be lower than Morning,
#   but not significantly so
#
# All interaction terms are non-significant (all p > 0.05)
# → The difference between Dec and Dem does not change across time windows


## 6. Model fit

# Residual standard error = 0.03141
# Multiple R-squared = 0.15
# Adjusted R-squared = 0.03137
# Overall F-statistic = 1.264, p = 0.287

# Interpretation:
# The model explains about 15% of the variance in input rate,
# but after accounting for the number of predictors,
# the adjusted R-squared is very low (0.03137).
#
# The overall model is not significant (p = 0.287),
# indicating that species, time window, and their interaction
# do not jointly explain a substantial amount of variation in input rate.


## 7. Final interpretation for paper

# Input rate did not differ significantly between species (Dec vs Dem),
# nor did it vary significantly across time windows.
# There was also no significant species × time_window interaction,
# indicating that temporal patterns were similar in both species.
#
# Although mean input appeared highest in the Morning,
# this trend was not statistically supported in the linear model.
# Overall, variation in input rate was high relative to the observed mean differences,
# and the model had low explanatory power.

## 8. Checking for sex difference 
model_sex <- lm(input_g_hr ~ species * time_window + sex, data = DecDem_)
anova(model_sex)

# Interpretation (ANOVA):
# species: F = 0.82, p = 0.369
# → No significant difference in input rate between Dec and Dem

# time_window: F = 2.44, p = 0.076
# → No statistically significant effect at alpha = 0.05,
#   but a trend suggesting input may vary across time

# sex: F = 2.03, p = 0.161
# → No significant difference between males and females

# species × time_window: F = 0.35, p = 0.787
# → No interaction
# → Temporal patterns do not differ between species


# Model coefficients

summary(model_sex)

# Interpretation:

# Intercept (0.0399, p < 0.001)
# → Estimated mean input for the reference group:
#    Dec females in the Morning

# speciesDem (0.0154, p = 0.224)
# → Dem tends to have slightly higher input than Dec in the Morning,
#   but this difference is not significant

# time_windowAfternoon (-0.0237, p = 0.193)
# → Input tends to be lower in the Afternoon compared to Morning (in Dec females),
#   but not significantly

# time_windowEvening (-0.0091, p = 0.532)
# → No difference between Evening and Morning

# time_windowNight (-0.0227, p = 0.147)
# → Input tends to be lower at Night compared to Morning,
#   but not significantly

# sexmale (-0.0129, p = 0.148)
# → Males tend to have slightly lower input than females,
#   but this difference is not statistically significant

# All interaction terms are non-significant
# → The effect of time does not differ between species


# Model fit

# Residual standard error = 0.03105
# Multiple R-squared = 0.186
# Adjusted R-squared = 0.053
# Overall model: F = 1.40, p = 0.219

# Interpretation:
# The model explains ~18.6% of the variance in input rate,
# but the adjusted R² is low (5.3%), indicating weak explanatory power.
#
# The overall model is not significant (p = 0.219),
# suggesting that species, time window, sex, and their interaction
# do not jointly explain a substantial portion of variation.


# Final interpretation (for paper)

# Input rate did not differ significantly between species or sexes,
# and there was no significant interaction between species and time window.
#
# There was a weak trend for input to vary across time windows,
# with lower values in the Afternoon and Night relative to Morning,
# but this pattern was not statistically significant.
#
# Overall, variation in input rate was high relative to the observed
# differences among groups, and the model explained little of the variance.


#>While the overall effect of time window was not statistically significant,
#>pairwise comparisons revealed a significant difference between Morning and Afternoon input rates.

#----------------------------------------------------------------
#### CicTen Input Rate + Plots - WITHOUT NEG ####

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(tibble)
library(ggpubr)

plot_data_pos <- final_cicten %>%
  filter(!is.na(input_g_hr), !is.na(time_window), input_g_hr >= 0) %>%
  mutate(
    x_cat = case_when(
      time_window == "Morning"   ~ 1,
      time_window == "Afternoon" ~ 2,
      time_window == "Evening"   ~ 3,
      time_window == "Night"     ~ 4
    ),
    
    x_species = x_cat + ifelse(species == "Dec", -0.15, 0.15)
  )

plot_data_pos

# Optional save
write_csv(plot_data_pos, "final_cicten_pos.csv")

#### Check species differences within each time window ####

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
#1 Morning       0.167
#2 Afternoon     0.8  
#3 Evening       0.429
#4 Night         0.352

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
  
  #stat_compare_means(
   # comparisons = list(c("Dec", "Dem")),
    #method = "wilcox.test",
    #label = "p.signif",
    #hide.ns = FALSE
  #) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
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

y_range <- range(plot_data_species_pos$input_g_hr, na.rm = TRUE)
y_limits <- y_range + c(-0.1, 0.1) * diff(y_range)

make_plot_pos <- function(data, title_label, fill_col, line_col, y_limits) {
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
    
    scale_shape_manual(values = c("Dec" = 16, "Dem" = 17)) +
    
    coord_cartesian(ylim = y_limits) +
    
    labs(
      title = title_label,
      x = "Species",
      y = "Input rate (g/hr)"
    ) +
    
    theme_classic() +
    theme(legend.position = "none")
}

pApos <- make_plot_pos(plot_morning_pos,   "A) Morning",   cols_all[5], cols_all[6], y_limits)
pBpos <- make_plot_pos(plot_afternoon_pos, "B) Afternoon", cols_all[9], cols_all[10], y_limits)
pCpos <- make_plot_pos(plot_evening_pos,   "C) Evening",   cols_all[3], cols_all[4], y_limits)
pDpos <- make_plot_pos(plot_night_pos,     "D) Night",     cols_all[1], cols_all[2], y_limits)

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
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
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

fig1_pos_pooled <- ggplot(plot_data_pos, aes(x = x_cat, y = input_g_hr)) +
  
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
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
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

fig2_pos <- ggplot(plot_data_pos, aes(x = time_window, y = input_g_hr)) +
  
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
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
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
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank()
  )

fig2_pos


#### 5.1. Figure 2: violin with combination of Dec and Dem ####

fig2_pos_pooled <- ggplot(plot_data_pos, aes(x = time_window, y = input_g_hr)) +
  
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
      c("Morning", "Afternoon"),
      c("Evening", "Afternoon"),
      c("Evening", "Night")
    ),
    method = "wilcox.test",
    label = "p.signif",
    hide.ns = TRUE,
    label.y = c(
      max(plot_data_pos$input_g_hr, na.rm = TRUE) * 1.05,
      max(plot_data_pos$input_g_hr, na.rm = TRUE) * 1.12,
      max(plot_data_pos$input_g_hr, na.rm = TRUE) * 1.19,
      max(plot_data_pos$input_g_hr, na.rm = TRUE) * 1.26
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Morning" = cols_all[6],
      "Afternoon" = cols_all[10],
      "Evening" = cols_all[4],
      "Night" = cols_all[2]
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Morning" = cols_all[5],
      "Afternoon" = cols_all[9],
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
    expand = expansion(mult = c(0.05, 0.20))
  ) +
  
  theme_classic()

fig2_pos_pooled

## Stats

kruskal_test <- kruskal.test(input_g_hr ~ time_window, data = plot_data_pos)
kruskal_test
#input(g/hr) bz time_window
# p = 0,00191

pairwise_tests <- pairwise.wilcox.test(
  plot_data_pos$input_g_hr,
  plot_data_pos$time_window,
  p.adjust.method = "BH"  # or "bonferroni"
)

pairwise_tests
# Morning vs Afternoon: p = 0,014
# Afternoon vs Evening: p = 0,014
# Morning vs Night: p = 0,014
# Evening vs Night: p = 0,019

#install.packages("rstatix")   # only once
library(rstatix)

pairwise_results <- plot_data_pos %>%
  pairwise_wilcox_test(
    input_g_hr ~ time_window,
    p.adjust.method = "BH"
  )

pairwise_results

#### Statistics: POSITIVE #####

library(dplyr)

## 1. Summary by species

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

# Interpretation:
# Dec mean input ≈ 0.0263 g/hr
# Dem mean input ≈ 0.0391 g/hr
#
# → Dem still shows a somewhat higher mean input rate than Dec (n = 29)
# → But variability is also high, especially in Dem (n = 22)
# → So the numerical difference is not necessarily a statistically reliable one


## 2. Linear model: species * time_window

model <- lm(input_g_hr ~ species * time_window, data = plot_data_pos)
anova(model)

# Interpretation of ANOVA:
# species: p = 0.144
# → No significant overall difference between Dec and Dem
#
# time_window: p = 0.111
#
# species:time_window: p = 0.95
# → No interaction
# → The time pattern does not differ between species


## 3. Model summary

summary(model)

# Interpretation:
# None of the individual coefficients are significant
# → This means no single contrast stands out strongly against the reference category
# → This is consistent with the ANOVA:
#    - weak species effect
#    - only a trend for time
#
# The Night coefficient is positive
# → suggests input tends to be higher at night,
#   but there is still too much variability for strong support in this linear model


## 4. Estimated marginal means:
#    compare species within each time window

library(emmeans)

emmeans(model, pairwise ~ species | time_window)

# Interpretation:
# Morning:    Dec vs Dem p = 0.267
# Afternoon:  Dec vs Dem p = 0.912
# Evening:    Dec vs Dem p = 0.554
# Night:      Dec vs Dem p = 0.559
#
# → No significant species differences in any time window
# → Dem is numerically higher in several windows, especially Night,
#   but not significantly so


## 5. Effect sizes


library(effectsize)
eta_squared(model)

# Interpretation:
# species ≈ 0.029  -> small effect
# time_window ≈ 0.121 -> moderate effect
# interaction ≈ 0.008 -> negligible effect
#
# → Time explains more variation than species
# → Species contributes little to variation in positive input rates


## 6. Non-parametric test across time windows

kruskal.test(input_g_hr ~ time_window, data = plot_data_pos)

# Interpretation:
# p = 0.0058
# → Strong evidence that input differs among time windows
# → This is more convincing than the linear model result
# → Likely because the data are not well-behaved for standard ANOVA assumptions
#
# Important:
# Kruskal-Wallis only tells us that at least one time window differs.
# It does NOT tell us which time windows differ from each other.


## 7. Dunn post-hoc test

# FSA::dunnTest() can fail when the data object is a tibble
# or when factor handling is not clean.
# So we create a plain data.frame and make sure time_window is a factor.

library(FSA)

dunn_data <- plot_data_pos %>%
  select(input_g_hr, time_window) %>%
  filter(!is.na(input_g_hr), !is.na(time_window)) %>%
  mutate(time_window = factor(time_window)) %>%
  as.data.frame()

FSA::dunnTest(input_g_hr ~ time_window, data = dunn_data, method = "bh")

#_> still error
# Interpretation:
# This post-hoc test will tell you WHICH time windows differ.
# Use these pairwise adjusted p-values to identify where the significant differences lie.
#
# Likely expectation from your summaries:
# → Night will differ from Morning and probably from Evening
# → Afternoon may be intermediate


## 8. Alternative if FSA still errors

# A very robust alternative is rstatix::dunn_test()

install.packages("rstatix")
library(rstatix)

plot_data_pos %>%
  filter(!is.na(input_g_hr), !is.na(time_window)) %>%
  dunn_test(input_g_hr ~ time_window, p.adjust.method = "BH")

# Pairwise comparisons between time windows (BH-adjusted)

# Morning vs Afternoon: p.adj = 0.258
# → Not significant
# → Input rates are similar between these periods

# Morning vs Evening: p.adj = 0.807
# → Not significant
# → No difference between early and mid-day input

# Morning vs Night: p.adj = 0.0248 *
# → Significant
# → Input at Night is higher than in the Morning

# Afternoon vs Evening: p.adj = 0.258
# → Not significant
# → These two intermediate periods are similar

# Afternoon vs Night: p.adj = 0.105
# → Not significant (trend only)
# → Suggests Night may be higher, but evidence is weak

# Evening vs Night: p.adj = 0.0248 *
# → Significant
# → Input at Night is higher than in the Evening


##  Final interpretation

# 1. Strongest pattern:
# → Night vs (Morning + Evening) is significantly different

# 2. Intermediate periods:
# → Afternoon and Evening are not clearly different from each other
# → Afternoon vs Night shows only a weak trend

# 3. Overall structure:
# Morning   → lowest input
# Afternoon → intermediate
# Evening   → intermediate
# Night     → highest input


## Biological conclusion

# → Input rate follows a diel pattern:
#    - Low in the morning
#    - Gradually increases
#    - Peaks at night

# → The significant contrasts (Night vs Morning, Night vs Evening)
#   identify Night as the key period driving the overall effect

# → Combined with previous results:
#    - Species (Dec vs Dem): NOT important
#    - Time of day: main driver of input variation

# Clean takeaway:
# → Feeding/input is primarily time-dependent, with a clear nocturnal peak


#----------------------------------------------------------------
#### 1. Excretion probability ####

library(dplyr)
library(ggplot2)
library(patchwork)

cols_all <- palette.colors(12, palette = "Paired")

time_cols <- c(
  "Morning"   = cols_all[5],
  "Afternoon" = cols_all[9],
  "Evening"   = cols_all[3],
  "Night"     = cols_all[1]
)

pee_data <- final_cicten %>%
  filter(!is.na(time_window)) %>%
  mutate(
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    peed = ifelse(!is.na(total_pee) & total_pee > 0, 1, 0)
  )

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

library(logistf)

## 1. Firth logistic regression

glm_firth <- logistf(peed ~ time_window, data = pee_data)
summary(glm_firth)

# Interpretation:
# This model tests whether the probability of peeing changes across time windows.
# Firth logistic regression is appropriate here because you likely have small sample sizes
# and possible separation issues (for example, Morning appears to have a probability of 1.0).

# Reference level:
# The intercept corresponds to the reference category, which is Morning
# (unless you changed the factor order).

# Intercept:
# coef = 2.565, p = 0.0135
# → In the Morning, the log-odds of peeing are significantly greater than zero
# → This corresponds to a very high peeing probability in the Morning

# Afternoon:
# coef = -2.242, p = 0.0590
# → Compared with Morning, peeing probability tends to be lower in the Afternoon
# → This is a trend, but not conventionally significant at p < 0.05

# Evening:
# coef = -2.565, p = 0.0387
# → Compared with Morning, peeing probability is significantly lower in the Evening

# Night:
# coef = -1.202, p = 0.374
# → Compared with Morning, Night is not significantly different

# Important:
# These coefficients compare each time window directly against Morning only.
# They do NOT test all pairwise differences between all time windows.


## 2. Overall model test

# Likelihood ratio test from logistf:
# LR test = 7.27 on 3 df, p = 0.0637

# Interpretation:
# The overall effect of time_window in the Firth model is borderline / trend-level
# → suggests some time dependence, but evidence is moderate rather than strong

# Wald test from logistf:
# Wald test = 11.83 on 3 df, p = 0.0080

# Interpretation:
# The Wald test suggests a significant overall effect of time_window
# → however, for small or sparse datasets, the likelihood ratio test is usually preferred
# → therefore, I would interpret the overall Firth result cautiously:
#    there is suggestive evidence that peeing probability changes with time



## 3. Standard logistic regression ANOVA

anova(glm_pee, test = "Chisq")

# Interpretation:
# Analysis of deviance gives p = 0.0247
# → In the standard binomial model, time_window significantly affects peeing probability
#
# Together with the Firth model:
# → both analyses point in the same direction:
#    peeing probability likely varies across time windows
# → but the exact strength of evidence depends on the method used


## 4. Pairwise comparisons using emmeans


emmeans(glm_pee, pairwise ~ time_window, type = "response")

# Estimated probabilities:
# Morning   = 1.000
# Afternoon = 0.583
# Evening   = 0.500
# Night     = 0.808

# Interpretation:
# Morning has the highest estimated peeing probability
# Evening has the lowest estimated peeing probability
# Night is also high
# Afternoon is intermediate

# Biological pattern:
# Morning   → very high peeing probability
# Afternoon → lower
# Evening   → lowest
# Night     → rises again


## 5. Pairwise contrasts

# Morning / Afternoon: p = 1.000
# Morning / Evening:   p = 1.000
# Morning / Night:     p = 1.000
# Afternoon / Evening: p = 0.965
# Afternoon / Night:   p = 0.325
# Evening / Night:     p = 0.235

# Interpretation:
# None of the pairwise comparisons are significant after Tukey adjustment.
#
# This can happen even when the overall test is significant or near significant:
# → the overall model detects a general time effect
# → but individual pairwise contrasts may lack power,
#   especially with small sample sizes and unequal probabilities


## 6. Why Morning looks strange

# Morning probability = 1.000 with extremely small SE
# → This suggests complete or near-complete separation:
#    all or almost all Morning individuals peed
#
# Because of this:
# → odds ratios involving Morning become extremely large
# → pairwise tests become unstable and hard to interpret
# → this is exactly why the Firth model is useful here


## 7. Final interpretation

# 1. Overall, peeing probability appears to vary across time windows

# 2. The strongest evidence is that:
#    - Evening is lower than Morning
#    - Afternoon may also be lower than Morning
#    - Night is not clearly different from Morning

# 3. However, no pairwise comparisons are significant after multiple-testing correction

# 4. Therefore, the safest conclusion is:
#    → there is evidence for an overall time-of-day effect on peeing probability,
#      but the exact pairwise differences between time windows remain uncertain

# Clean takeaway:
# → Peeing probability is highest in the Morning,
#   declines in the Afternoon and Evening,
#   and increases again at Night
# → The overall time pattern is supported,
#   but pairwise post-hoc contrasts are weak due to limited power and separation

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
#### 1: comparing weight and g/hr ####

library(dplyr)

## 1. Prepare dataset

input_bw_data <- final_cicten_pos %>%
  select(number, species, sex, input_g_hr, lab_weight) %>%
  filter(!is.na(input_g_hr), !is.na(lab_weight))

input_bw_data <- input_bw_data %>%
  mutate(input_mass_specific = input_g_hr / lab_weight)

# input_mass_specific standardizes input by body size
# → allows testing whether species differ independent of size


## 2. Species difference (mass-specific input)

wilcox.test(input_mass_specific ~ species, data = input_bw_data)

# Interpretation:
# p = 0.366
# → No significant difference between species (Dec vs Dem)
# → After correcting for body size, both species have similar input rates
#
# Biological meaning:
# → Any raw differences in input are not due to species-specific physiology,
#   but rather reflect similar size-normalized feeding rates


## 3. Linear model: input ~ body size * species

model_bw <- lm(input_g_hr ~ lab_weight * species, data = input_bw_data)
summary(model_bw)

# Intercept:
# Estimate = 0.0314, p = 0.107
# → Baseline input (Dec at lab_weight = 0) is not significant
# → Not biologically meaningful (weight = 0 is outside real range)

# lab_weight:
# Estimate = -0.0133, p = 0.690
# → No relationship between body size and input rate
# → Larger individuals do NOT show higher or lower input
#
# Key conclusion:
# → Input does NOT scale with body size in this dataset

# species (Dem vs Dec):
# Estimate = -0.00549, p = 0.838
# → No difference in baseline input between species
#
# → Confirms Wilcoxon test result:
#    species do not differ in input rate

# interaction (lab_weight × species):
# Estimate = 0.0231, p = 0.556
# → No difference in scaling between species
# → Both species show the same (non-existent) relationship with body size


## 5. Model fit

# R² = 0.036
# Adjusted R² = -0.016
#
# → Model explains essentially none of the variation in input
# → Predictors (size + species) have very low explanatory power

# F-statistic p = 0.563
# → Overall model is not significant
# → Cannot reject null hypothesis that all coefficients = 0


## 6. Final biological interpretation

# 1. No species effect:
#    → Dec and Dem have similar input rates

# 2. No size effect:
#    → Input does not scale with body weight

# 3. No interaction:
#    → Species do not differ in how input relates to size

# 4. Low explanatory power:
#    → Variation in input is driven by other factors (e.g., time of day)

# Clean takeaway:
# → Input rate is independent of both species identity and body size
# → This reinforces earlier results showing that time (not size or species)
#   is the primary driver of variation in input

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
#### sex difference ####

# additive model 
model_sex <- lm(input_g_hr ~ lab_weight * species + sex, data = input_bw_data)
summary(model_sex)

#Call:
#  lm(formula = input_g_hr ~ lab_weight * species + sex, data = input_bw_data)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.029809 -0.016023 -0.008232  0.006636  0.147525 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)            0.07776    0.03828   2.031   0.0472 *
#lab_weight            -0.07917    0.05753  -1.376   0.1745  
#speciesDem            -0.01350    0.02716  -0.497   0.6210  
#sexmale               -0.02303    0.01652  -1.394   0.1689  
#lab_weight:speciesDem  0.05684    0.04560   1.247   0.2179  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.03018 on 54 degrees of freedom
#Multiple R-squared:  0.06968,	Adjusted R-squared:  0.0007694 
#F-statistic: 1.011 on 4 and 54 DF,  p-value: 0.4099

#full interaction
model_full <- lm(input_g_hr ~ lab_weight * species * sex, data = input_bw_data)
summary(model_full)

#Call:
#  lm(formula = input_g_hr ~ lab_weight * species * sex, data = input_bw_data)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.040347 -0.015268 -0.005519  0.008978  0.139948 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)                    0.14762    0.07034   2.099   0.0408 *
#lab_weight                    -0.17949    0.10252  -1.751   0.0860 .
#speciesDem                    -0.11014    0.08073  -1.364   0.1784  
#sexmale                       -0.10664    0.09601  -1.111   0.2719  
#lab_weight:speciesDem          0.18019    0.10860   1.659   0.1032  
#lab_weight:sexmale             0.13139    0.19802   0.663   0.5100  
#speciesDem:sexmale             0.20584    0.15261   1.349   0.1834  
#lab_weight:speciesDem:sexmale -0.30778    0.27218  -1.131   0.2634  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.03038 on 51 degrees of freedom
#Multiple R-squared:   0.11,	Adjusted R-squared:  -0.01218 
#F-statistic: 0.9003 on 7 and 51 DF,  p-value: 0.5136

# plot
cols_all <- palette.colors(12, palette = "Paired")

ggplot(input_bw_data, 
       aes(x = lab_weight, y = input_g_hr, 
           color = species, shape = sex)) +
  
  geom_point(
    size = 3,
    alpha = 0.85
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Dec" = cols_all[9],
      "Dem" = cols_all[12]
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "female" = 15,  # square
      "male"   = 7    # square with cross
    )
  ) +
  
  labs(
    x = "Bodyweight (g)",
    y = "Input (g/hr)",
    color = "Species",
    shape = "Sex"
  ) +
  
  theme_classic()

# comparing within sex
input_bw_data %>%
  group_by(sex) %>%
  summarise(
    p_value = wilcox.test(input_g_hr ~ species)$p.value
  )
#sex    p_value
#<chr>    <dbl>
#1 female  0.0192 #females differ in dec & dem 
#2 male    0.403 

#Species differences in input rate are sex-specific, occurring in females but not in males.
#> While no overall effect of species or bodyweight on input rate was detected (linear models, p > 0.4)
#> , analyses within sex revealed a significant species difference in females (Wilcoxon test, p = 0.019)
#> , but not in males (p = 0.403), indicating a sex-specific pattern.

ggplot(input_bw_data, 
       aes(x = species, y = input_g_hr, color = species)) +
  
  geom_jitter(width = 0.1, alpha = 0.8) +
  
  facet_wrap(~sex) +
  
  scale_color_manual(
    values = c("Dec" = cols_all[9],
               "Dem" = cols_all[12]),
    guide = "none"   #
  ) +
  
  labs(
    x = "Species",          
    y = "Input (g/hr)"
  ) +
  
  theme_classic()

lm_female <- lm(input_g_hr ~ lab_weight * species,
                data = filter(input_bw_data, sex == "female"))

summary(lm_female)

#Call:
#  lm(formula = input_g_hr ~ lab_weight * species, data = filter(input_bw_data, sex == "female"))

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.031791 -0.015241 -0.005519  0.009121  0.098896 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)            0.14762    0.06008   2.457   0.0202 *
# lab_weight            -0.17949    0.08757  -2.050   0.0495 *
# speciesDem            -0.11014    0.06896  -1.597   0.1211  
# lab_weight:speciesDem  0.18019    0.09277   1.942   0.0619 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.02595 on 29 degrees of freedom
#Multiple R-squared:  0.1779,	Adjusted R-squared:  0.09287 
#F-statistic: 2.092 on 3 and 29 DF,  p-value: 0.123

#>In females, larger individuals have LOWER input rates (g/hr)
#>The relationship between bodyweight and input differs between species
#>Dec negative slope, dem has flat slope 

ggplot(filter(input_bw_data, sex == "female"),
       aes(x = lab_weight, y = input_g_hr, color = species)) +
  
  geom_point(size = 3, alpha = 0.85) +
  
  geom_smooth(method = "lm", se = FALSE) +
  
  scale_color_manual(
    values = c("Dec" = cols_all[9],
               "Dem" = cols_all[12])
  ) +
  
  theme_classic()
