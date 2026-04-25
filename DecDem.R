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
#### Excretion probability ####
# Question:
# Does the probability of excreting differ among time windows?
#
# Data used:
# ALL observations, because individuals that did NOT excrete are biologically
# important for estimating probability.


library(dplyr)
library(ggplot2)
library(logistf)

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
    n_peed = sum(peed),
    prop_peed = n_peed / n,
    .groups = "drop"
  )

excretion_rate

# Interpretation:
# Morning:    21/25 = 0.84
# Afternoon:   6/6 = 1.00
# Evening:    9/15 = 0.60
# Night:      6/12 = 0.50

# Excretion probability is highest in Afternoon and Morning,
# and lower in Evening and Night.

prob_plot <- ggplot(excretion_rate,
                    aes(x = time_window, y = prop_peed, fill = time_window)) +
  geom_col(width = 0.7, alpha = 0.85, color = "black") +
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

#### Firth logistic regression ####
# Why this test?
#
# The response variable is binary:
#   peed = 0 or 1
#
# Therefore, logistic regression is appropriate.
#
# However, Afternoon has complete separation:
#   Afternoon = 6/6 individuals peed
#
# Standard logistic regression can behave badly with complete separation.
# Firth logistic regression corrects this problem and gives more stable estimates.


model_firth <- logistf(peed ~ time_window, data = pee_data)

summary(model_firth)

# Interpretation:
#
# Overall likelihood-ratio test:
# p = 0.057
#
# This is slightly above 0.05, so the overall time-window effect is
# borderline / suggestive, but not conventionally significant by the
# Firth likelihood-ratio test.
#
# Coefficients are compared against the reference level: Morning.
#
# Afternoon vs Morning:
# p = 0.475
# No significant difference.
# Afternoon has 100% excretion, but n = 6 is small.
#
# Evening vs Morning:
# p = 0.098
# Trend toward lower excretion probability in Evening.
#
# Night vs Morning:
# p = 0.036
# Night has significantly lower excretion probability than Morning.
#
# Biological interpretation:
# Excretion probability appears to decline later in the day,
# especially at Night, but the overall Firth model is borderline.


#### Fisher's exact test ####
# Why this test?
#
# This is a non-parametric test for association between two categorical variables:
#   time_window and peed
#
# It is useful here because some sample sizes are small,
# especially Afternoon n = 6.
#
# Fisher's exact test does not rely on large-sample chi-square assumptions.

pee_table <- table(pee_data$time_window, pee_data$peed)

pee_table

fisher.test(pee_table)

# Interpretation:
#
# Fisher's exact test p = 0.040
#
# This suggests that excretion probability is not equal across time windows.
#
# Together with the Firth model:
# - Fisher's test supports a time-window effect.
# - Firth logistic regression gives a borderline overall effect,
#   but identifies Night as lower than Morning.
#
# Conservative conclusion:
# Excretion probability varies with time window, with the clearest decrease
# occurring at Night compared with Morning.


#### Input rate: positive input only ####
# Question:
# Among individuals with positive input, does the amount of input differ
# among time windows?
#
# Data used:
# ONLY observations with input_g_hr > 0.
#
# This is different from excretion probability.
# Here we are not asking whether input happened.
# We are asking how high input was when input occurred.

library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)

plot_data_pos <- final_cicten %>%
  filter(
    !is.na(time_window),
    !is.na(input_g_hr),
    input_g_hr > 0
  ) %>%
  mutate(
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    )
  )

input_summary <- plot_data_pos %>%
  group_by(time_window) %>%
  summarise(
    n = n(),
    mean_input = mean(input_g_hr),
    sd_input = sd(input_g_hr),
    median_input = median(input_g_hr),
    min_input = min(input_g_hr),
    max_input = max(input_g_hr),
    .groups = "drop"
  )

input_summary

# Interpretation:
#
# Morning:
# n = 24, mean = 0.0407 g/hr, median = 0.0315 g/hr
#
# Afternoon:
# n = 6, mean = 0.0120 g/hr, median = 0.0100 g/hr
#
# Evening:
# n = 11, mean = 0.0364 g/hr, median = 0.0320 g/hr
#
# Night:
# n = 10, mean = 0.0175 g/hr, median = 0.00614 g/hr
#
# Input rate is highest in Morning and Evening,
# and lower in Afternoon and Night.


#### Kruskal-Wallis test ####
# Why this test?
#
# Input rate is continuous, but sample sizes are uneven and small
# in some time windows.
#
# The data are also likely non-normal and skewed.
#
# Kruskal-Wallis is a non-parametric alternative to one-way ANOVA.
# It tests whether input-rate distributions differ among time windows.


kruskal.test(input_g_hr ~ time_window, data = plot_data_pos)

# Interpretation:
#
# Kruskal-Wallis chi-squared = 14.888
# df = 3
# p = 0.001915
#
# Input rate differs significantly among time windows.
#
# This test only says that at least one time window differs.
# It does not say which specific pairs differ.


#### Pairwise Wilcoxon tests ####
# Why this test?
#
# After a significant Kruskal-Wallis test, pairwise Wilcoxon tests are used
# to identify which time windows differ from each other.
#
# BH correction is used to control false discovery rate across multiple tests.

pairwise_input <- plot_data_pos %>%
  pairwise_wilcox_test(
    input_g_hr ~ time_window,
    p.adjust.method = "BH"
  )

pairwise_input

# Interpretation:
#
# Significant comparisons:
#
# Morning vs Afternoon:
# p.adj = 0.014
# Morning input is significantly higher than Afternoon.
#
# Morning vs Night:
# p.adj = 0.014
# Morning input is significantly higher than Night.
#
# Afternoon vs Evening:
# p.adj = 0.014
# Evening input is significantly higher than Afternoon.
#
# Evening vs Night:
# p.adj = 0.019
# Evening input is significantly higher than Night.
#
# Non-significant comparisons:
#
# Morning vs Evening:
# p.adj = 0.919
# Morning and Evening input rates are similar.
#
# Afternoon vs Night:
# p.adj = 0.958
# Afternoon and Night input rates are similar.
#
# Biological interpretation:
# Positive input rate follows a clear time-window pattern.
# Input is high in Morning and Evening,
# and low in Afternoon and Night.

#### Overall conclusion: ####
#
# Excretion probability:
# Excretion probability differed among time windows.
# Fisher's exact test was significant (p = 0.040), while Firth logistic
# regression showed a borderline overall time effect (p = 0.057).
# Night had significantly lower excretion probability than Morning
# in the Firth model (p = 0.036).
#
# Input rate:
# Among observations with positive input, input rate differed strongly
# among time windows (Kruskal-Wallis p = 0.0019).
# Morning and Evening had higher input rates, whereas Afternoon and Night
# had lower input rates.
#
# Important distinction:
# Excretion probability and input rate answer different questions.
# Excretion probability uses all observations.
# Input rate uses only positive-input observations.

#----------------------------------------------------------------
#### Bodyweight and input rate: Dec vs Dem ####
# Question:
# Do Dec and Dem differ in input rate, and is this difference explained
# by bodyweight?
#
# Data:
# Positive input data only.

library(dplyr)
library(ggplot2)

cols_all <- palette.colors(12, palette = "Paired")

input_bw_data <- final_cicten_pos %>%
  select(number, species, sex, input_g_hr, lab_weight) %>%
  filter(
    !is.na(input_g_hr),
    !is.na(lab_weight),
    !is.na(species),
    !is.na(sex)
  ) %>%
  mutate(
    species = factor(species, levels = c("Dec", "Dem")),
    sex = factor(sex, levels = c("female", "male"))
  )

#### 1. Plot: species separated ####
# This plot shows whether input rate changes with bodyweight
# separately for Dec and Dem.

plot_bodyweight_input_s <- ggplot(input_bw_data,
                       aes(x = lab_weight,
                           y = input_g_hr,
                           color = species)) +
  
  geom_point(size = 3, alpha = 0.85) +
  
  geom_smooth(method = "lm", se = TRUE) +
  
  scale_color_manual(
    values = c(
      "Dec" = cols_all[7],
      "Dem" = cols_all[8]
    )
  ) +
  
  labs(
    x = "Bodyweight (g)",
    y = "Input rate (g/hr)",
    color = "Species"
  ) +
  
  theme_classic()

plot_bodyweight_input_s

#### 2. Linear model: bodyweight + species ####
# Why this test?
#
# This model tests whether input rate is related to bodyweight
# and whether species differ after accounting for bodyweight.
#
# lab_weight:
# Tests whether larger individuals have higher or lower input rate.
#
# species:
# Tests whether Dec and Dem differ in input rate after controlling
# for bodyweight.

model_species_bw <- lm(input_g_hr ~ lab_weight + species,
                       data = input_bw_data)

summary(model_species_bw)
anova(model_species_bw)

# Interpretation based on your results:
#
# lab_weight:
# p = 0.375
# Bodyweight does not significantly explain input rate.
#
# species:
# p = 0.403
# Dec and Dem do not significantly differ in input rate after
# accounting for bodyweight.
#
# Overall model:
# p = 0.475
# R² = 0.027
#
# The model explains very little variation in input rate.
#
# Biological conclusion:
# There is no evidence that input rate differs between Dec and Dem
# because of bodyweight differences.


#### Species and sex separated ####
# Question:
# Does the relationship between bodyweight and input rate differ
# by species and sex?

plot_sex_species <- ggplot(input_bw_data,
                           aes(x = lab_weight,
                               y = input_g_hr,
                               color = species,
                               shape = sex)) +
  
  geom_point(size = 3, alpha = 0.85) +
  
  geom_smooth(method = "lm", se = FALSE) +
  
  facet_wrap(~sex) +
  
  scale_color_manual(
    values = c(
      "Dec" = cols_all[7],
      "Dem" = cols_all[8]
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "female" = 15,
      "male"   = 7
    )
  ) +
  
  labs(
    x = "Bodyweight (g)",
    y = "Input rate (g/hr)",
    color = "Species",
    shape = "Sex"
  ) +
  
  theme_classic()

plot_sex_species


#### 3. Additive model: bodyweight + species + sex ####
#
# This model tests whether input rate is explained independently by:
# bodyweight, species, and sex.
#
# It does NOT test whether the species effect differs between males
# and females.

model_full <- lm(input_g_hr ~ lab_weight + species + sex,
                 data = input_bw_data)

summary(model_full)
anova(model_full)

# Interpretation based on your results:
#
# lab_weight:
# p = 0.376
# No significant effect of bodyweight.
#
# species:
# p = 0.404
# No significant overall difference between Dec and Dem.
#
# sex:
# p = 0.365
# No significant overall difference between females and males.
#
# Overall model:
# p = 0.511
# R² = 0.042
#
# Biological conclusion:
# Input rate is not significantly explained by bodyweight,
# species, or sex.

#### 4. Interaction model: species * bodysize ####
# Do species differ in input depending on body size?


model_interaction <- lm(input_g_hr ~ lab_weight * species,
                        data = input_bw_data)

summary(model_interaction)
anova(model_interaction)

#### 5. Interaction model: species × sex ####
#
# This tests whether the species difference depends on sex.
#
# In other words:
# Are Dec and Dem different in females, but not in males?

model_interaction <- lm(input_g_hr ~ lab_weight + species * sex,
                        data = input_bw_data)

summary(model_interaction)
anova(model_interaction)

# Interpretation based on your results:
#
# species:sex interaction:
# p = 0.611
#
# This is not significant.
#
# Therefore, there is no strong statistical evidence that the
# species effect differs between females and males.
#
# Important:
# Because this interaction is not significant, any female-only
# species difference must be interpreted cautiously.


#### 6. Exploratory within-sex Wilcoxon tests ####
# Why this test?
#
# These tests compare Dec vs Dem separately within each sex.
#
# They are useful as exploratory follow-up tests, but they should not
# be treated as the main result because the species × sex interaction
# was not significant.

wilcox_female <- wilcox.test(input_g_hr ~ species,
                             data = filter(input_bw_data, sex == "female"))

wilcox_male <- wilcox.test(input_g_hr ~ species,
                           data = filter(input_bw_data, sex == "male"))

wilcox_female
wilcox_male

# Interpretation based on your results:
#
# Females:
# p = 0.030
# Female Dec and Dem differ in input rate in this exploratory test.
#
# Males:
# p = 0.403
# Male Dec and Dem do not significantly differ.
#
# Important caution:
# The species × sex interaction was not significant in the linear model.
# Therefore, the female-only Wilcoxon result should be described as
# exploratory, not as strong evidence for a sex-specific species effect.


#### 7. Optional plot: species comparison within each sex ####
# This plot directly shows the exploratory within-sex comparison.

plot_species_by_sex <- ggplot(input_bw_data,
                              aes(x = species,
                                  y = input_g_hr,
                                  color = species)) +
  
  geom_jitter(width = 0.1, size = 3, alpha = 0.85) +
  
  facet_wrap(~sex) +
  
  scale_color_manual(
    values = c(
      "Dec" = cols_all[7],
      "Dem" = cols_all[8]
    ),
    guide = "none"
  ) +
  
  labs(
    x = "Species",
    y = "Input rate (g/hr)"
  ) +
  
  theme_classic()

plot_species_by_sex

#### Final interpretation: ####
#
# Bodyweight did not significantly explain input rate.
# Dec and Dem did not differ significantly in input rate after accounting
# for bodyweight.
# Sex also did not significantly explain input rate.
# The species × sex interaction was not significant, meaning there is no
# strong evidence that species differences depend on sex.
#
# Exploratory Wilcoxon tests showed a significant Dec vs Dem difference
# within females, but not within males. However, because the interaction
# model was not significant, this female-only pattern should be interpreted
# cautiously.
#
# Overall conclusion:
# Input rate does not appear to be driven by bodyweight differences between
# Dec and Dem.