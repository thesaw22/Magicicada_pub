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
# Important structure in data sheet:
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

#-----------------------------------------------------------------
#### Figure: Input rate (g/hr) + violin + time windows + mean & significance #####
# figure shows all 4 time windows 
# Violin shape 
# significance with ns 

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


#### Figure: ABCD, separate or combined #####
#Same figure just separated of x- and y-axis 
# no significance

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
 
# separated plots 
pA
pB
pC
pD

# combined plots (patchwork)
(pA | pB) / (pC | pD)

#### Figure: raw points + mean ± SE ####
# simple plot without violin

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

#### Figure: raw points + mean ± SE -> Dec&Dem ####
# species as one 
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

#-> better visualization of time window-effect (Behavioral pattern)
# -> combination of species has a higher statistical power (Afternoon!)

#### Figure: violin + raw points + mean ± SE ####
# separation of species again
# legend in the plot 

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

#### Figure: violin + raw points + mean ± SE & significance -> Dec&Dem ####
# Again combining Dec and Dem for higher statistic power 

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

#-> shows first significance for time window (behavioral pattern) 

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
# low power in the afternoon makes it difficult to interpret Results (!!!)


## 4. Linear model: species * time_window

model <- lm(input_g_hr ~ species * time_window, data = DecDem_)
anova(model)

# Interpretation of ANOVA:
# species: F = 0.803, p = 0.374
# -> No significant main effect of species
# -> Dec and Dem do not differ significantly in input rate overall
#
# time_window: F = 2.380, p = 0.080 
# -> No statistically significant main effect of time window at alpha = 0.05
# -> There is only a weak trend suggesting that input may vary across time
#
# species × time_window: F = 0.301, p = 0.825
# -> No interaction between species and time window
# -> temporal pattern in input does not differ between Dec and Dem


## 5. Model coefficients

summary(model)

# Interpretation:
# The intercept (0.0316, p < 0.001) represents the estimated mean input rate
# for the reference category, i.e. Dec in the Morning.
#
# speciesDem: estimate = 0.0167, p = 0.193
# -> Dem tends to have slightly higher Morning input than Dec,
#   but not significant
#
# time_windowAfternoon: estimate = -0.0186, p = 0.301
# -> In Dec, Afternoon input tends to be lower than Morning,
#   but not significant
#
# time_windowEvening: estimate = -0.00814, p = 0.581
# -> In Dec, Evening input does not differ significantly from Morning
#
# time_windowNight: estimate = -0.0187, p = 0.233
# -> In Dec, Night input tends to be lower than Morning,
#   but not significant
#
# -> All interaction terms are not significant (all p > 0.05)
# -> The difference between Dec and Dem does not change across time windows (!!!)


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
# -> No significant difference in input rate between Dec and Dem

# time_window: F = 2.44, p = 0.076
# -> No significant effect at alpha = 0.05,
#   but a trend suggesting input may vary across time

# sex: F = 2.03, p = 0.161
# -> No significant difference for sex

# species × time_window: F = 0.35, p = 0.787
# -> No interaction
# -> Temporal patterns do not differ between species
  # -> this might need a figure for the supplements (!!!)


# Model coefficients

summary(model_sex)

# Interpretation:

# Intercept (0.0399, p < 0.001)
# -> Estimated mean input for the reference group:
#    Dec females in the Morning

# speciesDem (0.0154, p = 0.224)
# -> Dem tends to have slightly higher input than Dec in the Morning,
#   but not significant

# time_windowAfternoon (-0.0237, p = 0.193)
# -> Input tends to be lower in the Afternoon compared to Morning (in Dec females),
#   but not significant

# time_windowEvening (-0.0091, p = 0.532)
# -> No difference between Evening and Morning

# time_windowNight (-0.0227, p = 0.147)
# -> Input tends to be lower at Night compared to Morning,
#   but not significant

# sexmale (-0.0129, p = 0.148)
# -> Males tend to have slightly lower input than females,
#   but not significant

# All interaction terms are not significant
# -> The effect of time does not differ between species (or sex)


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
# do not jointly explain a substantial portion of variation.(!!!)


# Final interpretation 

# Input rate did not differ significantly between species or sexes,
# and there was no significant interaction between species and time window.
#
# There was a weak trend for input to vary across time windows,
# with lower values in the Afternoon and Night relative to Morning,
# but this pattern was not significant.
#
# Overall, variation in input rate was high relative to the observed
# differences among groups, and the model explained little of the variance. (low power ?)


#-> While the overall effect of time window was not significant,
#-> pairwise comparisons revealed a significant difference between Morning and Afternoon input rates.
  # -> Seen in: Figure: violin + raw points + mean ± SE & significance -> Dec&Dem

#-----------------------------------------------------------------
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

# save as csv
write_csv(plot_data_pos, "final_cicten_pos.csv")

#### Checking species differences within each time window ####

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

#### Figure: Input rate (g/hr) + violin + time winow + mean ± SE (POS)####
# same figure as previously
# data only includes positive input 

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


#### Figure: ABCD, separate or combined #####

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


#### Figure: raw points + mean ± SE ####

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

# -> legend should be rearranged if used (!)

#### Figure: raw points + mean ± SE -> Dec&Dem ####

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


#### Figure: violin + raw points + mean ± SE ####

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


#### Figure: violin + raw points + mean ± SE & significance -> Dec&Dem ####

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

#-> usage of only positive input changes time window significance (!!!)
#-> argumentation for positive only in paper (...)

#### Statisitic tests ####

kruskal_test <- kruskal.test(input_g_hr ~ time_window, data = plot_data_pos)
kruskal_test
#input(g/hr) by time_window
# p = 0,00191

pairwise_tests <- pairwise.wilcox.test(
  plot_data_pos$input_g_hr,
  plot_data_pos$time_window,
  p.adjust.method = "BH"  #Benjamini Hochberg correction
)

pairwise_tests
# Morning vs Afternoon: p = 0,014
# Afternoon vs Evening: p = 0,014
# Morning vs Night: p = 0,014
# Evening vs Night: p = 0,019

#install.packages("rstatix")   
library(rstatix)

pairwise_results <- plot_data_pos %>%
  pairwise_wilcox_test(
    input_g_hr ~ time_window,
    p.adjust.method = "BH"
  )

pairwise_results

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
# Dec (n = 29) mean input ≈ 0.0263 g/hr
# Dem (n = 22) mean input ≈ 0.0391 g/hr
#
# -> Dem still shows a somewhat higher mean input rate than Dec 
# -> But variability is also high, especially in Dem
# -> So the numerical difference is not necessarily a statistically reliable one


## 2. Linear model: species * time_window

model <- lm(input_g_hr ~ species * time_window, data = plot_data_pos)
anova(model)

#No evidence that overall input differs between species
#No strong evidence that input changes across time of day

## 3. Model summary

summary(model)

# Interpretation:
# None of the individual coefficients are significant
# -> no single contrast stands out strongly against the reference category
# -> This is consistent with the ANOVA:
#    - weak species effect
#    - only a trend for time
#
# The Night coefficient is positive
# -> suggests input tends to be higher at night,
#   but there is still too much variability for strong support in this linear model


## 4. Estimated marginal means:
# comparing species within each time window

library(emmeans)

emmeans(model, pairwise ~ species | time_window)

# Interpretation:
# Morning:    Dec vs Dem p = 0.267
# Afternoon:  Dec vs Dem p = 0.912
# Evening:    Dec vs Dem p = 0.554
# Night:      Dec vs Dem p = 0.559
#
# -> No significant species differences in any time window
# -> Dem is numerically higher in several windows, especially Night,
#   but not significant


## 5. Effect sizes


library(effectsize)
eta_squared(model)

# Interpretation:
# species ≈ 0.05  -> small effect
# time_window ≈ 0.13 -> moderate effect
# interaction ≈ 0.0-e -> negligible effect
#
# -> Time explains more variation than species
# -> Species contributes little to variation in positive input rates


## 6. Non-parametric test across time windows

kruskal.test(input_g_hr ~ time_window, data = plot_data_pos)

# Interpretation:
# p = 0.0058
# -> Strong evidence that input differs among time windows
# -> Better than the linear model result (!!)
# -> Likely because the data are not good for standard ANOVA assumptions
#
# Important:
# Kruskal-Wallis only says that at least one time window differs.
# It does NOT tell us which time windows differ from each other. -> further testing 


## 7. Alternative if FSA still errors

# robust alternative is rstatix::dunn_test()

#install.packages("rstatix")
library(rstatix)

plot_data_pos %>%
  filter(!is.na(input_g_hr), !is.na(time_window)) %>%
  dunn_test(input_g_hr ~ time_window, p.adjust.method = "BH")

# Pairwise comparisons between time windows (BH-adjusted)

# Morning vs Afternoon: p.adj = 0.258
# -> Not significant
# -> Input rates are similar between these periods

# Morning vs Evening: p.adj = 0.807
# -> Not significant
# -> No difference between early and mid-day input

# Morning vs Night: p.adj = 0.0248 *
# -> Significant
# -> Input at Night is higher than in the Morning

# Afternoon vs Evening: p.adj = 0.258
# -> Not significant
# -> These two periods are similar in statistic (...)

# Afternoon vs Night: p.adj = 0.105
# -> Not significant 
# -> Suggests Night may be higher, but evidence is weak

# Evening vs Night: p.adj = 0.0248 *
# -> Significant
# -> Input at Night is higher than in the Evening

#-----------------------------------------------------------------
####  Final interpretation ####

# 1. Strongest pattern:
# -> Night vs (Morning + Evening) is significantly different

# 2. Intermediate periods:
# -> Afternoon and Evening are not clearly different from each other
# -> Afternoon vs Night shows only a weak trend

# 3. Overall structure:
# Morning   -> lowest input
# Afternoon -> intermediate
# Evening   -> intermediate
# Night     -> highest input


## Biological conclusion

# -> Input rate follows a diel pattern:
#    - Low in the morning
#    - Gradually increases
#    - Peaks at night

# -> The significant contrasts (Night vs Morning, Night vs Evening)
#   identify Night as the key period driving the overall effect

# -> Combined with previous results:
#    - Species (Dec vs Dem): NOT important
#    - Time of day: main driver of input variation

# Clean takeaway:
# -> Feeding/input is primarily time-dependent, with a clear nocturnal peak


#----------------------------------------------------------------
#### Excretion probability with positive input rate ####
#Clean corrected Input sheet

input_clean <- Input %>%
  clean_names() %>%
  select(-starts_with("unnamed")) %>%
  mutate(
    across(where(is.character), str_squish),
    across(where(is.character), ~ na_if(., "")),
    
    number = parse_number(as.character(number)),
    
    purpose = str_squish(purpose),
    species = str_squish(species),
    sex = str_squish(sex),
    
    action_3 = str_to_lower(as.character(action_3)),
    action_4 = str_to_lower(as.character(action_4)),
    
    weight_2 = as.numeric(str_replace(as.character(weight_2), ",", ".")),
    weight_3 = as.numeric(str_replace(as.character(weight_3), ",", ".")),
    
    input = as.numeric(str_replace(as.character(input), ",", ".")),
    input_hr = as.numeric(str_replace(as.character(input_hr), ",", ".")),
    
    time_window = case_when(
      str_to_lower(time_slot_of_third_action) == "morning" ~ "Morning",
      str_to_lower(time_slot_of_third_action) == "noon" ~ "Afternoon",
      str_to_lower(time_slot_of_third_action) == "afternoon" ~ "Afternoon",
      str_to_lower(time_slot_of_third_action) == "evening" ~ "Evening",
      str_to_lower(time_slot_of_third_action) == "night" ~ "Night",
      TRUE ~ NA_character_
    ),
    
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    
    species = factor(species, levels = c("Dec", "Dem", "Cas"))
  ) %>%
  filter(
    purpose == "Excretion",
    species %in% c("Dec", "Dem"),
    !number %in% remove_ids,
    !is.na(time_window)
  ) %>%
  droplevels()


#### Excretion probability: actual pee data ####
# This is NOT input rate.
# It uses observed pee events only.

pee_data <- input_clean %>%
  mutate(
    pee_amount =
      if_else(action_3 == "pee" & !is.na(weight_2), weight_2, 0) +
      if_else(action_4 == "pee" & !is.na(weight_3), weight_3, 0),
    
    peed = as.integer(pee_amount > 0)
  )

excretion_rate <- pee_data %>%
  group_by(time_window) %>%
  summarise(
    n = n(),
    n_peed = sum(peed, na.rm = TRUE),
    prop_peed = n_peed / n,
    .groups = "drop"
  )

excretion_rate


#### Figure: Excretion probability ####

prob_plot <- ggplot(
  excretion_rate,
  aes(x = time_window, y = prop_peed, fill = time_window)
) +
  geom_col(
    width = 0.7,
    alpha = 0.85,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0(n_peed, "/", n)),
    vjust = -0.5,
    size = 5
  ) +
  scale_fill_manual(values = time_cols, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Time window",
    y = "Excretion probability"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )

prob_plot


#### Fisher's exact test ####

pee_table <- table(pee_data$time_window, pee_data$peed)

pee_table

#           0  1
#Morning    4 21
#Afternoon  0  6
#Evening    6  9
#Night      6  6

fisher_pee <- fisher.test(pee_table)

fisher_pee
# p-value = 0.04041

#### Positive input rate ####
# This is separate from pee probability.
# corrected input_hr and only positive input-rate observations.

plot_data_pos <- input_clean %>%
  filter(
    !is.na(input_hr),
    input_hr > 0
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
    mean_input = mean(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    min_input = min(input_hr, na.rm = TRUE),
    max_input = max(input_hr, na.rm = TRUE),
    .groups = "drop"
  )

input_summary

#Morning: Median -> 0.0396
#Afternoon: Median -> 0.0107
#Evening: Median -> 0.0389
#Night: Median -> 0.0064

#### Kruskal-Wallis test ####

kruskal_input <- kruskal.test(input_hr ~ time_window, data = plot_data_pos)

kruskal_input
#data:  input_hr by time_window
#Kruskal-Wallis chi-squared = 16.746, df = 3, p-value = 0.000797

#### Pairwise Wilcoxon tests ####

pairwise_input <- plot_data_pos %>%
  pairwise_wilcox_test(
    input_hr ~ time_window,
    p.adjust.method = "BH"
  )

pairwise_input

#-> Morning - Afternoon = significant **
#-> Morning - Evening = ns 
#-> Morning - Night = significant **
#-> Afternoon - Evening = significant **
#-> Afternoon - Night = ns 
#-> Evening - Night = significant *

#----------------------------------------------------------------
#### Body weight and input rate: Dec vs Dem ####
# Clean corrected Input sheet

input_clean <- Input %>%
  clean_names() %>%
  select(-starts_with("unnamed")) %>%
  mutate(
    across(where(is.character), str_squish),
    across(where(is.character), ~ na_if(., "")),
    
    number = parse_number(as.character(number)),
    
    purpose = str_squish(purpose),
    species = str_squish(species),
    sex = str_to_lower(str_squish(sex)),
    
    lab_weight = as.numeric(str_replace(as.character(weight_1), ",", ".")),
    input = as.numeric(str_replace(as.character(input), ",", ".")),
    input_hr = as.numeric(str_replace(as.character(input_hr), ",", ".")),
    
    species = factor(species, levels = c("Dec", "Dem", "Cas")),
    sex = factor(sex, levels = c("female", "male"))
  ) %>%
  filter(
    purpose == "Excretion",
    species %in% c("Dec", "Dem"),
    !number %in% remove_ids,
    !is.na(input_hr),
    !is.na(lab_weight),
    !is.na(species),
    !is.na(sex),
    input_hr > 0
  ) %>%
  droplevels()


#### Final body weight/input dataset ####

input_bw_data <- input_clean %>%
  select(number, species, sex, input_hr, lab_weight)

input_bw_data

write_csv(input_bw_data, "input_bodyweight_positive_corrected.csv")


#### Figure: body weight vs input rate, species separated ####

plot_bodyweight_input_s <- ggplot(
  input_bw_data,
  aes(
    x = lab_weight,
    y = input_hr,
    color = species
  )
) +
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


#### Statistic test: body weight + species ####
# This is the main test for whether species differ after accounting to body weight.

model_species_bw <- lm(
  input_hr ~ lab_weight + species,
  data = input_bw_data
)

summary(model_species_bw)
anova(model_species_bw)


#### Figure: body weight vs input rate by species and sex ####

plot_sex_species <- ggplot(
  input_bw_data,
  aes(
    x = lab_weight,
    y = input_hr,
    color = species,
    shape = sex
  )
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ sex) +
  scale_color_manual(
    values = c(
      "Dec" = cols_all[7],
      "Dem" = cols_all[8]
    )
  ) +
  scale_shape_manual(
    values = c(
      "female" = 15,
      "male" = 7
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


#### Statistic test: body weight + species + sex ####
# This checks whether sex explains additional variation.

model_species_bw_sex <- lm(
  input_hr ~ lab_weight + species + sex,
  data = input_bw_data
)

summary(model_species_bw_sex)
anova(model_species_bw_sex)
# -> no significance

#### Statistic test: body weight × species ####
# This checks whether the body weight-input relationship differs by species.
# interaction model (*)
model_bw_species_interaction <- lm(
  input_hr ~ lab_weight * species,
  data = input_bw_data
)

summary(model_bw_species_interaction)
anova(model_bw_species_interaction)


#### Statistic test: species × sex ####
# This checks whether species differences depend on sex.
# interaction model

model_species_sex_interaction <- lm(
  input_hr ~ lab_weight + species * sex,
  data = input_bw_data
)

summary(model_species_sex_interaction)
anova(model_species_sex_interaction)

#-> no significance 

#### Figure: species comparison within each sex ####
# Visual only. 
# the separation of sex probablz does not have a lot of meaning or statistic value (...)

plot_species_by_sex <- ggplot(
  input_bw_data,
  aes(
    x = species,
    y = input_hr,
    color = species
  )
) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.85) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 4,
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.12,
    color = "black",
    linewidth = 0.5
  ) +
  facet_wrap(~ sex) +
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
#----------------------------------------------------------------

