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

#### tidy data set for cas ####
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(hms)

# IDs to remove for Cas
remove_ids_cas <- c(26, 27, 28, 29, 45, 46, 49, 50, 62, 64, 76, 78, 124, 134, 156, 169, 170)

cicten_tidy_cas <- CicTen %>%
  
  # clean column names
  clean_names() %>%
  
  # trim whitespace
  mutate(across(where(is.character), str_squish)) %>%
  
  # make sure number is numeric
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
  
  # keep only Cas weight individuals
  filter(
    species == "Cas",
    purpose == "Weight"
  ) %>%
  
  # remove biologically wrong individuals
  filter(!number %in% remove_ids_cas) %>%
  
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
  
  # rebuild the event structure explicitly
  transmute(
    number,
    species,
    sex,
    purpose,
    sampling_date,
    
    # event 1: keep action/date/weight 1, but use time 2
    action_1 = action_1,
    date_1   = date_1,
    time_1   = time_2,
    weight_1 = weight_1,
    
    # event 2: drop old action/date/time 2; weight 2 goes with action/date/time 3
    action_2 = action_3,
    date_2   = date_3,
    time_2   = time_3,
    weight_2 = weight_2,
    
    # event 3
    action_3 = action_4,
    date_3   = date_4,
    time_3   = time_4,
    weight_3 = weight_3,
    
    # event 4
    action_4 = action_5,
    date_4   = date_5,
    time_4   = time_5,
    weight_4 = weight_4,
    
    # event 5 empty
    action_5 = NA_character_,
    date_5   = NA_character_,
    time_5   = NA,
    weight_5 = NA_real_
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
 
view(cicten_tidy_cas)

write_csv(cicten_tidy_cas, "data/cicten_tidy_cas.csv")

#### 1: weight change of individuals ####

library(dplyr)
library(ggplot2)
library(scales)

paired <- palette.colors(12, "Paired")

bg_cols <- c(
  "morning"   = paired[5],
  "afternoon" = paired[9],
  "evening"   = paired[3]
)

plot_weight <- cicten_tidy_cas %>%
  filter(!is.na(time), !is.na(weight)) %>%
  mutate(
    date = as.character(date),
    time = as.character(time),
    datetime = as.POSIXct(
      paste(date, time),
      format = "%d.%m.%y %H:%M"
    )
  ) %>%
  filter(!is.na(datetime)) %>%
  arrange(number, datetime)

days_all <- data.frame(
  day = seq.Date(
    from = as.Date(min(plot_weight$datetime)),
    to   = as.Date(max(plot_weight$datetime)),
    by   = "day"
  )
)

time_blocks <- bind_rows(
  days_all %>%
    mutate(
      period = "morning",
      xmin = as.POSIXct(paste(day, "08:00:00")),
      xmax = as.POSIXct(paste(day, "11:00:00"))
    ),
  days_all %>%
    mutate(
      period = "afternoon",
      xmin = as.POSIXct(paste(day, "12:00:00")),
      xmax = as.POSIXct(paste(day, "17:00:00"))
    ),
  days_all %>%
    mutate(
      period = "evening",
      xmin = as.POSIXct(paste(day, "18:00:00")),
      xmax = as.POSIXct(paste(day, "20:00:00"))
    )
) %>%
  mutate(
    ymin = -Inf,
    ymax = Inf
  )

fig_weight_time <- ggplot() +
  geom_rect(
    data = time_blocks,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = period
    ),
    alpha = 0.22,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_line(
    data = plot_weight,
    aes(
      x = datetime,
      y = weight,
      group = number
    ),
    linewidth = 0.5,
    alpha = 0.7,
    color = "grey35"
  ) +
  geom_point(
    data = plot_weight,
    aes(
      x = datetime,
      y = weight,
      shape = sex
    ),
    size = 2.8,
    stroke = 0.8,
    color = "black",
    fill = "black"
  ) +
  scale_fill_manual(values = bg_cols) +
  scale_shape_manual(
    values = c(
      "female" = 15,
      "male"   = 7
    )
  ) +
  scale_y_continuous(
    labels = function(x) sprintf("%.3f", x)
  ) +
  scale_x_datetime(
    date_labels = "%d.%m %H:%M",
    date_breaks = "4 hours"
  ) +
  labs(
    x = "Time",
    y = "Bodyweight (g)",
    shape = "Sex"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

fig_weight_time

#### 2: plotting it into "one day" ####

plot_weight <- cicten_tidy_cas %>%
  filter(!is.na(time), !is.na(weight)) %>%
  mutate(
    time_h = as.numeric(time) / 3600   # seconds → hours
  ) %>%
  arrange(number, time_h)

time_blocks <- data.frame(
  period = c("morning", "afternoon", "evening"),
  xmin = c(8, 12, 18),
  xmax = c(11, 17, 20),
  ymin = -Inf,
  ymax = Inf
)

fig_weight_time <- ggplot() +
  
  geom_rect(
    data = time_blocks,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = period
    ),
    alpha = 0.22,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  
  geom_line(
    data = plot_weight,
    aes(
      x = time_h,
      y = weight,
      group = number
    ),
    linewidth = 0.5,
    alpha = 0.7,
    color = "grey35"
  ) +
  
  geom_point(
    data = plot_weight,
    aes(
      x = time_h,
      y = weight,
      shape = sex
    ),
    size = 2.8,
    stroke = 0.8,
    color = "black",
    fill = "black"
  ) +
  
  scale_fill_manual(values = bg_cols) +
  
  scale_shape_manual(
    values = c(
      "female" = 15,
      "male"   = 7
    )
  ) +
  
  scale_y_continuous(
    labels = function(x) sprintf("%.3f", x)
  ) +
  
  scale_x_continuous(
    breaks = seq(6, 22, by = 2),
    labels = function(x) sprintf("%02d:00", x)
  ) +
  
  labs(
    x = "Time window",
    y = "Bodyweight (g)",
    shape = "Sex"
  ) +
  
  theme_classic()

fig_weight_time

#### postiive and negtive bodychange ####
library(dplyr)
library(ggplot2)

plot_weight <- cicten_tidy_cas %>%
  filter(!is.na(time), !is.na(weight)) %>%
  mutate(
    time_h = as.numeric(time) / 3600   # seconds → hours
  ) %>%
  arrange(number, time_h) %>%
  group_by(number) %>%
  mutate(
    delta_weight = weight - lag(weight),
    change_dir = case_when(
      delta_weight > 0 ~ "gain",
      delta_weight < 0 ~ "loss",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

time_blocks <- data.frame(
  period = c("morning", "afternoon", "evening"),
  xmin = c(8, 12, 18),
  xmax = c(11, 17, 20),
  ymin = -Inf,
  ymax = Inf
)

fig_weight_time_2 <- ggplot() +
  
  geom_rect(
    data = time_blocks,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = period
    ),
    alpha = 0.22,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  
  geom_line(
    data = plot_weight,
    aes(
      x = time_h,
      y = weight,
      group = number
    ),
    linewidth = 0.5,
    alpha = 0.7,
    color = "grey35"
  ) +
  
  geom_point(
    data = plot_weight,
    aes(
      x = time_h,
      y = weight,
      shape = sex,
      color = change_dir
    ),
    size = 2.8,
    stroke = 0.8
  ) +
  
  scale_fill_manual(values = bg_cols) +
  
  scale_color_manual(
    values = c(
      "gain" = "black",
      "loss" = "grey65"
    ),
    na.value = "black",
    name = "Weight change",
    labels = c(
      "gain" = "Gain",
      "loss" = "Loss"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "female" = 15,
      "male"   = 7
    )
  ) +
  
  scale_y_continuous(
    labels = function(x) sprintf("%.3f", x)
  ) +
  
  scale_x_continuous(
    breaks = seq(6, 22, by = 2),
    labels = function(x) sprintf("%02d:00", x)
  ) +
  
  labs(
    x = "Time window",
    y = "Bodyweight (g)",
    shape = "Sex"
  ) +
  
  theme_classic()

fig_weight_time_2

#### statistics ####
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)

_____________________________________________________________________
#### 1. Prepare analysis dataset ####

# Create time variable (in hours) and define biologically
# meaningful time windows (based on your sampling times)

analysis_data <- cicten_tidy_cas %>%
  filter(!is.na(time), !is.na(weight), !is.na(sex)) %>%
  mutate(
    time_h = as.numeric(time) / 3600,  # convert time to hours
    
    # assign each observation to a time window
    time_window = case_when(
      time_h >= 8  & time_h < 11 ~ "08:00",   # morning
      time_h >= 11 & time_h < 12 ~ "11:00",   # late morning (peak?)
      time_h >= 12 & time_h < 20 ~ "17:30",   # daytime (decline?)
      time_h >= 20 & time_h < 22 ~ "20:30",   # night (secondary rise?)
      TRUE ~ NA_character_
    ),
    
    # set order of time windows
    time_window = factor(
      time_window,
      levels = c("08:00", "11:00", "17:30", "20:30")
    ),
    
    sex = factor(sex)
  ) %>%
  filter(!is.na(time_window)) %>%
  arrange(number, time_h)

# check sample size per group
analysis_data %>%
  count(time_window, sex)


#### 2. Visualization: bodyweight across time and sex ####

# Shows distribution (boxplot) + individual data points

ggplot(analysis_data, aes(x = time_window, y = weight)) +
  
  geom_boxplot(
    aes(color = sex),
    alpha = 0.5,
    outlier.shape = NA,
    position = position_dodge(width = 0.8)
  ) +
  
  geom_jitter(
    aes(shape = sex, color = sex),
    size = 2.5,
    alpha = 0.8,
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.8
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "female" = 15,   # filled square
      "male"   = 7     # square with cross
    )
  ) +
  
  scale_color_manual(
    values = c(
      "female" = cols_all[9],
      "male"   = cols_all[10]
    )
  ) +
  
  scale_y_continuous(
    labels = function(x) sprintf("%.3f", x)
  ) +
  
  theme_classic() +
  
  labs(
    x = "Time window",
    y = "Bodyweight (g)",
    color = "Sex",
    shape = "Sex"
  )


#### 3. MODEL: Does bodyweight differ across time? ####
# Tests:
# "Is bodyweight different at different times of day?"
# Random effect (number) accounts for repeated measurements

#remove.packages("lme4")
#install.packages("Matrix")
#install.packages("lme4", type = "source")

library(Matrix)
library(lmerTest)
library(lme4)

model_time <- lmer(weight ~ time_window + (1 | number), data = analysis_data)

# fixed effects
fixef(model_time)

# fitted values / residuals if needed
head(fitted(model_time))
head(resid(model_time))

anova(model_time)

# Mixed model: weight ~ time_window + (1 | number)

# (Intercept)       = 0.6107
# time_window11:00  = 0.0792
# time_window17:30  = 0.0433
# time_window20:30  = 0.0621

# Interpretation:
# Weight changes significantly across time windows.
# The lowest estimated weight is at 08:00.
# Weight is highest at 11:00.
# Weight then drops somewhat at 17:30,
# and rises again slightly by 20:30.
#
# This suggests a clear within-day fluctuation in body weight.

# ANOVA result:
# time_window: F = 25.14, p = 4e-10

# Interpretation:
# → VERY strong effect of time_window on weight
# → Weight significantly changes across time points

# Because (1 | number) is included:
# → This accounts for repeated measurements per individual
# → So this result reflects WITHIN-individual changes over time

# Biological meaning:
# → Individuals systematically gain or lose weight across the day
# → This is not due to differences between individuals,
#   but due to temporal dynamics within the same individual


# pairwise comparisons (which times differ?)
emm_time <- emmeans(model_time, pairwise ~ time_window)
emm_time
# RESULT:
# - Bodyweight lowest at 08:00
# - Strong increase at 11:00
# - Decrease during day (17:30)
# - Increase again at 20:30
# → peak at late morning, smaller peak at night


#### 4. MODEL: Are males and females different overall? ####

# Tests:
# "Are females and males different in bodyweight overall?"

model_sex <- lmer(weight ~ sex + time_window + (1 | number), data = analysis_data)

summary(model_sex)
anova(model_sex)
# RESULT:
# Strong effect of sex


# estimated means for each sex
emmeans(model_sex, pairwise ~ sex)
# RESULT:
# females > males (p < 0.0001)
# → females are significantly heavier


#### 5. MODEL: Do males and females differ in pattern over time? ####

# Tests:
# "Does the change across time differ between sexes?"

model_time_sex <- lmer(weight ~ time_window * sex + (1 | number), data = analysis_data)

summary(model_time_sex)
anova(model_time_sex)
# RESULT:
# - time_window significant
# - sex significant
# - interaction NOT significant
# → same pattern in both sexes


# time effects within each sex
emm_time_sex <- emmeans(model_time_sex, pairwise ~ time_window | sex)
emm_time_sex
# RESULT:
# both sexes show increase at 11:00 and decrease during day


# sex differences within each time window
emm_sex_time <- emmeans(model_time_sex, pairwise ~ sex | time_window)
emm_sex_time
# RESULT:
# females > males at ALL times


#### 6. MODEL: When does bodyweight actually increase? ####

# Instead of absolute weight, calculate change between measurements

analysis_change <- analysis_data %>%
  group_by(number) %>%
  arrange(time_h, .by_group = TRUE) %>%
  mutate(
    weight_change = weight - lag(weight)
  ) %>%
  ungroup()

# inspect changes
analysis_change %>%
  select(number, time_window, time_h, weight, weight_change) %>%
  print(n = 30)


# mixed model for weight change
# Tests:
# "At what time does weight increase or decrease?"

model_change <- lmer(weight_change ~ time_window * sex + (1 | number),
                     data = analysis_change,
                     REML = FALSE)

summary(model_change)
anova(model_change)
# RESULT:
# Strong effect of time_window on weight change


# pairwise comparisons
emmeans(model_change, pairwise ~ time_window)
# RESULT:
# - largest increase at 11:00
# - decrease during day (17:30)
# - smaller increase at 20:30


emmeans(model_change, pairwise ~ sex)
# RESULT:
# no significant difference in weight change between sexes


emmeans(model_change, pairwise ~ time_window | sex)
# RESULT:
# both sexes show same general pattern:
# strong increase at 11:00, decrease during day, small increase at night


#### 7. Visualization: smooth trend over time ####

# Shows continuous pattern (not statistical test)

ggplot(analysis_data, aes(x = time_h, y = weight)) +
  
  geom_point(
    aes(shape = sex, color = sex),
    alpha = 0.8,
    size = 2.5
  ) +
  
  geom_smooth(
    aes(color = sex),
    method = "loess",  # captures nonlinear pattern
    se = TRUE
  ) +
  
  scale_shape_manual(
    values = c(
      "female" = 15,
      "male"   = 7
    )
  ) +
  
  scale_color_manual(
    values = c(
      "female" = cols_all[9],
      "male"   = cols_all[10]
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(8, 22, by = 2),
    labels = function(x) sprintf("%02d:00", x)
  ) +
  
  scale_y_continuous(
    labels = function(x) sprintf("%.3f", x)
  ) +
  
  theme_classic() +
  
  labs(
    x = "Time of day",
    y = "Bodyweight (g)",
    color = "Sex",
    shape = "Sex"
  )

___________________________________________________________
_____________________________________________________________________
#### Cas: Control ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(stringr)
library(readr)
library(lubridate)

# Build Cas control dataset

cas_control <- CicTen %>%
  
  clean_names() %>%
  
  mutate(across(where(is.character), str_squish)) %>%
  
  mutate(
    number = parse_number(as.character(number))
  ) %>%
  
  mutate(
    across(
      starts_with("weight"),
      ~ as.numeric(str_replace(as.character(.), ",", "."))
    )
  ) %>%
  
  filter(
    species == "Cas",
    purpose == "C. Weight"
  ) %>%
  
  filter(!number %in% remove_ids_cas) %>%
  
  select(
    -notes,
    -pressure_bomb_bar,
    -starts_with("time_slot")
  ) %>%
  
  rename(
    date_1 = date_of_action,
    date_2 = date_of_second_action,
    date_3 = date_of_third_action,
    
    time_1 = time_of_first_action,
    time_2 = time_of_second_action,
    time_3 = time_of_third_action
  ) %>%
  
  transmute(
    number,
    species,
    sex,
    purpose,
    sampling_date,
    
    w1 = weight_1,
    w2 = weight_2,
    
    action_1 = action_1,
    date_1   = date_1,
    time_1   = time_1,
    weight_1 = w1,
    
    action_2 = action_2,
    date_2   = date_2,
    time_2   = time_2,
    weight_2 = NA_real_,
    
    action_3 = action_3,
    date_3   = date_3,
    time_3   = time_3,
    weight_3 = w2
  ) %>%
  
  pivot_longer(
    cols = c(
      action_1, date_1, time_1, weight_1,
      action_2, date_2, time_2, weight_2,
      action_3, date_3, time_3, weight_3
    ),
    names_to = c(".value", "event"),
    names_pattern = "(action|date|time|weight)_(\\d)"
  ) %>%
  
  mutate(
    event = as.integer(event)
  ) %>%
  
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
    weight
  )

View(cas_control)

write_csv(cas_control, "data/cas_control.csv")

# Create plotting dataset

plot_cas_control <- cas_control %>%
  mutate(
    time = as.character(time),
    time = str_trim(time),
    time = if_else(
      str_detect(time, "^\\d{1,2}:\\d{2}$"),
      paste0(time, ":00"),
      time
    ),
    time_h = hour(hms(time)) +
      minute(hms(time)) / 60 +
      second(hms(time)) / 3600
  ) %>%
  filter(!is.na(time_h), !is.na(weight))

# Optional check
plot_cas_control %>%
  select(number, event, action, time, time_h, weight)


# Plot settings

cols_all <- palette.colors(12, palette = "Paired")

time_blocks <- data.frame(
  period = c("morning", "afternoon"),
  xmin = c(8, 12),
  xmax = c(11, 17),
  ymin = -Inf,
  ymax = Inf
)

bg_cols <- c(
  "morning"   = cols_all[5],
  "afternoon" = cols_all[9]
)

# Plot using cas_control data


fig_weight_time_C <- ggplot() +
  
  geom_rect(
    data = time_blocks,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = period
    ),
    alpha = 0.2,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  
  geom_line(
    data = plot_cas_control,
    aes(
      x = time_h,
      y = weight,
      group = number
    ),
    linewidth = 0.5,
    alpha = 0.7,
    color = "grey35"
  ) +
  
  geom_point(
    data = plot_cas_control,
    aes(
      x = time_h,
      y = weight,
      color = sex,
      shape = sex
    ),
    size = 2.8,
    alpha = 0.9
  ) +
  
  scale_color_manual(
    values = c(
      "female" = cols_all[9],
      "male"   = cols_all[10]
    )
  ) +
  
  scale_fill_manual(values = bg_cols) +
  
  scale_shape_manual(
    values = c(
      "female" = 15,
      "male"   = 7
    )
  ) +
  
  scale_y_continuous(
    labels = function(x) sprintf("%.3f", x)
  ) +
  
  scale_x_continuous(
    breaks = seq(8, 18, by = 2),
    labels = function(x) sprintf("%02d:00", x)
  ) +
  
  labs(
    x = "Time of day",
    y = "Bodyweight (g)",
    color = "Sex",
    shape = "Sex"
  ) +
  
  theme_classic()

fig_weight_time_C

#### stacking cas and control ####

library(patchwork)

# Shared axis settings
y_range <- range(plot_weight$weight, na.rm = TRUE)

y_pad <- diff(y_range) * 0.03   # ~3% padding

y_limits <- c(
  y_range[1] - y_pad,
  y_range[2] + y_pad
)

# Panel A
fig_weight_time_A <- fig_weight_time +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
    limits = range(x_breaks)
  ) +
  coord_cartesian(ylim = y_limits) +
  labs(title = "A", x = NULL) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    axis.text.x = element_blank(),
    axis.ticks.x = element_line(),   # keep ticks for alignment
    legend.position = "none",
    plot.margin = margin(5.5, 10, 2, 5.5)
  )

# Panel B
fig_weight_time_B <- fig_weight_time_C +
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
    limits = range(x_breaks)
  ) +
  coord_cartesian(ylim = y_limits) +
  labs(title = "B", x = "Time window") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.margin = margin(2, 10, 5.5, 5.5),
    legend.position = "right"
  )

# Stack panels
fig_weight_stacked <- fig_weight_time_A / fig_weight_time_B +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "none"
  )

fig_weight_stacked

#### Control: loss in percent ####
 library(dplyr)
 library(ggplot2)
 
 # calculate percentage bodyweight loss per individual
 bw_loss <- cas_control %>%
   filter(action %in% c("lab", "bodyweight")) %>%
   filter(!is.na(weight)) %>%
   arrange(number, event) %>%
   group_by(number, species, sex) %>%
   summarise(
     weight_start = first(weight),
     weight_end   = last(weight),
     .groups = "drop"
   ) %>%
   mutate(
     loss_g = weight_start - weight_end,
     loss_percent = (loss_g / weight_start) * 100
   )
 
 # view individual values
 bw_loss
 
 # overall summary
 bw_loss %>%
   summarise(
     n = n(),
     mean_loss = mean(loss_percent, na.rm = TRUE),
     sd_loss   = sd(loss_percent, na.rm = TRUE),
     min_loss  = min(loss_percent, na.rm = TRUE),
     max_loss  = max(loss_percent, na.rm = TRUE)
   )
 
 # summary by sex
 bw_loss %>%
   group_by(sex) %>%
   summarise(
     n = n(),
     mean_loss = mean(loss_percent, na.rm = TRUE),
     sd_loss   = sd(loss_percent, na.rm = TRUE),
     min_loss  = min(loss_percent, na.rm = TRUE),
     max_loss  = max(loss_percent, na.rm = TRUE),
     .groups = "drop"
   )
 
 # check for unexpected weight gain
 bw_loss %>%
   filter(loss_percent < 0)
 
 # plot
 cols_all <- palette.colors(12, palette = "Paired")
 
 fig_bw_loss <- ggplot(
   bw_loss,
   aes(x = sex, y = loss_percent, color = sex, shape = sex)
 ) +
   geom_jitter(width = 0.1, size = 3, alpha = 0.9) +
   geom_boxplot(alpha = 0.2, outlier.shape = NA, width = 0.5) +
   scale_color_manual(
     values = c(
       "female" = cols_all[9],
       "male"   = cols_all[10]
     )
   ) +
   scale_shape_manual(
     values = c(
       "female" = 15,
       "male"   = 7
     )
   ) +
   labs(
     x = "Sex",
     y = "Bodyweight loss (%)",
     color = "Sex",
     shape = "Sex"
   ) +
   theme_classic()
 
 fig_bw_loss

#### combining control and percent ####
 
 library(dplyr)
 library(ggplot2)
 library(patchwork)
 
 cols_all <- palette.colors(12, palette = "Paired")
 
 # Choose what to include:
 # Option 1: females only
 plot_cas_control_use <- plot_cas_control %>%
   filter(sex == "female")
 
 cas_control_use <- cas_control %>%
   filter(sex == "female")
 
 # Option 2: female + male combined
 # plot_cas_control_use <- plot_cas_control
 # cas_control_use <- cas_control
 
 
##Calculate bodyweight loss ##
 
 bw_loss <- cas_control_use %>%
   filter(action %in% c("lab", "bodyweight")) %>%
   filter(!is.na(weight)) %>%
   arrange(number, event) %>%
   group_by(number, species, sex) %>%
   summarise(
     weight_start = first(weight),
     weight_end   = last(weight),
     .groups = "drop"
   ) %>%
   mutate(
     loss_g = weight_start - weight_end,
     loss_percent = (loss_g / weight_start) * 100
   )
 
 
## A) Bodyweight over time ##
 
 fig_weight_time_C <- ggplot() +
   
   geom_rect(
     data = time_blocks,
     aes(
       xmin = xmin,
       xmax = xmax,
       ymin = ymin,
       ymax = ymax,
       fill = period
     ),
     alpha = 0.2,
     inherit.aes = FALSE,
     show.legend = FALSE
   ) +
   
   geom_line(
     data = plot_cas_control_use,
     aes(
       x = time_h,
       y = weight,
       group = number
     ),
     linewidth = 0.5,
     alpha = 0.7,
     color = "grey35"
   ) +
   
   geom_point(
     data = plot_cas_control_use,
     aes(
       x = time_h,
       y = weight
     ),
     size = 2.8,
     alpha = 0.9,
     color = cols_all[9],
     shape = 15
   ) +
   
   scale_fill_manual(values = bg_cols) +
   
   scale_y_continuous(
     labels = function(x) sprintf("%.3f", x)
   ) +
   
   scale_x_continuous(
     breaks = seq(8, 18, by = 2),
     labels = function(x) sprintf("%02d:00", x)
   ) +
   
   labs(
     x = "Time of day",
     y = "Bodyweight (g)"
   ) +
   
   theme_classic()
 
 
## B) Percent bodyweight loss ##
 
 fig_bw_loss <- ggplot(
   bw_loss,
   aes(x = "", y = loss_percent)
 ) +
   geom_boxplot(
     width = 0.35,
     alpha = 0.2,
     outlier.shape = NA,
     color = "black"
   ) +
   geom_jitter(
     width = 0.08,
     size = 3,
     alpha = 0.9,
     color = cols_all[9],
     shape = 15
   ) +
   labs(
     x = NULL,
     y = "Bodyweight loss (%)"
   ) +
   theme_classic()
 
 
## Combined figure ##
 
 fig_control_combined <- fig_weight_time_C + fig_bw_loss +
   plot_annotation(tag_levels = "A")
 
 fig_control_combined

#### Stats ####
 ## 1. Does bodyweight loss differ from 0?
 
 wilcox.test(bw_loss$loss_percent, mu = 0)
 
 # Interpretation:
 # The Wilcoxon signed-rank test was significant
 # (V = 28, p = 0.0156), indicating that percentage
 # bodyweight loss was significantly different from 0.
 # Since the observed mean loss is positive, this means
 # cicadas significantly lost bodyweight during the
 # experimental time window.
 
 
 t.test(bw_loss$loss_percent, mu = 0)
 
 # Interpretation:
 # The one-sample t-test was also significant
 # (t(6) = 5.66, p = 0.0013), confirming that mean
 # percentage bodyweight loss was greater than 0.
 # The mean loss was 3.07%, with a 95% confidence
 # interval from 1.74% to 4.39%.
 # This suggests a clear overall decline in bodyweight.
 
 
 ## 2. Does bodyweight loss differ between sexes?
 
 model_bw <- lm(loss_percent ~ sex, data = bw_loss)
 summary(model_bw)
 
 # Interpretation:
 # In the linear model, the intercept represents females.
 # Female cicadas lost on average 2.70% bodyweight.
 # The coefficient for males was +2.52, meaning males
 # lost on average about 2.52 percentage points more
 # bodyweight than females.
 #
 # However, this sex effect was not statistically significant
 # (p = 0.103), so there is no strong evidence that males
 # and females differed in percentage bodyweight loss.
 #
 # Because sample size is very small (n = 7 total), this
 # result should be interpreted cautiously. The pattern
 # suggests a possible tendency for males to lose more
 # weight, but this cannot be concluded statistically.
 
 ## 3. Group means for easier reporting
 
 bw_loss %>%
   group_by(sex) %>%
   summarise(
     n = n(),
     mean_loss = mean(loss_percent, na.rm = TRUE),
     sd_loss = sd(loss_percent, na.rm = TRUE),
     min_loss = min(loss_percent, na.rm = TRUE),
     max_loss = max(loss_percent, na.rm = TRUE),
     .groups = "drop"
   )
 
 # Interpretation:
 # Use this output to report descriptive statistics.
 # These values show the average percentage bodyweight
 # loss per sex and help describe the direction of the pattern,
 # even though the sex difference was not significant.
 
 
 
 ## 4. Optional non-parametric sex comparison
 
 wilcox.test(loss_percent ~ sex, data = bw_loss)
 
 # Interpretation:
 # This provides a non-parametric test of the sex difference.
 # It is useful because of the very small sample size.
 # If this test is also non-significant, it supports the same
 # conclusion as the linear model: no clear sex difference.
 
 
 ## 5. Simple effect size without extra packages
 
 #install.packages("effsize") 
 library(effsize)
 
 cohen.d(loss_percent ~ sex, data = bw_loss)
 
 # Interpretation:
 # Cohen's d gives the size of the sex difference.
 # This is useful because with such a small sample,
 # biologically meaningful differences may fail to
 # reach statistical significance.
 # Report this together with the p-value if relevant.
 
#### Overall feedback####
 
 ### Overall bodyweight loss
 
 # Percentage bodyweight loss differed significantly from zero
 # (Wilcoxon signed-rank test: V = 28, p = 0.0156).
 # This result was supported by a one-sample t-test
 # (t(6) = 5.66, p = 0.0013).
 
 # On average, cicadas lost 3.07% of their bodyweight
 # (95% CI: 1.74–4.39%).
 
 # Interpretation:
 # Cicadas show a clear and statistically significant
 # reduction in bodyweight over the experimental period.
 

 ### Sex differences
 
 # A linear model suggested that males lost more bodyweight
 # than females (estimate = +2.52 percentage points).
 
 # However, this effect was not statistically significant
 # (F(1,5) = 3.97, p = 0.103).
 
 # A non-parametric Wilcoxon rank-sum test also found no
 # significant difference between sexes (W = 0, p = 0.286).
 
 # Interpretation:
 # There is no statistical evidence for a difference in
 # bodyweight loss between males and females.
 #> only one male (!)
 

 ### Descriptive statistics
 
 
 # Females (n = 6) lost on average 2.70% bodyweight
 # (SD = 1.17, range: 1.58–4.39%).
 
 # The single male individual lost 5.23% bodyweight.
 
 # Interpretation:
 # While the male showed higher weight loss, this observation
 # is based on only one individual and therefore cannot be
 # generalized or statistically tested.


#### weight change Cas vs Control ####
 
 library(tidyverse)
 library(lubridate)
 library(hms)
 
 cas <- read_csv("data/cicten_tidy_cas.csv") %>%
   mutate(group = "CAS")
 
 control <- read_csv("data/cas_control.csv") %>%
   mutate(group = "Control")
 
 bodyweight_step <- bind_rows(cas, control) %>%
   filter(!is.na(weight), !is.na(time), !is.na(date)) %>%
   mutate(
     date = dmy(date),
     time = as_hms(time),
     time_h = as.numeric(time) / 3600,
     datetime = as.POSIXct(date) + as.numeric(time)
   ) %>%
   arrange(group, number, date, time_h) %>%
   group_by(group, number, date) %>%
   mutate(
     prev_time = lag(time),
     prev_weight = lag(weight),
     step_change_g = weight - prev_weight,
     step_change_pct = (weight - prev_weight) / prev_weight * 100,
     time_window = paste0(prev_time, " to ", time)
   ) %>%
   ungroup()
 
 step_table <- bodyweight_step %>%
   filter(!is.na(step_change_pct)) %>%
   select(
     group, number, species, sex, date,
     time_window,
     prev_weight,
     weight,
     step_change_g,
     step_change_pct
   )
 
 step_table

 summary_table <- step_table %>%
   group_by(group, time_window) %>%
   summarise(
     n = n(),
     mean_pct_change = mean(step_change_pct, na.rm = TRUE),
     sd_pct_change = sd(step_change_pct, na.rm = TRUE),
     se_pct_change = sd_pct_change / sqrt(n),
     .groups = "drop"
   )
 
 summary_table 

## plot 
 library(dplyr)
 library(ggplot2)
 library(hms)
 
 plot_change <- bodyweight_step %>%
   filter(group == "CAS") %>%   
   arrange(number, date, time_h) %>%
   group_by(number, date) %>%
   mutate(
     step_change_pct = (weight - lag(weight)) / lag(weight) * 100,
     step_change_pct = ifelse(row_number() == 1, 0, step_change_pct)  # ✅ first = 0%
   ) %>%
   ungroup()
 
 cols_all <- palette.colors(12, palette = "Paired")
 
 bg_cols <- c(
   "morning"   = cols_all[5],
   "afternoon" = cols_all[9],
   "evening"   = cols_all[3]
 )
 
 time_blocks <- data.frame(
   period = c("morning", "afternoon", "evening"),
   xmin = c(8, 12, 18),
   xmax = c(11, 17, 20),
   ymin = -Inf,
   ymax = Inf
 )
 
 fig_weight_change <- ggplot() +
   
   geom_rect(
     data = time_blocks,
     aes(
       xmin = xmin,
       xmax = xmax,
       ymin = ymin,
       ymax = ymax,
       fill = period
     ),
     alpha = 0.22,
     inherit.aes = FALSE,
     show.legend = FALSE
   ) +
   
   geom_hline(yintercept = 0, linetype = "dashed") +
   
   geom_line(
     data = plot_change,
     aes(
       x = time_h,
       y = step_change_pct,
       group = number
     ),
     linewidth = 0.5,
     alpha = 0.7,
     color = "grey35"
   ) +
   
   geom_point(
     data = plot_change,
     aes(
       x = time_h,
       y = step_change_pct,
       shape = sex
     ),
     size = 1.7,
     stroke = 0.8,
     color = "black",
     fill = "black"
   ) +
   
   scale_fill_manual(values = bg_cols) +
   
   scale_shape_manual(
     values = c(
       "female" = 15,
       "male"   = 7
     )
   ) +
   
   scale_y_continuous(
     labels = function(x) paste0(sprintf("%.1f", x), "%")
   ) +
   
   scale_x_continuous(
     breaks = seq(6, 22, by = 2),
     labels = function(x) sprintf("%02d:00", x)
   ) +
   
   labs(
     x = "Time of day",
     y = "Stepwise bodyweight change (%)",
     shape = "Sex"
   ) +
   
   theme_classic()
 
 fig_weight_change
 
 ## Plotting control in percent 
 
 
 plot_cas_control_pct <- plot_cas_control_use %>%
   arrange(number, time_h) %>%
   group_by(number) %>%
   mutate(
     start_weight = first(weight),
     weight_change_pct = (weight - start_weight) / start_weight * 100
   ) %>%
   ungroup()
 
 fig_control_pct <- ggplot() +
   
   geom_rect(
     data = time_blocks,
     aes(
       xmin = xmin,
       xmax = xmax,
       ymin = ymin,
       ymax = ymax,
       fill = period
     ),
     alpha = 0.2,
     inherit.aes = FALSE,
     show.legend = FALSE
   ) +
   
   geom_hline(yintercept = 0, linetype = "dashed") +
   
   geom_line(
     data = plot_cas_control_pct,
     aes(
       x = time_h,
       y = weight_change_pct,
       group = number
     ),
     linewidth = 0.5,
     alpha = 0.7,
     color = "grey35"
   ) +
   
   geom_point(
     data = plot_cas_control_pct,
     aes(
       x = time_h,
       y = weight_change_pct
     ),
     size = 1,5,
     alpha = 0.9,
     color = "black",
     shape = 15
   ) +
   
   scale_fill_manual(values = bg_cols) +
   
   scale_y_continuous(
     labels = function(x) paste0(sprintf("%.1f", x), "%")
   ) +
   
   scale_x_continuous(
     breaks = seq(8, 18, by = 2),
     labels = function(x) sprintf("%02d:00", x)
   ) +
   
   labs(
     x = "Time of day",
     y = "Bodyweight change (%)"
   ) +
   
   theme_classic()
 
 fig_control_pct
 
 library(patchwork)
 
 ## A) CAS plot (top)
 
 fig_cas_A <- fig_weight_change +
   labs(title = "A", x = NULL) +  
   theme(
     plot.title = element_text(face = "bold", size = 14, hjust = 0),
     plot.margin = margin(5.5, 5.5, 2, 5.5)   
   )
 
 
 ## B) Control plot (bottom)
 
 fig_control_B <- fig_control_pct +
   labs(title = "B", x = "Time of day") +
   theme(
     plot.title = element_text(face = "bold", size = 14, hjust = 0),
     plot.margin = margin(2, 5.5, 5.5, 5.5)   
   )
 
 
 ## Combined figure
 
 fig_cas_control_pct <- fig_cas_A / fig_control_B +
   plot_layout(heights = c(1, 1))
 
 fig_cas_control_pct
 
 
 #### Analysis####
 
 
 ## Cas 
 analysis_rate <- bodyweight_step %>%
   arrange(group, number, date, time_h) %>%
   group_by(group, number, date) %>%
   mutate(
     prev_time_h = lag(time_h),
     prev_time   = lag(time),
     
     delta_time_h = time_h - prev_time_h,
     
     step_change_pct = (weight - lag(weight)) / lag(weight) * 100,
     
     change_per_hour = step_change_pct / delta_time_h,
     
     interval = paste0(prev_time, " → ", time)  
   ) %>%
   ungroup() %>%
   filter(!is.na(delta_time_h), delta_time_h > 0)

 #Table
 rate_table <- analysis_rate %>%
   select(
     group,
     number,
     sex,
     date,
     interval,         
     delta_time_h,
     step_change_pct,
     change_per_hour
   )
 
 rate_table

 View(rate_table)
 write_csv(rate_table, "data/rate_table.csv")

 ## Control 
 
 control_rate <- plot_cas_control_use %>%
   arrange(number, time_h) %>%
   group_by(number) %>%
   mutate(
     prev_time_h = lag(time_h),
     prev_time = lag(time),
     prev_weight = lag(weight),
     
     delta_time_h = time_h - prev_time_h,
     
     step_change_g = weight - prev_weight,
     step_change_pct = (weight - prev_weight) / prev_weight * 100,
     change_per_hour = step_change_pct / delta_time_h,
     
     interval = paste0(prev_time, " → ", time)
   ) %>%
   ungroup() %>%
   filter(!is.na(delta_time_h), delta_time_h > 0)
 
 # Table
 control_rate_table <- control_rate %>%
   select(
     number,
     sex,
     interval,
     delta_time_h,
     prev_weight,
     weight,
     step_change_g,
     step_change_pct,
     change_per_hour
   )
 
 control_rate_table
 
 View(control_rate_table)
 write_csv(control_rate_table, "data/control_rate_table.csv")

 ## Stats for cas 
 
 cas_rate_summary <- analysis_rate %>%
   filter(group == "CAS") %>%
   summarise(
     n = n(),
     mean_rate = mean(change_per_hour, na.rm = TRUE),
     sd_rate = sd(change_per_hour, na.rm = TRUE),
     min_rate = min(change_per_hour, na.rm = TRUE),
     max_rate = max(change_per_hour, na.rm = TRUE)
   )
 
cas_rate_summary
# PER HOUR
#n mean_rate sd_rate min_rate max_rate
#<int>     <dbl>   <dbl>    <dbl>    <dbl>
#   1    54      1.51    2.88    -2.16     9.89

wilcox.test(
  analysis_rate$change_per_hour[analysis_rate$group == "CAS"],
  mu = 0
)

#Wilcoxon signed rank test with continuity correction

#data:  analysis_rate$change_per_hour[analysis_rate$group == "CAS"]
#V = 1062, p-value = 0.006021
#alternative hypothesis: true location is not equal to 0
# _> there is a sistematic weight change per hour 

cas_gain_loss <- analysis_rate %>%
  filter(group == "CAS") %>%
  mutate(
    direction = ifelse(change_per_hour > 0, "gain", "loss")
  ) %>%
  group_by(direction) %>%
  summarise(
    mean_rate = mean(change_per_hour, na.rm = TRUE),
    n = n()
  )

cas_gain_loss 
#direction mean_rate     n
#<chr>         <dbl>   <int>
#1 gain          3.64     29
#2 loss         -0.961    25
# -> more gain than loss 

model_time_simple <- lm(
  change_per_hour ~ interval,
  data = analysis_rate %>% filter(group == "CAS")
)

anova(model_time_simple)

#Analysis of Variance Table

#Response: change_per_hour
#Df Sum Sq Mean Sq F value    Pr(>F)    
#interval   2 272.98 136.489  41.576 1.947e-11 ***
#Residuals 51 167.43   3.283                      
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# -> it differs 
  
  model_time_aov <- aov(
    change_per_hour ~ interval,
    data = analysis_rate %>% filter(group == "CAS")
  )

TukeyHSD(model_time_aov)
# -> all time windows are significantly different to each other 