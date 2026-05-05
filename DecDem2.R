library(readr)
Input <- read_delim("data/Input.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)
View(Input)

library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(tibble)

#### Color theme for this paper ####

cols_all <- palette.colors(12, palette = "Paired")

barplot(rep(1, length(cols_all)),
        col = cols_all,
        names.arg = 1:length(cols_all),
        border = NA,
        main = "Paired palette (indexed)")

time_fill_cols <- c(
  "Morning"   = cols_all[5],
  "Afternoon" = cols_all[9],
  "Evening"   = cols_all[3],
  "Night"     = cols_all[1]
)

time_point_cols <- c(
  "Morning"   = cols_all[6],
  "Afternoon" = cols_all[10],
  "Evening"   = cols_all[4],
  "Night"     = cols_all[2]
)


#### Removed IDs ####
# these iDs are removed becuase individual died in field or lost their pipette tip 

remove_ids <- c(
  24,
  25,
  30,
  31,
  39:44,
  47, 48,
  57:61,
  63, 68,
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


#### Clean corrected Input sheet ####

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
    
    weight_1 = as.numeric(str_replace(as.character(weight_1), ",", ".")),
    weight_2 = as.numeric(str_replace(as.character(weight_2), ",", ".")),
    weight_3 = as.numeric(str_replace(as.character(weight_3), ",", ".")),
    duration = as.numeric(str_replace(as.character(duration), ",", ".")),
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
    !is.na(input_hr),
    !is.na(time_window)
  ) %>%
  droplevels()

DecDem_ <- input_clean

write_csv(input_clean, "input_clean_corrected.csv")
write_csv(DecDem_, "DecDem_input_corrected.csv")


#### Figure 1: species by time window, faceted ####

fig_species_by_time <- ggplot(
  DecDem_,
  aes(x = species, y = input_hr)
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
  facet_wrap(~ time_window, nrow = 2, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  scale_color_manual(values = time_point_cols, drop = FALSE) +
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


#### Figure 2: fully separate plots by time window ####

plot_morning <- DecDem_ %>% filter(time_window == "Morning")
plot_noon    <- DecDem_ %>% filter(time_window == "Afternoon")
plot_evening <- DecDem_ %>% filter(time_window == "Evening")
plot_night   <- DecDem_ %>% filter(time_window == "Night")

y_range <- range(DecDem_$input_hr, na.rm = TRUE)
y_limits <- y_range + c(-0.1, 0.1) * diff(y_range)

make_plot <- function(data, title_label, fill_col, line_col, y_limits) {
  ggplot(data, aes(x = species, y = input_hr)) +
    
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
    
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.12,
      color = "black",
      linewidth = 0.5
    ) +
    
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

fig_separate <- (pA | pB) / (pC | pD)
fig_separate


#### Figure 3: raw points + mean ± SE, species separated within time windows ####

plot_data_fig1 <- DecDem_ %>%
  mutate(
    x_cat = as.numeric(time_window),
    x_species = x_cat + ifelse(species == "Dec", -0.15, 0.15)
  )

fig1 <- ggplot(plot_data_fig1, aes(x = x_cat, y = input_hr)) +
  
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
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
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
    legend.position = c(1.03, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    plot.margin = margin(5.5, 45, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

fig1


#### Figure 4: raw points + mean ± SE, Dec and Dem pooled ####

plot_data_pooled <- DecDem_ %>%
  mutate(
    x_cat = as.numeric(time_window)
  )

fig1_pooled <- ggplot(plot_data_pooled, aes(x = x_cat, y = input_hr)) +
  
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
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
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


#### Figure 5: violin + raw points + mean ± SE, species separated ####

plot_data_fig2 <- DecDem_

fig2 <- ggplot(plot_data_fig2, aes(x = time_window, y = input_hr)) +
  
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
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
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
    legend.position = c(1.03, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    plot.margin = margin(5.5, 45, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

fig2


#### Figure 6: violin + raw points + mean ± SE, Dec and Dem pooled ####

plot_data_fig2_pooled <- DecDem_

y_max <- max(plot_data_fig2_pooled$input_hr, na.rm = TRUE)

fig2_pooled <- ggplot(plot_data_fig2_pooled, aes(x = time_window, y = input_hr)) +
  
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
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
  labs(
    x = "Time window",
    y = "Input rate (g/hr)"
  ) +
  
  guides(
    color = "none",
    fill = "none"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  theme_classic()

fig2_pooled


#### Statistics ####

#### 1. Overall summary ####

DecDem_ %>%
  summarise(
    n = n(),
    min_input = min(input_hr, na.rm = TRUE),
    max_input = max(input_hr, na.rm = TRUE),
    mean_input = mean(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE)
  )


#### 2. Summary by species ####

DecDem_ %>%
  group_by(species) %>%
  summarise(
    n = n(),
    min_input = min(input_hr, na.rm = TRUE),
    max_input = max(input_hr, na.rm = TRUE),
    mean_input = mean(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE),
    .groups = "drop"
  )


#### 3. Summary by time window ####

DecDem_ %>%
  group_by(time_window) %>%
  summarise(
    n = n(),
    min_input = min(input_hr, na.rm = TRUE),
    max_input = max(input_hr, na.rm = TRUE),
    mean_input = mean(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE),
    .groups = "drop"
  )


#### 4. Summary by species and time window ####

DecDem_ %>%
  group_by(species, time_window) %>%
  summarise(
    n = n(),
    min_input = min(input_hr, na.rm = TRUE),
    max_input = max(input_hr, na.rm = TRUE),
    mean_input = mean(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE),
    .groups = "drop"
  )


#### 5. Main model ####
#### This is the main statistical test matching your main biological question:
#### Does input rate differ by species, time window, or their interaction?

model <- lm(input_hr ~ species * time_window, data = DecDem_)

anova(model)
summary(model)

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


#### 6. Pairwise tests between time windows ####
#### Use this only as follow-up because the main model tests the overall pattern.

pairwise.wilcox.test(
  x = DecDem_$input_hr,
  g = DecDem_$time_window,
  p.adjust.method = "BH"
)


#### 7. Optional sex model ####
#### Keep this as a secondary check only.
#### Do not make it the main result unless sex is part of your research question.

model_sex <- lm(input_hr ~ species * time_window + sex, data = DecDem_)

anova(model_sex)
summary(model_sex)


#### 8. Optional non-parametric species test ####
#### This tests only Dec vs Dem overall, ignoring time window.

wilcox.test(input_hr ~ species, data = DecDem_)


#### 9. Optional species tests within each time window ####
#### These match Figure 1 / faceted species-by-time plot.

DecDem_ %>%
  group_by(time_window) %>%
  summarise(
    p_value = wilcox.test(input_hr ~ species)$p.value,
    .groups = "drop"
  )

#-------------------------------------------------------------------------------
#### CicTen Input Rate + Plots - WITHOUT NEGATIVE VALUES ####

library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
library(tibble)
library(ggpubr)
library(patchwork)
library(rstatix)

#### Removed IDs ####

remove_ids <- c(
  24,
  25,
  30,
  31,
  39:44,
  47, 48,
  57:61,
  63, 68,
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


#### Clean corrected Input sheet ####
#### input_hr is taken directly from Input.csv.
#### No recalculation of input rate is done here.

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
    
    weight_1 = as.numeric(str_replace(as.character(weight_1), ",", ".")),
    weight_2 = as.numeric(str_replace(as.character(weight_2), ",", ".")),
    weight_3 = as.numeric(str_replace(as.character(weight_3), ",", ".")),
    duration = as.numeric(str_replace(as.character(duration), ",", ".")),
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
    !is.na(input_hr),
    !is.na(time_window)
  ) %>%
  droplevels()


#### Positive-only data ####

plot_data_pos <- input_clean %>%
  filter(input_hr >= 0) %>%
  mutate(
    species = factor(species, levels = c("Dec", "Dem")),
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    
    x_cat = case_when(
      time_window == "Morning"   ~ 1,
      time_window == "Afternoon" ~ 2,
      time_window == "Evening"   ~ 3,
      time_window == "Night"     ~ 4
    ),
    
    x_species = x_cat + ifelse(species == "Dec", -0.15, 0.15)
  )

plot_data_pos

write_csv(plot_data_pos, "Input_positive_only_corrected.csv")


#### Figure 1: species comparison within each time window ####

fig_species_by_time_pos <- ggplot(
  plot_data_pos,
  aes(x = species, y = input_hr)
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
  
  facet_wrap(~ time_window, nrow = 2, drop = FALSE) +
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
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


#### Figure 2: fully separate positive-only plots ####

plot_morning_pos   <- plot_data_pos %>% filter(time_window == "Morning")
plot_afternoon_pos <- plot_data_pos %>% filter(time_window == "Afternoon")
plot_evening_pos   <- plot_data_pos %>% filter(time_window == "Evening")
plot_night_pos     <- plot_data_pos %>% filter(time_window == "Night")

y_range_pos <- range(plot_data_pos$input_hr, na.rm = TRUE)
y_limits_pos <- y_range_pos + c(-0.1, 0.1) * diff(y_range_pos)

make_plot_pos <- function(data, title_label, fill_col, line_col, y_limits) {
  ggplot(data, aes(x = species, y = input_hr)) +
    
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
    
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.12,
      color = "black",
      linewidth = 0.5
    ) +
    
    scale_shape_manual(values = c("Dec" = 16, "Dem" = 17)) +
    
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

pApos <- make_plot_pos(plot_morning_pos,   "A) Morning",   cols_all[5], cols_all[6], y_limits_pos)
pBpos <- make_plot_pos(plot_afternoon_pos, "B) Afternoon", cols_all[9], cols_all[10], y_limits_pos)
pCpos <- make_plot_pos(plot_evening_pos,   "C) Evening",   cols_all[3], cols_all[4], y_limits_pos)
pDpos <- make_plot_pos(plot_night_pos,     "D) Night",     cols_all[1], cols_all[2], y_limits_pos)

pApos
pBpos
pCpos
pDpos

fig_separate_pos <- (pApos | pBpos) / (pCpos | pDpos)
fig_separate_pos


#### Figure 3: raw points + mean ± SE, species separated ####

fig1_pos <- ggplot(plot_data_pos, aes(x = x_cat, y = input_hr)) +
  
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
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
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
    legend.position = c(1.03, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    plot.margin = margin(5.5, 45, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

fig1_pos


#### Figure 4: raw points + mean ± SE, Dec and Dem pooled ####

fig1_pos_pooled <- ggplot(plot_data_pos, aes(x = x_cat, y = input_hr)) +
  
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
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
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

fig1_pos_pooled


#### Figure 5: violin + raw points + mean ± SE, species separated ####

fig2_pos <- ggplot(plot_data_pos, aes(x = time_window, y = input_hr)) +
  
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
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
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
    legend.position = c(1.03, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    plot.margin = margin(5.5, 45, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

fig2_pos


#### Figure 6: violin + raw points + mean ± SE, Dec and Dem pooled ####

y_max_pos <- max(plot_data_pos$input_hr, na.rm = TRUE)

fig2_pos_pooled <- ggplot(plot_data_pos, aes(x = time_window, y = input_hr)) +
  
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
      y_max_pos * 1.05,
      y_max_pos * 1.12,
      y_max_pos * 1.19,
      y_max_pos * 1.26
    )
  ) +
  
  scale_color_manual(values = time_point_cols, drop = FALSE) +
  scale_fill_manual(values = time_fill_cols, drop = FALSE) +
  
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


#### Statistics: positive-only data ####

#### 1. Descriptive summaries ####

plot_data_pos %>%
  summarise(
    n = n(),
    min_input = min(input_hr, na.rm = TRUE),
    max_input = max(input_hr, na.rm = TRUE),
    mean_input = mean(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE)
  )

plot_data_pos %>%
  group_by(species) %>%
  summarise(
    n = n(),
    min_input = min(input_hr, na.rm = TRUE),
    max_input = max(input_hr, na.rm = TRUE),
    mean_input = mean(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE),
    .groups = "drop"
  )

plot_data_pos %>%
  group_by(time_window) %>%
  summarise(
    n = n(),
    min_input = min(input_hr, na.rm = TRUE),
    max_input = max(input_hr, na.rm = TRUE),
    mean_input = mean(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE),
    .groups = "drop"
  )

plot_data_pos %>%
  group_by(species, time_window) %>%
  summarise(
    n = n(),
    mean_input = mean(input_hr, na.rm = TRUE),
    median_input = median(input_hr, na.rm = TRUE),
    sd_input = sd(input_hr, na.rm = TRUE),
    .groups = "drop"
  )


#### 2. Main model: species, time window, and interaction ####
#### This checks whether positive input rate differs by species,
#### time window, or species-specific time pattern.

model_pos <- lm(input_hr ~ species * time_window, data = plot_data_pos)

anova(model_pos)
summary(model_pos)

par(mfrow = c(2, 2))
plot(model_pos)
par(mfrow = c(1, 1))


#### 3. Main non-parametric test across time windows ####
#### Important because positive-only input rates are usually non-normal/skewed.

kruskal_pos <- kruskal.test(input_hr ~ time_window, data = plot_data_pos)
kruskal_pos


#### 4. Pairwise time-window comparisons ####
#### Use this only if Kruskal-Wallis is significant or if you want planned contrasts.

pairwise_time_pos <- plot_data_pos %>%
  pairwise_wilcox_test(
    input_hr ~ time_window,
    p.adjust.method = "BH"
  )

pairwise_time_pos


#### 5. Species differences within each time window ####
#### This matches the faceted species plots.

species_tests_pos <- plot_data_pos %>%
  group_by(time_window) %>%
  summarise(
    p_value = tryCatch(
      wilcox.test(input_hr ~ species)$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

species_tests_pos
#--------------------------------------------------------------------------------
