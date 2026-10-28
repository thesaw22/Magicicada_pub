# "Sucking to Survive? Adult 17-year periodical cicadas (Magicicada spp.) exhibit low rates of xylem ingestion"
# Year: 2026
# Script author: Theresa Weinspach
# Manuscript authors:  Theresa Weinspach, Leigh Boardman, and Phillip G. D. Matthews
# Purpose: recreate the paper data, figures, summaries and statistical tests.
# Run from the CicTen RStudio project root. Put the four source files at:
# data/CicTen.csv, data/Input.csv, data/Weather data.csv,
# data/pressure_tidy.csv. The original pressure-chamber raw sheet was not
# supplied; pressure_tidy.csv is consequently the earliest available source.
# All generated tables, figures and a package-version record go in output/.

#-# Chapter 1: Packages and reproducibility ####

# R == 4.3.1
library(readr)      #2.1.4
library(dplyr)      #1.1.4
library(tidyr)      #1.3.1
library(stringr)    #1.5.0
library(janitor)    #2.2.1
library(lubridate)  #1.9.3
library(ggplot2)    #3.5.2
library(ggpubr)     #0.6.1
library(patchwork)  #1.2.0
library(rstatix)    #0.7.2
library(logistf)    #1.26.1
library(lme4)       #2.0.1
library(lmerTest)   #3.1.3
library(emmeans)    #1.11.1

# Citations:
citation() #for R citation
citation("rstatix")
citation("lme4")
citation("lmerTest")
citation("emmeans")

dir.create("output", recursive = TRUE, showWarnings = FALSE)
packages_used <- c("readr", "dplyr", "tidyr", "stringr", "janitor",
                   "lubridate", "ggplot2", "ggpubr", "patchwork", "rstatix",
                   "logistf", "lme4", "lmerTest", "emmeans")
writeLines(c(paste("R ==", getRversion()),
             paste(packages_used,
                   vapply(packages_used, function(x) as.character(packageVersion(x)), ""),
                   sep = " == ")),
           "output/package_versions.txt")

to_decimal <- function(x) as.numeric(str_replace(as.character(x), ",", "."))
read_source <- function(path) {
  read_delim(path, delim = ";", col_types = cols(.default = col_character()),
             trim_ws = TRUE, show_col_types = FALSE) %>%
    clean_names() %>%
    mutate(across(where(is.character), ~ na_if(str_squish(.x), "")))
}
save_table <- function(x, filename) write_csv(x, file.path("output", filename))
save_plot <- function(x, filename, width, height) {
  ggsave(file.path("output", filename), x, width = width, height = height,
         dpi = 300)
}
stopifnot(all(file.exists(c("data/CicTen.csv", "data/Input.csv",
                             "data/Weather data.csv", "data/pressure_tidy.csv"))))

#----------------------------
#-## Chapter 2: Original CicTen sheet to tidy event tables ####

# The original sheet stores several actions in one row. Each block below maps
# one action to its date, time and mass, giving one row per specimen/event.
# Read weights as text first because the source uses decimal commas. ID is
# a specimen key, not a row index. Preserve the recorded times unchanged.

raw <- read_source("data/CicTen.csv") %>%
  mutate(number = parse_number(number),
         across(starts_with("weight_"), to_decimal),
         species = factor(species, levels = c("Cas", "Dec", "Dem"))) %>%
  filter(!is.na(number))  # 18 blank trailing spreadsheet rows
stopifnot(!anyDuplicated(raw$number), nrow(raw) == 179)

event <- function(data, event_number, action, date, time, mass) {
  data %>% transmute(number, species, sex, purpose,
                     event = event_number,
                     action = .data[[action]], date = .data[[date]],
                     time = .data[[time]], weight_g = .data[[mass]])
}
valid_events <- function(x) x %>%
  filter(!is.na(action) | !is.na(date) | !is.na(time) | !is.na(weight_g)) %>%
  arrange(number, event)

# Dec/Dem: weight 1 belongs to the lab event; field placement has no mass.
# Weight 2 records the excreta at action 3; weight 3 records body mass at action 4. 
# The action 3/4 measurements provide the excretion indicator.
# This makes a lot of sense in the original data sheet CicTen, but is confusing for R (Attention)

dec_dem_raw <- raw %>% filter(species %in% c("Dec", "Dem"), purpose == "Excretion")
dec_dem_tidy <- bind_rows(
  event(dec_dem_raw, 1, "action_1", "date_of_action", "time_of_first_action", "weight_1"),
  event(dec_dem_raw, 2, "action_2", "date_of_second_action", "time_of_second_action", "weight_1") %>%
    mutate(weight_g = NA_real_),
  event(dec_dem_raw, 3, "action_3", "date_of_third_action", "time_of_third_action", "weight_2"),
  event(dec_dem_raw, 4, "action_4", "date_of_fourth_action", "time_of_fourth_action", "weight_3")
) %>% valid_events()
save_table(dec_dem_tidy, "cicten_tidy.csv")

# Cas Weight: the first study mass was measured at the time in the second
# action column, then weights 2–4 follow action 3, 4 and 5 respectively.

cas_exclude <- c(26, 27, 28, 29, 45, 46, 49, 50, 62, 64, 76, 78,
                 124, 134, 156, 169, 170)
cas_raw <- raw %>% filter(species == "Cas", purpose == "Weight",
                          !number %in% cas_exclude)
cas_tidy <- bind_rows(
  event(cas_raw, 1, "action_1", "date_of_action", "time_of_second_action", "weight_1"),
  event(cas_raw, 2, "action_3", "date_of_third_action", "time_of_third_action", "weight_2"),
  event(cas_raw, 3, "action_4", "date_of_fourth_action", "time_of_fourth_action", "weight_3"),
  event(cas_raw, 4, "action_5", "date_of_fith_action", "time_of_fith_action", "weight_4")
) %>% valid_events()
save_table(cas_tidy, "cicten_tidy_cas.csv")

# Control: the first and third action columns hold the two masses; 
# placement at action 2 has no mass. The comparison uses six female controls;
# the one male control is omitted explicitly. Actual elapsed time is retained.

control_raw <- raw %>% filter(species == "Cas", purpose == "C. Weight", sex == "female",
                              !number %in% cas_exclude)
control_tidy <- bind_rows(
  event(control_raw, 1, "action_1", "date_of_action", "time_of_first_action", "weight_1"),
  event(control_raw, 2, "action_2", "date_of_second_action", "time_of_second_action", "weight_1") %>%
    mutate(weight_g = NA_real_),
  event(control_raw, 3, "action_3", "date_of_third_action", "time_of_third_action", "weight_2")
) %>% valid_events()
save_table(control_tidy, "cas_control_tidy.csv")

#----------------------------
#-### Chapter 3: Table 1, sample sizes and starting body mass ####

# All 179 original specimens enter the descriptive table, including animals
# excluded from later experimental analyses. SD describes variation among
# individuals; it is not the SE of a plotted group mean.

table_1 <- raw %>% group_by(species, sex) %>%
  summarise(n = n(), mean_mass_g = mean(weight_1, na.rm = TRUE),
            sd_mass_g = sd(weight_1, na.rm = TRUE), .groups = "drop")
save_table(table_1, "Table1_species_sex_mass.csv")
table_1_paper <- table_1 %>%
  mutate(mass_mean_sd = sprintf("%.2f ± %.2f", mean_mass_g, sd_mass_g)) %>%
  select(species, sex, n, mass_mean_sd) %>%
  pivot_wider(names_from = sex, values_from = c(n, mass_mean_sd)) %>%
  mutate(total = n_female + n_male) %>%
  select(species, n_female, n_male, total,
         mass_mean_sd_female, mass_mean_sd_male)
save_table(table_1_paper, "Table1_publication_layout.csv")
print(table_1)
stopifnot(sum(table_1$n) == 179)

# Body-size evidence underlying the later normalization and Supplement SS1.
print(kruskal.test(weight_1 ~ species, data = raw)) 
print(pairwise.wilcox.test(raw$weight_1, raw$species, p.adjust.method = "bonferroni")) #bonferroni or holm, significance stays the same 

#----------------------------
#-#### Chapter 4: Figure 1, weather and xylem pressure ####

# The station source is Fahrenheit; convert to Celsius. 
# Summaries pool the same clock hour on 20–22 May 2025 (fieldwork days).
# Error bars are observed MIN–MAX ranges,not SE or confidence intervals. 
# UTC here is a timezone-neutral plot clock:
# source clock times are local and are never converted between time zones.

clock <- function(x, day = "2000-01-01")
  as.POSIXct(paste(day, x), format = "%Y-%m-%d %H:%M", tz = "UTC")
weather <- read_source("data/Weather data.csv") %>%
  transmute(datetime = dmy_hm(timestamp, tz = "UTC"),
            temp_f = to_decimal(wireless_et_station_b_temperature)) %>%
  mutate(temp_c = (temp_f - 32) * 5 / 9) %>%
  filter(!is.na(datetime), !is.na(temp_c),
         as.Date(datetime) %in% as.Date(c("2025-05-20", "2025-05-21", "2025-05-22")))
temperature_hourly <- weather %>%
  mutate(hour = format(floor_date(datetime, "hour"), "%H:00")) %>%
  group_by(hour) %>%
  summarise(n = n(), mean_c = mean(temp_c), min_c = min(temp_c),
            max_c = max(temp_c), .groups = "drop") %>%
  mutate(clock = clock(hour)) %>% arrange(clock)
save_table(temperature_hourly, "temperature_hourly_c.csv")

# The supplied pressure_tidy.csv has signed bar readings but no individual
# tree/date identifiers. We cannot re-check the three-tree repeated design.

pressure <- read_csv("data/pressure_tidy.csv", show_col_types = FALSE) %>%
  transmute(time = substr(as.character(time), 1, 5),
            pressure_bar = as.numeric(pressure)) %>%
  filter(!is.na(time), !is.na(pressure_bar))
stopifnot(nrow(pressure) > 0, all(pressure$pressure_bar <= 0))
pressure_hourly <- pressure %>% group_by(time) %>%
  summarise(n = n(), mean_mpa = mean(pressure_bar) * 0.1,
            min_mpa = min(pressure_bar) * 0.1,
            max_mpa = max(pressure_bar) * 0.1, .groups = "drop") %>%
  mutate(clock = clock(time)) %>% arrange(clock)
save_table(pressure_hourly, "xylem_pressure_hourly_mpa.csv")
pressure_windows <- pressure_hourly %>%
  mutate(hour = as.integer(substr(time, 1, 2)),
         time_window = factor(case_when(
           hour >= 7 & hour < 10 ~ "Morning",
           hour >= 10 & hour < 17 ~ "Afternoon",
           hour >= 17 & hour < 20 ~ "Evening",
           TRUE ~ "Night"),
           levels = c("Morning", "Afternoon", "Evening", "Night"))) %>%
  group_by(time_window) %>%
  summarise(n_clock_times = n(), mean_hourly_mpa = mean(mean_mpa), .groups = "drop")
save_table(pressure_windows, "Figure1_pressure_by_window.csv")

paired <- palette.colors(12, palette = "Paired")
window_fill <- c(Morning = paired[5], Afternoon = paired[9],
                 Evening = paired[3], Night = paired[1])
windows <- tibble(
  group = c(rep("Dec/Dem", 5), rep("Cas", 3)),
  period = c("Night", "Morning", "Afternoon", "Evening", "Night",
             "Morning", "Afternoon", "Evening"),
  start = c("00:00", "07:00", "10:00", "17:00", "20:00",
            "08:00", "12:00", "18:00"),
  end = c("07:00", "10:00", "17:00", "20:00", "24:00",
          "11:00", "17:00", "20:00"),
  ymin = c(rep(-2.04, 5), rep(-1.82, 3)),
  ymax = c(rep(-1.86, 5), rep(-1.68, 3))
) %>% mutate(xmin = clock(start),
             xmax = if_else(end == "24:00", clock("00:00", "2000-01-02"), clock(end)),
             period = factor(period, levels = names(window_fill)))
axis_range <- c(clock("00:00"), clock("00:00", "2000-01-02"))
axis_clock <- scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M",
                               limits = axis_range, expand = c(0, 0), timezone = "UTC")
panel_temp <- ggplot(temperature_hourly, aes(clock, mean_c)) +
  geom_line() + geom_point(size = 2) +
  geom_errorbar(aes(ymin = min_c, ymax = max_c), width = 1200) +
  axis_clock + labs(x = NULL, y = "Air temperature (°C)") + theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
panel_pressure <- ggplot() +
  geom_rect(data = windows, aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                ymax = ymax, fill = period), alpha = .55,
            color = "black", linewidth = .3) +
  geom_text(data = windows,
            aes(x = xmin + (xmax - xmin) / 2, y = (ymin + ymax) / 2,
                label = ifelse(group == "Cas" & period == "Evening", "Eve", as.character(period))),
            size = 2.6) +
  geom_line(data = pressure_hourly, aes(clock, mean_mpa)) +
  geom_point(data = pressure_hourly, aes(clock, mean_mpa), size = 2) +
  geom_errorbar(data = pressure_hourly,
                aes(x = clock, ymin = min_mpa, ymax = max_mpa), width = 1200) +
  axis_clock + scale_y_reverse(breaks = seq(0, -2, -.2)) +
  coord_cartesian(ylim = c(0, -2.1)) +
  scale_fill_manual(values = window_fill) +
  labs(x = "Local time", y = "Xylem pressure (MPa)") + theme_classic() +
  theme(legend.position = "none")
fig1 <- panel_temp / panel_pressure + plot_annotation(tag_levels = "A", tag_suffix = ")")
save_plot(fig1, "Figure1_temperature_xylem_AB.png", 9, 8)

#----------------------------
#-##### Chapter 5: Figure 2, excretion proportion ####

# The exclusions follow the Dec/Dem analysis 
# (death in the field or lost pipette tip). 
# Input.csv also omits three unfinished specimens
# (#96, #139, #141); use its 58 IDs for the Figure 2 denominator. 
# Excretion is 1 when measured excreta mass is > 0; zero/absent values remain. 
# The source's 'noon' category denotes the Afternoon experimental window, 10:00–17:00.

decdem_exclude <- c(24, 25, 30, 31, 39:44, 47, 48, 57:61, 63, 68,
                 84:86, 112, 117:120, 122, 137, 138, 145,
                 148, 149, 151, 161, 162)
time_levels <- c("Morning", "Afternoon", "Evening", "Night")
window_name <- function(x) factor(case_when(
  str_to_lower(x) == "morning" ~ "Morning",
  str_to_lower(x) %in% c("noon", "afternoon") ~ "Afternoon",
  str_to_lower(x) == "evening" ~ "Evening",
  str_to_lower(x) == "night" ~ "Night"), levels = time_levels)
corrected_input_raw <- read_source("data/Input.csv")
rate_ids <- parse_number(corrected_input_raw$number)
excretion <- dec_dem_raw %>%
  filter(!number %in% decdem_exclude, number %in% rate_ids) %>%
  mutate(time_window = window_name(time_slot_of_third_action),
         excreta_g = coalesce(if_else(str_to_lower(action_3) == "pee" & !is.na(weight_2),
                                      weight_2, 0), 0) +
           coalesce(if_else(str_to_lower(action_4) == "pee" & !is.na(weight_3),
                            weight_3, 0), 0),
         excreting = as.integer(excreta_g > 0)) %>%
  filter(!is.na(time_window)) %>%
  select(number, species, time_window, excreta_g, excreting)
stopifnot(!anyDuplicated(excretion$number), nrow(excretion) == 58)
stopifnot(!anyNA(excretion$excreting))
save_table(excretion, "Figure2_excretion_individuals.csv")
excretion_summary <- excretion %>% group_by(time_window) %>%
  summarise(n = n(), excreting = sum(excreting),
            probability_pct = 100 * excreting / n, .groups = "drop")
print(excretion_summary)
stopifnot(nrow(excretion_summary) == 4,
          all(is.finite(excretion_summary$probability_pct)),
          all(excretion_summary$probability_pct >= 0 &
                excretion_summary$probability_pct <= 100))
save_table(excretion_summary, "Figure2_excretion_summary.csv")
fig2 <- ggplot(excretion_summary, aes(time_window, probability_pct, fill = time_window)) +
  geom_col(width = .7, color = "black") +
  geom_text(aes(label = paste0(excreting, "/", n)), vjust = -0.5) +
  scale_fill_manual(values = window_fill) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  coord_cartesian(ylim = c(0, 110)) +
  labs(x = "Time window", y = "Excretion proportion (%)") +
  theme_classic() + theme(legend.position = "none")
save_plot(fig2, "Figure2_excretion_proportion.png", 7, 5)



# Compare excretion (yes/no) across the four time windows using an overall 4 × 2 Fisher's exact test.
# All six afternoon cicadas excreted, so the observed afternoon proportion is 6/6 (100%).
# Follow the overall test with six pairwise 2 × 2 Fisher's exact tests,
# adjusting their p-values using Holm's method.

pee_table <- table(excretion$time_window, excretion$excreting)
print(pee_table)
print(fisher.test(pee_table))

pairs <- combn(rownames(pee_table), 2, simplify = FALSE)
pairwise_fisher <- data.frame(
  comparison = vapply(pairs, paste, collapse = " vs ", FUN.VALUE = ""),
  p_raw = vapply(
    pairs,
    function(x) fisher.test(pee_table[x, , drop = FALSE])$p.value,
    numeric(1)
  )
)
pairwise_fisher$p_holm <- p.adjust(pairwise_fisher$p_raw, method = "holm")
print(pairwise_fisher)
save_table(pairwise_fisher, "Figure2_pairwise_fisher.csv")

#----------------------------
#-###### Chapter 6: Figure 3, positive input rates ####

# Input.csv is the corrected rate calculation: 
# duration starts at field placement (action 2), not the initial lab weighing. 
# The rate is rounded in the source. 
# Keep all corrected rates for auditing -> select positive values for the manuscript plots/tests.
# Usage of Equation 1 in methods 

input_all <- corrected_input_raw %>%
  transmute(number = parse_number(number), purpose, species, sex,
            time_window = window_name(time_slot_of_third_action),
            lab_mass_g = to_decimal(weight_1), duration_h = to_decimal(duration),
            input_g = to_decimal(input), input_g_hr = to_decimal(input_hr)) %>%
  filter(purpose == "Excretion", species %in% c("Dec", "Dem"),
         !number %in% decdem_exclude, !is.na(time_window), !is.na(input_g_hr)) %>%
  mutate(species = factor(species, levels = c("Dec", "Dem")))
stopifnot(!anyDuplicated(input_all$number), nrow(input_all) == 58)
rate_check <- input_all %>%
  filter(is.na(duration_h) | duration_h <= 0 | is.na(input_g) |
           abs(input_g_hr - input_g / duration_h) > .0001)
if (nrow(rate_check)) {
  save_table(rate_check, "input_rate_check.csv")
  stop("Corrected Input.csv rates failed the duration check; inspect output/input_rate_check.csv")
}
save_table(input_all, "DecDem_all_rates.csv")
positive <- input_all %>% filter(input_g_hr > 0)
save_table(positive, "DecDem_positive_rates.csv")
stopifnot(nrow(positive) == 51)

# Each panel displays raw specimens (Dec circles; Dem triangles) with black group mean ± SE.  
# SE is the precision of the mean, not individual spread.
# all y- axis have the same lable 

plot_input <- function(x, label, fill, point_color) {
  ggplot(x, aes(species, input_g_hr)) +
    geom_violin(fill = fill, alpha = .45, trim = FALSE, color = NA) +
    geom_jitter(aes(shape = species), color = point_color,
                width = .08, size = 2.4) +
    stat_summary(fun = mean, geom = "point", color = "black", size = 3.2) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .12) +
    scale_shape_manual(values = c(Dec = 16, Dem = 17)) +
    scale_y_continuous(
      breaks = seq(0, 0.25, by = 0.05),
      labels = function(y) sprintf("%.2f", y)
    ) +
    coord_cartesian(ylim = c(0, 0.26)) +
    labs(title = label, x = "Species", y = "Input rate (g h⁻¹)") +
    theme_classic() + theme(legend.position = "none")
}
fig3 <- (
  plot_input(filter(positive, time_window == "Morning"),
             "A) Morning", paired[5], paired[6]) |
    plot_input(filter(positive, time_window == "Afternoon"),
               "B) Afternoon", paired[9], paired[10])
) / (
  plot_input(filter(positive, time_window == "Evening"),
             "C) Evening", paired[3], paired[4]) |
    plot_input(filter(positive, time_window == "Night"),
               "D) Night", paired[1], paired[2])
)
save_plot(fig3, "Figure3_positive_input_four_panels.png", 8, 7)

rate_summary <- positive %>% group_by(species, time_window) %>%
  summarise(n = n(), mean_g_hr = mean(input_g_hr),
            sd_g_hr = sd(input_g_hr), median_g_hr = median(input_g_hr),
            se_g_hr = sd_g_hr / sqrt(n), .groups = "drop")
save_table(rate_summary, "Figure3_input_rate_summary.csv")
print(rate_summary)
species_summary <- positive %>%
  group_by(species) %>%
  summarise(
    n = n(), mean_g_hr = mean(input_g_hr),
    sd_g_hr = sd(input_g_hr),
    .groups = "drop"
  )

print(species_summary)
save_table(species_summary, "Figure3_species_summary.csv")
print(wilcox.test(input_g_hr ~ species, data = positive))
print(kruskal.test(input_g_hr ~ time_window, data = positive))
pair_rate <- pairwise_wilcox_test(
  positive, input_g_hr ~ time_window,
  p.adjust.method = "holm"
)
save_table(pair_rate, "Figure3_pairwise_Holm.csv")
print(pair_rate)

# Table gives guidance of mean per species and in each time window  
# Mean is of all time windows combined 
# Wilcoxon (2 species) -> not significant
# Kruskal (4 time windows) -> significant
# Wilcoxon to compare time windows, holm adjustment

# Important (!!!):
# Positive estimated input can result from excreta, body-mass gain, or both.
# Individuals with no excreta but positive body-mass change remain included.
positive <- input_all %>% filter(input_g_hr > 0)

#----------------------------
#-####### Chapter 7: Figure 4, rate per body mass ####

# Uses the same positive-input as Figure 3. 
# Dividing g/h by starting body mass in g gives a mass-specific rate in h^-1.

mass_rate <- positive %>%
  filter(!is.na(lab_mass_g), lab_mass_g > 0) %>%
  mutate(rate_per_mass_h = input_g_hr / lab_mass_g)

# Each cicada appears once - contributes to one observation in a time window
# Field day is the day it was placed outside.

day_lookup <- corrected_input_raw %>%
  transmute(number = parse_number(number),
            field_day = dmy(date_of_second_action)) %>%
  filter(!is.na(number))
stopifnot(!anyDuplicated(day_lookup$number))

mass_rate_day <- mass_rate %>% left_join(day_lookup, by = "number")
stopifnot(nrow(mass_rate_day) == 51,
          n_distinct(mass_rate_day$number) == 51,
          !anyNA(mass_rate_day$field_day),
          is.factor(mass_rate_day$time_window),
          !is.ordered(mass_rate_day$time_window))
print(table(mass_rate_day$field_day, mass_rate_day$time_window))
save_table(mass_rate_day, "Figure4_mass_specific_individuals.csv")

# Descriptive arithmetic means and SD on the original h^-1 scale.

mass_summary <- mass_rate_day %>% group_by(species, time_window) %>%
  summarise(n = n(), mean_per_h = mean(rate_per_mass_h),
            sd_per_h = sd(rate_per_mass_h), .groups = "drop")
mass_species_summary <- mass_rate_day %>% group_by(species) %>%
  summarise(n = n(), mean_per_h = mean(rate_per_mass_h),
            sd_per_h = sd(rate_per_mass_h), .groups = "drop")
print(mass_summary)
print(mass_species_summary)
save_table(mass_summary, "Figure4_by_species_and_window.csv")
save_table(mass_species_summary, "Figure4_by_species.csv")

# Raw-scale residuals were strongly skewed; the log model fit better.
# Time is a categorical factor, not a numeric or linear time trend.
# With only three field days, include day as a fixed effect.

log_full <- lm(log(rate_per_mass_h) ~ species + factor(field_day) +
                 time_window, data = mass_rate_day)
log_null <- update(log_full, . ~ . - time_window)
log_no_species <- update(log_full, . ~ . - species)
log_interaction <- update(log_full, . ~ . + species:time_window)

print(anova(log_null, log_full))           # Overall time: F(3,44)=10.18, p=3.269e-05
print(anova(log_no_species, log_full))     # Species: F(1,44)=0.51, p=0.481
print(anova(log_full, log_interaction))    # Interaction: F(3,41)=0.38, p=0.768

pdf("output/Figure4_log_model_diagnostics.pdf", width = 7, height = 7)
par(mfrow = c(2, 2)); plot(log_full); dev.off()
pdf("output/Figure4_interaction_diagnostics.pdf", width = 7, height = 7)
par(mfrow = c(2, 2)); plot(log_interaction); dev.off()

# May 23 has Morning observations only.
# Repeating the analysis using May 21–22, when all four windows were sampled.

mass_21_22 <- mass_rate_day %>%
  filter(field_day %in% as.Date(c("2025-05-21", "2025-05-22")))
stopifnot(nrow(mass_21_22) == 40)
log_sensitivity <- lm(log(rate_per_mass_h) ~ species + factor(field_day) +
                        time_window, data = mass_21_22)
print(anova(update(log_sensitivity, . ~ . - time_window),
            log_sensitivity))             # F(3,34)=8.31, p=0.000278

# Primary comparisons: day-adjusted time contrasts, pooled over species.
# Holm adjusts the six p-values; estimates are log differences.

time_means <- emmeans(log_full, ~ time_window)
time_pairs <- as.data.frame(pairs(time_means, adjust = "holm")) %>%
  mutate(rate_ratio = exp(estimate))
print(time_pairs)
save_table(time_pairs, "Figure4_pooled_model_Holm.csv")

# Repeat all six contrasts on May 21–22 to check the Morning-only third day.

sensitivity_pairs <- as.data.frame(
  pairs(emmeans(log_sensitivity, ~ time_window), adjust = "holm"))
print(sensitivity_pairs)
save_table(sensitivity_pairs, "Figure4_two_day_sensitivity_Holm.csv")

# Secondary exploratory comparisons within each species. 
# Require the interaction model; Holm is applied to six tests within each species.

species_time_means <- emmeans(log_interaction, ~ time_window | species)
species_time_pairs <- as.data.frame(
  pairs(species_time_means, adjust = "holm")) %>%
  mutate(rate_ratio = exp(estimate))
print(species_time_pairs)
save_table(species_time_pairs, "Figure4_within_species_model_Holm.csv")

# Build stars from model-adjusted p-values

make_brackets <- function(x) x %>%
  filter(p.value < .05) %>%
  separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
  mutate(p.adj = p.value,
         p.adj.signif = case_when(
           p.adj < .001 ~ "***", p.adj < .01 ~ "**",
           p.adj < .05 ~ "*", TRUE ~ "ns"),
         y.position = .415 + .03 * (row_number() - 1))

pooled_brackets <- make_brackets(time_pairs)
dec_brackets <- species_time_pairs %>%
  filter(species == "Dec") %>% make_brackets()

window_point <- c(Morning = paired[6], Afternoon = paired[10],
                  Evening = paired[4], Night = paired[2])

plot_mass <- function(x, label, brackets = NULL, show_y = FALSE) {
  panel <- ggplot(x, aes(time_window, rate_per_mass_h)) +
    geom_violin(aes(fill = time_window), alpha = .45, color = NA, trim = FALSE) +
    geom_jitter(aes(color = time_window, shape = species),
                width = .1, height = 0, size = 2.5) +
    stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 color = "black", width = .12) +
    scale_fill_manual(values = window_fill) +
    scale_color_manual(values = window_point) +
    scale_shape_manual(values = c(Dec = 16, Dem = 17)) +
    scale_y_continuous(breaks = seq(0, .5, .1),
                       labels = function(y) sprintf("%.1f", y)) +
    coord_cartesian(ylim = c(0, .54), clip = "off") +
    labs(title = label, x = "Time window",
         y = if (show_y) "Mass-specific total input rate ((g h⁻¹g⁻¹)" else NULL) +
    theme_classic() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 40, hjust = 1),
          plot.title = element_text(face = "bold"))
  if (!show_y) panel <- panel +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  if (!is.null(brackets) && nrow(brackets)) panel <- panel +
      stat_pvalue_manual(brackets, label = "p.adj.signif",
                         xmin = "group1", xmax = "group2",
                         y.position = "y.position", tip.length = .01,
                         inherit.aes = FALSE)
  panel
}

# A: primary additive-model contrasts. 
# B: exploratory interaction-model contrasts within Dec. 
# C: Dem has no significant adjusted contrasts.

fig4 <- plot_mass(mass_rate_day, "A)", pooled_brackets, show_y = TRUE) |
  plot_mass(filter(mass_rate_day, species == "Dec"), "B)", dec_brackets) |
  plot_mass(filter(mass_rate_day, species == "Dem"), "C)")
save_plot(fig4, "Figure4_mass_specific_ABC.png", 9.5, 8.5)

#----------------------------
#-######## Chapter 8: Figure 5, Cas masses and control ####

# Four masses per cicada. 
# The first mass was measured before field placement;
# panel A displays it at 08:00 to mark the start of the field exposure.
# All weight per hour calculations, are using the time they have been in the filed on the trees only.

study_mass <- cas_tidy %>%
  filter(!is.na(weight_g), !is.na(time), !is.na(date)) %>%
  mutate(measurement = factor(event, levels = 1:4,
                              labels = c("08:00", "11:00", "17:30", "20:30"))) %>%
  arrange(number, event)
stopifnot(n_distinct(study_mass$number) == 18, nrow(study_mass) == 72)
save_table(study_mass, "Figure5_cas_masses.csv")

mass_summary <- study_mass %>% group_by(measurement) %>%
  summarise(n = n(), mean_g = mean(weight_g),
            sd_g = sd(weight_g), se_g = sd_g / sqrt(n),
            .groups = "drop")
print(mass_summary)
save_table(mass_summary, "Figure5A_body_mass_summary.csv")

# Random intercept accounts for four measurements from each cicada.
# The interaction tests whether the time profile differs between sexes.
# -> does the pattern of body-mass change over time differs between females and males. 

model_time <- lmer(weight_g ~ measurement + (1 | number), data = study_mass)
model_sex <- lmer(weight_g ~ sex + measurement + (1 | number), data = study_mass)
model_sex_time <- lmer(weight_g ~ sex * measurement + (1 | number),
                       data = study_mass)
print(anova(model_time))
print(anova(model_sex))
print(anova(model_sex, model_sex_time))

# Change between consecutive masses, divided by hours of field exposure (!!!).
# These are the exposure duration specified in Methods:
# 3h (Morning), 5h (Afternoon) and 2h (Evening).
# -> It excludes the time in the lab between measurements (!!!)

field_hours <- c(Morning = 3, Afternoon = 5, Evening = 2)
step_change <- study_mass %>%
  group_by(number) %>% arrange(event, .by_group = TRUE) %>%
  mutate(previous_mass = lag(weight_g),
         change_pct = 100 * (weight_g - previous_mass) / previous_mass,
         period = factor(c("baseline", "Morning", "Afternoon", "Evening")[event],
                         levels = names(field_hours))) %>%
  filter(!is.na(period)) %>% ungroup() %>%
  mutate(field_h = unname(field_hours[as.character(period)]),
         change_pct_field_h = change_pct / field_h)
stopifnot(nrow(step_change) == 54, all(step_change$field_h > 0))
save_table(step_change, "Figure5_cas_step_changes.csv")

step_summary <- step_change %>% group_by(period) %>%
  summarise(n = n(), mean_total_pct = mean(change_pct),
            mean_pct_h = mean(change_pct_field_h),
            se_pct_h = sd(change_pct_field_h) / sqrt(n), .groups = "drop")
print(step_summary)
save_table(step_summary, "Figure5_step_change_summary.csv")

# Each cicada contributes one rate in each time window. 
# Friedman tests the overall window effect while keeping the three observations paired.

stopifnot(all(table(step_change$number, step_change$period) == 1))
print(friedman.test(change_pct_field_h ~ period | number,
                    data = step_change))

# Follow-up comparisons are paired by specimen ID. Holm adjusts three tests.
# -> because one individual gives us 4 measurements (!!!)

rate_wide <- step_change %>%
  select(number, period, change_pct_field_h) %>%
  pivot_wider(names_from = period, values_from = change_pct_field_h)
stopifnot(nrow(rate_wide) == 18,
          !anyNA(rate_wide[c("Morning", "Afternoon", "Evening")]))
window_pairs <- list(c("Morning", "Afternoon"),
                     c("Morning", "Evening"),
                     c("Afternoon", "Evening"))
paired_results <- tibble(
  comparison = vapply(window_pairs, paste, collapse = " vs ", FUN.VALUE = ""),
  p_raw = vapply(window_pairs, function(x)
    wilcox.test(rate_wide[[x[1]]], rate_wide[[x[2]]],
                paired = TRUE, exact = FALSE)$p.value, numeric(1))
) %>% mutate(p_holm = p.adjust(p_raw, method = "holm"))
print(paired_results)
save_table(paired_results, "Figure5_paired_window_Holm.csv")

# Test change against zero in each window; 
# -> it is against zero to have a natrual look at it and not influenced by the previous time window (this can be discussed)
# adjust the three p values.

zero_tests <- step_change %>% group_by(period) %>%
  summarise(n = n(),
            V = unname(wilcox.test(change_pct_field_h, mu = 0)$statistic),
            p_raw = wilcox.test(change_pct_field_h, mu = 0)$p.value,
            .groups = "drop") %>%
  mutate(p_holm = p.adjust(p_raw, method = "holm"))
print(zero_tests)
save_table(zero_tests, "Figure5_wilcoxon_vs_zero.csv")

# Six female controls; each spent 3 h in the field.

control_mass <- control_tidy %>% filter(!is.na(weight_g)) %>%
  arrange(number, event) %>% group_by(number) %>%
  mutate(previous_mass = lag(weight_g),
         change_pct = 100 * (weight_g - previous_mass) / previous_mass) %>%
  filter(!is.na(change_pct)) %>% ungroup() %>%
  mutate(field_h = 3, change_pct_field_h = change_pct / field_h,
         period = factor("Morning", levels = names(field_hours)))
stopifnot(nrow(control_mass) == 6)
save_table(control_mass, "Figure5_control_changes.csv")

control_summary <- control_mass %>%
  summarise(n = n(), mean_pct_h = mean(change_pct_field_h),
            se_pct_h = sd(change_pct_field_h) / sqrt(n),
            period = factor("Morning", levels = names(field_hours)))
print(control_summary)

# Compare Afternoon study cicadas with controls on the same % per field h scale.
# -> Afternoon because it has the negative body mass change = did they not feed? 
# The female-only test matches the sex of the controls; 
# the all-sex test checks whether restricting the study group changes the conclusion.

afternoon <- filter(step_change, period == "Afternoon")
print(t.test(filter(afternoon, sex == "female")$change_pct_field_h,
             control_mass$change_pct_field_h))
print(t.test(afternoon$change_pct_field_h,
             control_mass$change_pct_field_h))

# Figure 5A: individual trajectories; female = filled, male = open square.

cas_cols <- c(Morning = paired[5], Afternoon = paired[9], Evening = paired[3])
blocks <- tibble(period = factor(names(cas_cols), levels = names(cas_cols)),
                 xmin = c(8, 12, 18), xmax = c(11, 17, 20))
mass_plot <- study_mass %>%
  mutate(time_h = as.numeric(substr(time, 1, 2)) +
           as.numeric(substr(time, 4, 5)) / 60)

panel_5a <- ggplot() +
  geom_rect(data = blocks, aes(xmin = xmin, xmax = xmax, ymin = -Inf,
                               ymax = Inf, fill = period),
            alpha = .22, show.legend = FALSE) +
  geom_line(data = mass_plot, aes(time_h, weight_g, group = number),
            color = "grey35", linewidth = .5, alpha = .7) +
  geom_point(data = mass_plot, aes(time_h, weight_g, shape = sex),
             color = "black", size = 2.8) +
  scale_fill_manual(values = cas_cols) +
  scale_shape_manual(values = c(female = 15, male = 0)) +
  scale_x_continuous(breaks = seq(8, 20, 2),
                     labels = function(x) sprintf("%02d:00", x)) +
  labs(title = "A)", x = "Time of day", y = "Body mass (g)", shape = "Sex") +
  theme_classic() +
  theme(legend.position = "top", legend.justification = "left",
        plot.title = element_text(face = "bold"))

# Figure 5B: one bar per cicada; black point and error bar = mean ± SE.

panel_5b <- ggplot(step_change,
                   aes(period, change_pct_field_h, fill = period)) +
  geom_col(aes(group = number),
           position = position_dodge2(width = .8, preserve = "single",
                                      padding = .08),
           width = .75, alpha = .75, color = "black", linewidth = .2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(data = step_summary,
                aes(period, ymin = mean_pct_h - se_pct_h,
                    ymax = mean_pct_h + se_pct_h),
                inherit.aes = FALSE, width = .1) +
  geom_point(data = step_summary, aes(period, mean_pct_h),
             inherit.aes = FALSE, color = "black", size = 2) +
  scale_fill_manual(values = cas_cols, guide = "none") +
  scale_x_discrete(labels = c(Morning = "08:00–11:00",
                              Afternoon = "12:00–17:00",
                              Evening = "18:00–20:00")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f%%", x)) +
  labs(title = "B)", x = "Field time window",
       y = "Body mass change per field hour (% h⁻¹)") +
  theme_classic() + theme(plot.title = element_text(face = "bold"))

# Figure 5C: controls on the same per-field-hour scale.

panel_5c <- ggplot(control_mass, aes(period, change_pct_field_h)) +
  geom_col(aes(group = number),
           position = position_dodge2(width = .8, preserve = "single",
                                      padding = .08),
           width = .75, fill = cas_cols["Morning"], alpha = .75,
           color = "black", linewidth = .2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(data = control_summary,
                aes(period, ymin = mean_pct_h - se_pct_h,
                    ymax = mean_pct_h + se_pct_h),
                inherit.aes = FALSE, width = .1) +
  geom_point(data = control_summary, aes(period, mean_pct_h),
             inherit.aes = FALSE, color = "black", size = 2) +
  scale_x_discrete(limits = names(cas_cols), drop = FALSE,
                   labels = c(Morning = "09:00–12:00",
                              Afternoon = "12:00–17:00",
                              Evening = "18:00–20:00")) +
  scale_y_continuous(labels = function(x) sprintf("%.1f%%", x)) +
  labs(title = "C)", x = "Field time window (Control)",
       y = "Body mass change per field hour (% h⁻¹)") +
  theme_classic() + theme(plot.title = element_text(face = "bold"))

fig5 <- panel_5a / panel_5b / panel_5c + plot_layout(heights = c(1.1, 1, 1))
save_plot(fig5, "Figure5_cas_body_mass_ABC.png", 10, 10)


 #----------------------------
#-######### Chapter 9: Figure S1, initial body mass across species ####

# Use all 179 initial masses (first body mass measurements), 
# including specimens excluded from later experiments.
# These are starting masses, not repeated weights.

body_s1 <- raw %>%
  filter(!is.na(weight_1), sex %in% c("female", "male")) %>%
  transmute(number, species = factor(species, levels = c("Cas", "Dec", "Dem")),
            sex = factor(sex, levels = c("female", "male")),
            body_mass_g = weight_1)
stopifnot(nrow(body_s1) == 179, !anyDuplicated(body_s1$number))

# Black symbols mark the median for each species and sex.

s1_medians <- body_s1 %>% group_by(species, sex) %>%
  summarise(median_g = median(body_mass_g), .groups = "drop")
s1_cols <- c(Cas = paired[12], Dec = paired[7], Dem = paired[8])

fig_s1 <- ggplot(body_s1, aes(species, body_mass_g, color = species, shape = sex)) +
  geom_point(position = position_jitter(width = .15, height = 0, seed = 1),
             size = 3, alpha = .9) +
  geom_point(data = s1_medians, aes(species, median_g, shape = sex),
             inherit.aes = FALSE, color = "black", size = 3.3) +
  scale_color_manual(values = s1_cols, guide = "none") +
  scale_shape_manual(values = c(female = 15, male = 0)) +
  labs(x = "Species", y = "Body mass (g)", shape = "Sex") +
  theme_classic() +
  theme(legend.position = c(.05, .95),
        legend.justification = c(0, 1))
save_plot(fig_s1, "FigureS1_initial_body_mass.png", 9, 6.5)
save_table(s1_medians, "FigureS1_species_sex_medians.csv")

# Independent female–male comparisons within each species; adjust three tests.

s1_sex_tests <- body_s1 %>% group_by(species) %>%
  summarise(n_female = sum(sex == "female"), n_male = sum(sex == "male"),
            p_raw = wilcox.test(body_mass_g[sex == "female"],
                                body_mass_g[sex == "male"],
                                exact = FALSE)$p.value, .groups = "drop") %>%
  mutate(p_holm = p.adjust(p_raw, method = "holm"))
print(s1_sex_tests)
save_table(s1_sex_tests, "FigureS1_sex_tests_Holm.csv")

# Species means ± SD and the overall species tests are already in Chapter 3.

