# Dec/Dem feeding analysis for: "Sucking to Survive? Adult 17-year periodical cicadas (Magicicada spp.) exhibit low rates of xylem ingestion"
# Year: 2026
# Script author: Theresa Weinspach
# Manuscript authors: Theresa Weinspach, Leigh Boardman, and Phillip G. D. Matthews
# Purpose: Figure 3 (Rate of xylem ingestion (g h^-1) by M. septendecula (Dec) and M. septendecim (Dem) accross different times of day) 
#       and Figure 4 (Excretion rate normalized to bodyweight in M. septendecula and M. septendecim)
#       additional statistics and their interpretation/ explanations
# Source: data/Input.csv (semicolon separator, decimal commas).
# 
# This is only part of the code used to understand the results of our Research. 
#   This code only includes the figures needed and published in the paper.
#   For further information please contact authors
#
# Input.csv contains input mass and input rate calculated for the time in the field. 
# The relevant duration runs from action 2 (field placement) to the final measurement, 
# rather than from action 1 (lab weighing). 
# This script uses the source sheet's input_hr and checks it against input / duration.
# Interpret the rate as estimated input over that field interval.

#-# Chapter 1: Packages and reproducibility ####
# Package versions are recorded at run time in output/package_versions.txt.
# There are no specimen loops; the data are processed as whole columns.

#R == 4.3.1

library(readr)    #2.1.4
library(dplyr)    #1.1.4
library(stringr)  #1.5.0
library(janitor)  #2.2.1
library(ggplot2)  #3.5.2
library(ggpubr)   #0.6.1
library(patchwork)#1.1.0
library(rstatix)  #0.7.2
library(tibble)   #3.2.1

dir.create("output", showWarnings = FALSE, recursive = TRUE)

packages_used <- c(
  "readr", "dplyr", "stringr", "janitor", "ggplot2",
  "ggpubr", "patchwork", "rstatix", "tibble"
)

package_versions <- paste(
  packages_used,
  vapply(packages_used, function(pkg) as.character(packageVersion(pkg)), ""),
  sep = " == "
)

writeLines(
  c(paste("R ==", getRversion()), package_versions),
  "paper_analysis/output/package_versions.txt"
)

#----------------------------
#-## Chapter 2: Import and analysis decisions ####

# Read every field as text. Otherwise readr can interpret a decimal comma
# such as 3,50 as a thousands separator and turn it into 350.
input_raw <- read_delim(
  "data/Input.csv",
  delim = ";",
  col_types = cols(.default = col_character()),
  trim_ws = TRUE,
  show_col_types = FALSE
)

to_decimal <- function(x) as.numeric(str_replace(x, ",", "."))

remove_ids <- c(
  24, 25, 30, 31, 39:44, 47, 48, 57:61, 63, 68,
  84:86, 112, 117:120, 122, 137, 138, 145,
  148, 149, 151, 161, 162
) #These iD's were removed from the raw data to generate input.csv

# The listed individuals died in the field or lost their pipette tip.
# A negative input estimate remains in the source-based audit data set, but
# is excluded from the positive-input figures and tests.
# This is a choice about the biological question, not a correction of the source values.
# Negative values mean the individual did not feed and lost weight 
# Therefore, comparison/ statistic is between feeding individuals 

input_all <- input_raw %>%
  clean_names() %>%
  mutate(across(where(is.character), ~ na_if(str_squish(.x), ""))) %>%
  transmute(
    number = parse_number(number),
    purpose,
    species,
    sex,
    time_slot_of_third_action,
    lab_weight_g = to_decimal(weight_1),
    duration_h = to_decimal(duration),
    input_g = to_decimal(input),
    input_g_hr = to_decimal(input_hr),
    time_window = case_when(
      str_to_lower(time_slot_of_third_action) == "morning" ~ "Morning",
      str_to_lower(time_slot_of_third_action) %in%
        c("noon", "afternoon") ~ "Afternoon",
      str_to_lower(time_slot_of_third_action) == "evening" ~ "Evening",
      str_to_lower(time_slot_of_third_action) == "night" ~ "Night", #different names, one day I will remember to do my notes better 
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    purpose == "Excretion",
    species %in% c("Dec", "Dem"),
    !number %in% remove_ids,
    !is.na(input_g_hr),
    !is.na(time_window)
  ) %>%
  mutate(
    species = factor(species, levels = c("Dec", "Dem")),
    time_window = factor(
      time_window,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    )
  )

# The input-rate column is rounded in the source. A 0.0001 g/hr tolerance
# covers that rounding but still flags a substantial inconsistency.

rate_check <- input_all %>%
  mutate(
    recalculated_g_hr = input_g / duration_h,
    difference_g_hr = input_g_hr - recalculated_g_hr
  ) %>%
  filter(
    is.na(duration_h) | duration_h <= 0 | is.na(input_g) |
      is.na(difference_g_hr) | abs(difference_g_hr) > 0.0001
  )

if (nrow(rate_check) > 0) {
  write_csv(rate_check, "paper_analysis/output/input_rate_check.csv")
  stop("Check output/input_rate_check.csv before interpreting the figures.")
}

if (anyDuplicated(input_all$number)) {
  stop("Duplicate specimen IDs in the included Dec/Dem data.")
}

# Both manuscript figures concern positive estimated intake. 
# A zero estimate would mean no measurable gain, so it is also left out here. (-> this can be discussed...) 
# Reconsider this exclusion for any analysis intended to describe ALL sampled cicadas.(-> Script "DecDem" has statistic analysis on both)

positive <- input_all %>%
  filter(input_g_hr > 0) %>%
  mutate(
    x_cat = as.numeric(time_window),
    x_species = x_cat + if_else(species == "Dec", -0.15, 0.15)
  )

cat("Included observations, all rates:", nrow(input_all), "\n")
cat("Included observations, positive rates:", nrow(positive), "\n")
print(count(input_all, species, time_window))
print(count(positive, species, time_window))

write_csv(input_all, "paper_analysis/output/DecDem_all_rates.csv")
write_csv(positive, "paper_analysis/output/DecDem_positive_rates.csv")

#----------------------------
###-### Chapter 3: Figure 3, positive input rate ####

# Raw points show individuals. 
# The black points and error bars summarize each species within each window as mean +/- standard error (SE).
# SE describes uncertainty in a group mean; 
# it does not show the spread of individual cicadas. 
# Dec and Dem are offset so their points do not overlap.

###-### Chapter 3: Figure 2, four positive-input panels ###-###

# Raw points show individuals. The black points and error bars summarize
# each species within each time window as mean +/- standard error (SE).
# SE describes uncertainty in a group mean; it does not show the spread of
# individual cicadas. Separate panels show the species comparison for each
# time window. The shared y-axis makes their scales directly comparable.

cols_all <- palette.colors(12, palette = "Paired")
time_point_cols <- c(
  Morning = cols_all[6], Afternoon = cols_all[10],
  Evening = cols_all[4], Night = cols_all[2]
)
time_fill_cols <- c(
  Morning = cols_all[5], Afternoon = cols_all[9],
  Evening = cols_all[3], Night = cols_all[1]
)

range_positive <- range(positive$input_g_hr)
y_limits_positive <- range_positive + c(-0.1, 0.1) * diff(range_positive)

make_positive_panel <- function(data, panel_title, fill_col, point_col) {
  ggplot(data, aes(x = species, y = input_g_hr)) +
    geom_violin(fill = fill_col, alpha = 0.45,
                color = NA, trim = FALSE) +
    geom_jitter(aes(shape = species), width = 0.08,
                size = 2.6, alpha = 0.9, color = point_col) +
    stat_summary(fun = mean, geom = "point", size = 3.5,
                 color = "black") +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 width = 0.12, linewidth = 0.5, color = "black") +
    scale_shape_manual(values = c(Dec = 16, Dem = 17)) +
    coord_cartesian(ylim = y_limits_positive) +
    labs(title = panel_title, x = "Species", y = "Input rate (g/hr)") +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))
}

pApos <- make_positive_panel(
  filter(positive, time_window == "Morning"),
  "A) Morning", cols_all[5], cols_all[6]
)
pBpos <- make_positive_panel(
  filter(positive, time_window == "Afternoon"),
  "B) Afternoon", cols_all[9], cols_all[10]
)
pCpos <- make_positive_panel(
  filter(positive, time_window == "Evening"),
  "C) Evening", cols_all[3], cols_all[4]
)
pDpos <- make_positive_panel(
  filter(positive, time_window == "Night"),
  "D) Night", cols_all[1], cols_all[2]
)

fig_separate_pos <- (pApos | pBpos) / (pCpos | pDpos)
print(fig_separate_pos)
ggsave("paper_analysis/output/Figure2_positive_input_four_panels.png", fig_separate_pos,
       width = 8, height = 7, dpi = 300)

#----------------------------
#-#### Chapter 4: Mass-specific input and the A/B/C figure ####

# Dividing by the initial lab body mass asks how large the estimated input
# rate is relative to animal size. The unit is (g/hr)/g, equivalent to 1/hr.
# This ratio does not itself test whether body mass affects the raw rate.

positive_bw <- positive %>%
  filter(!is.na(lab_weight_g), lab_weight_g > 0) %>%
  mutate(input_per_body_mass_h = input_g_hr / lab_weight_g)

write_csv(positive_bw, "paper_analysis/output/DecDem_positive_mass_specific.csv")

# Each panel has its own family of six pairwise time-window comparisons:
# A pooled, B Dec, and C Dem. BH correction is applied within each family.
# These tests compare distributions. 
# They do not establish a circadian mechanism or a statistically significant difference between species.

get_pairwise <- function(data) {
  data %>%
    pairwise_wilcox_test(
      input_per_body_mass_h ~ time_window,
      p.adjust.method = "BH"
    )
}

pairwise_pooled <- get_pairwise(positive_bw)
pairwise_dec <- get_pairwise(filter(positive_bw, species == "Dec"))
pairwise_dem <- get_pairwise(filter(positive_bw, species == "Dem"))

print(pairwise_pooled)
print(pairwise_dec)
print(pairwise_dem)

write_csv(pairwise_pooled, "paper_analysis/output/pairwise_mass_specific_pooled.csv")
write_csv(pairwise_dec, "paper_analysis/output/pairwise_mass_specific_Dec.csv")
write_csv(pairwise_dem, "paper_analysis/output/pairwise_mass_specific_Dem.csv")

y_max <- max(positive_bw$input_per_body_mass_h)
y_limit <- y_max * 1.40

make_bw_panel <- function(data, tests, label, show_y = FALSE) {
  sig <- tests %>%
    filter(!is.na(p.adj), p.adj < 0.05) %>%
    mutate(y.position = y_max * (1.05 + 0.08 * (row_number() - 1)))

  p <- ggplot(data, aes(x = time_window, y = input_per_body_mass_h)) +
    geom_violin(aes(fill = time_window), alpha = 0.5,
                color = NA, trim = FALSE) +
    geom_jitter(aes(color = time_window, shape = species),
                width = 0.12, size = 2.6, alpha = 0.9) +
    stat_summary(fun = mean, geom = "point", size = 3.2,
                 color = "black") +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 width = 0.08, linewidth = 0.5, color = "black") +
    scale_color_manual(values = time_point_cols, drop = FALSE) +
    scale_fill_manual(values = time_fill_cols, drop = FALSE) +
    scale_shape_manual(values = c(Dec = 16, Dem = 17)) +
    coord_cartesian(ylim = c(0, y_limit)) +
    labs(
      title = label,
      x = "Time window",
      y = if (show_y) "Mass-specific input rate ((g/hr)/g)" else NULL
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      legend.position = "none",
      axis.title.y = if (show_y) element_text() else element_blank(),
      axis.text.y = if (show_y) element_text() else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank(),
      axis.text.x = element_text(angle = 35, hjust = 1)
    )

  if (nrow(sig) > 0) {
    p <- p + stat_pvalue_manual(
      sig, label = "p.adj.signif", xmin = "group1", xmax = "group2",
      y.position = "y.position", tip.length = 0.01
    )
  }

  p
}

panel_a <- make_bw_panel(positive_bw, pairwise_pooled, "A)", show_y = TRUE)
panel_b <- make_bw_panel(
  filter(positive_bw, species == "Dec"), pairwise_dec, "B)"
)
panel_c <- make_bw_panel(
  filter(positive_bw, species == "Dem"), pairwise_dem, "C)"
)

fig_bw_ABC <- panel_a | panel_b | panel_c
print(fig_bw_ABC)
ggsave("paper_analysis//Plots/Figure_mass_specific_ABC.png", fig_bw_ABC,
       width = 12, height = 4.5, dpi = 300)

#----------------------------
#-##### Chapter 5: Statistics and interpretation ####

# Report n as well as means and medians because the groups are uneven and
# the input-rate distributions are skewed. Never interpret SE as SD.

summary_rates <- positive %>%
  group_by(species, time_window) %>%
  summarise(
    n = n(), mean_g_hr = mean(input_g_hr),
    sd_g_hr = sd(input_g_hr), median_g_hr = median(input_g_hr),
    .groups = "drop"
  )

summary_mass_specific <- positive_bw %>%
  group_by(species, time_window) %>%
  summarise(
    n = n(), mean_per_h = mean(input_per_body_mass_h),
    sd_per_h = sd(input_per_body_mass_h),
    median_per_h = median(input_per_body_mass_h),
    .groups = "drop"
  )

print(summary_rates)
print(summary_mass_specific)
write_csv(summary_rates, "paper_analysis/output/summary_positive_input.csv")
write_csv(summary_mass_specific, "paper_analysis/output/summary_mass_specific.csv")

# Pooled Kruskal-Wallis asks whether at least one time-window distribution differs; 
# it does not identify which window differs. 
# Follow it with the BH-adjusted Wilcoxon contrasts below. 
# Because Dec and Dem are pooled here,
# this test alone cannot distinguish a species effect from a time effect.

kw_rate <- kruskal.test(input_g_hr ~ time_window, data = positive)
kw_mass <- kruskal.test(
  input_per_body_mass_h ~ time_window, data = positive_bw
)
print(kw_rate)
print(kw_mass)

pairwise_rate <- positive %>%
  pairwise_wilcox_test(input_g_hr ~ time_window, p.adjust.method = "BH")
print(pairwise_rate)
write_csv(pairwise_rate, "paper_analysis/output/pairwise_positive_input.csv")

# Model checks ask whether the species-by-time pattern differs between Dec
# and Dem. The interaction is the test of that difference. anova(lm(...))
# uses sequential sums of squares, so the term order matters in an unbalanced
# design. Inspect residual diagnostics before relying on linear-model p values.

model_rate <- lm(input_g_hr ~ species * time_window, data = positive)
model_mass <- lm(
  input_per_body_mass_h ~ species * time_window,
  data = positive_bw
)
print(anova(model_rate))
print(summary(model_rate))
print(anova(model_mass))
print(summary(model_mass))

pdf("paper_analysis/output/model_diagnostics.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(model_rate, main = "Input rate model")
plot(model_mass, main = "Mass-specific model")
dev.off()

# Interpretation guide:
# - A small omnibus p value supports some difference among time windows.
# - Consult adjusted pairwise p values for the specific windows; read the
#     medians and raw points to determine the direction of each difference.
# - A non-significant interaction is insufficient evidence to claim identical
#     species patterns. A non-significant species term does not prove equality.
# - All positive-only findings are conditional on excluding non-positive
#     estimates; they do not describe every cicada sampled in the field.
# - Use the current output, not p values copied from a previous run, when
#     writing the paper. Discuss the exclusion and field-duration definition
#     with coauthors before finalizing the Methods and Results.
