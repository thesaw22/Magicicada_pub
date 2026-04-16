# Wrtiing code for the bodyweight of Dem, Dec and Cas 
# needed for species differences 
# needed for sex differences 

library(readr)
CicTen <- read_delim("data/CicTen.csv", delim = ";", 
                     escape_double = FALSE, col_types = cols(`weight 1` = col_number()), 
                     locale = locale(decimal_mark = ","), 
                     trim_ws = TRUE)
View(CicTen)

bodyweight_1 <- CicTen %>%
  mutate(
    species = trimws(species),
    sex = trimws(sex)
  ) %>%
  filter(species %in% c("Dec", "Dem", "Cas")) %>%
  select(Number, species, sex, `weight 1`) %>%
  rename(bodyweight_1 = `weight 1`) %>%
  arrange(species, sex)

View(bodyweight_1)

# color palette

cols_all <- palette.colors(12, palette = "Paired")

barplot(rep(1, length(cols_all)),
        col = cols_all,
        names.arg = 1:length(cols_all),
        border = NA,
        main = "Paired palette (indexed)")

library(ggplot2)

# plot
fig_bw <- ggplot(bodyweight_1,
                 aes(x = species,
                     y = bodyweight_1,
                     color = species,
                     shape = sex)) +
  
  geom_jitter(
    width = 0.15,
    size = 3,
    alpha = 0.9
  ) +
  
  scale_color_manual(
    values = c(
      "Cas" = cols_all[10],
      "Dec" = cols_all[9],
      "Dem" = cols_all[12]
    ),
    guide = "none"
  ) +
  
  scale_shape_manual(
    values = c(
      "female" = 15,   # square
      "male" = 7    # squar ewith cross
    ), 
  ) +
  
  
  labs(
    x = "Species",
    y = "Bodyweight (g)",
    color = "Species",
    shape = "Sex"
  ) +
  
  theme_classic() +
  
   theme(
    legend.position = c(0.05, 0.95),   # (x, y) in [0–1]
    legend.justification = c(0, 1)     # anchor top-left corner
  )

fig_bw

#### significance test ####

library(dplyr)


### 1. Sex differences within species
sex_tests <- bodyweight_1 %>%
  group_by(species) %>%
  summarise(
    p_value = wilcox.test(bodyweight_1 ~ sex)$p.value,
    .groups = "drop"
  )

sex_tests

#species  p_value
#<chr>      <dbl>
#1 Cas     1.30e-14
#2 Dec     9.20e-15
#3 Dem     2.00e-16

# Interpretation:
# All species (Cas, Dec, Dem) show extremely small p-values (~10^-14 to 10^-16).
# → Strong evidence that males and females differ in bodyweight within EACH species.
# → This is a very robust effect (not borderline significant).

## 2. Differences between species

kruskal.test(bodyweight_1 ~ species, data = bodyweight_1)

#Kruskal-Wallis rank sum test

#data:  bodyweight_1 by species
#Kruskal-Wallis chi-squared = 44.341, df = 2, p-value = 2.352e-10

# Interpretation:
# Kruskal-Wallis test is highly significant (p ≈ 2.35e-10).
# → At least one species differs in bodyweight from the others.
# → This just tells us "there is a difference", not WHICH species differ.

## 3. Post-hoc pairwise comparisons
# hoc test to see what species are significant to each other 
pairwise.wilcox.test(
  bodyweight_1$bodyweight_1,
  bodyweight_1$species,
  p.adjust.method = "BH"
)

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  bodyweight_1$bodyweight_1 and bodyweight_1$species 

#Cas          Dec    
#Dec 0.02    -      
#Dem 2.8e-06 2.2e-09

#P value adjustment method: BH 

# Interpretation (adjusted p-values):
# Cas vs Dec → p = 0.02
#   → Significant, but weaker than others
# Cas vs Dem → p = 2.8e-06
#   → Very strong difference
# Dec vs Dem → p = 2.2e-09
#   → Extremely strong difference
#
# Overall:
# → All species differ from each other
# → Dem seems most distinct (very small p-values vs both others)

## 4. Normality check
by(bodyweight_1$bodyweight_1, bodyweight_1$species, shapiro.test)

___________________________________________________
#bodyweight_1$species: Cas

#Shapiro-Wilk normality test

#data:  dd[x, ]
#W = 0.92261, p-value = 0.001214

#---
#  bodyweight_1$species: Dec

#Shapiro-Wilk normality test

#data:  dd[x, ]
#W = 0.94297, p-value = 0.008038


#---
#  bodyweight_1$species: Dem

#Shapiro-Wilk normality test

#data:  dd[x, ]
#W = 0.93188, p-value = 0.001964
______________________________________________

# Interpretation:
# All species have p < 0.01
# → Data significantly deviates from normality
# → Justifies using non-parametric tests (Wilcoxon, Kruskal-Wallis)
# → Your statistical choices above are appropriate

## 5. Plotting significance

# plotting the significance 
library(ggpubr)

fig_bw +
  stat_compare_means(
    aes(group = sex),
    method = "wilcox.test",
    label = "p.signif"
  )

# Interpretation:
# Adds significance stars for male vs female within each species
# → Visual confirmation of strong sex effect

## 6. Combined model (rank-based ANOVA)

model_bw <- lm(
  rank(bodyweight_1) ~ species * sex,
  data = bodyweight_1
)

anova(model_bw)

_____________________________________________________
#Analysis of Variance Table

#Response: rank(bodyweight_1)
#Df Sum Sq Mean Sq  F value Pr(>F)    
#species       2 119055   59527 110.7127 <2e-16 ***
#sex           1 265520  265520 493.8311 <2e-16 ***
#species:sex   2    336     168   0.3128 0.7318    
#Residuals   173  93017     538                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
______________________________________________________

# species effect:
# F = 110.7, p < 2e-16
# → Strong evidence that bodyweight differs between species

# sex effect:
# F = 493.8, p < 2e-16
# → VERY strong effect of sex on bodyweight
# → This is the dominant factor in your dataset

# interaction (species:sex):
# F = 0.31, p = 0.73 (NOT significant)
# → The effect of sex is consistent across species
# → In other words:
#    - Males vs females differ
#    - BUT the magnitude of that difference does NOT depend on species


## 7. Final biological interpretation

# 1. Sex strongly affects bodyweight (in all species)
# 2. Species differ in bodyweight overall
# 3. No interaction:
#    → Sex differences are parallel across species
#    → e.g., if females are heavier, they are heavier in ALL species similarly

# Clean takeaway:
# → Bodyweight is structured by BOTH species and sex,
#   but these effects act independently (no interaction)