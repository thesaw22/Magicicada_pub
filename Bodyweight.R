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


# significance within each species 
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

# difference between species 

kruskal.test(bodyweight_1 ~ species, data = bodyweight_1)

#Kruskal-Wallis rank sum test

#data:  bodyweight_1 by species
#Kruskal-Wallis chi-squared = 44.341, df = 2, p-value = 2.352e-10

# Kruskal is significant 
# hoc test to see what species are significant to each other 
pairwise.wilcox.test(
  bodyweight_1$bodyweight_1,
  bodyweight_1$species,
  p.adjust.method = "BH"
)


#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  bodyweight_1$bodyweight_1 and bodyweight_1$species 

#Cas     Dec    
#Dec 0.02    -      
#Dem 2.8e-06 2.2e-09

#P value adjustment method: BH 

by(bodyweight_1$bodyweight_1, bodyweight_1$species, shapiro.test)

bodyweight_1$species: Cas

Shapiro-Wilk normality test

data:  dd[x, ]
W = 0.92261, p-value = 0.001214

-------------------------------------------------------------------------- 
  bodyweight_1$species: Dec

Shapiro-Wilk normality test

data:  dd[x, ]
W = 0.94297, p-value = 0.008038


-------------------------------------------------------------------------- 
  bodyweight_1$species: Dem

Shapiro-Wilk normality test

data:  dd[x, ]
W = 0.93188, p-value = 0.001964

# plotting the significance 
library(ggpubr)

fig_bw +
  stat_compare_means(
    aes(group = sex),
    method = "wilcox.test",
    label = "p.signif"
  )

#### combined model ####

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

anova_res <- anova(model_bw)

anova_res
______________________________________________________
#Analysis of Variance Table

#Response: rank(bodyweight_1)
#Df Sum Sq Mean Sq  F value Pr(>F)    
#species       2 119055   59527 110.7127 <2e-16 ***
#sex           1 265520  265520 493.8311 <2e-16 ***
#species:sex   2    336     168   0.3128 0.7318    
#Residuals   173  93017     538                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
_______________________________________________________