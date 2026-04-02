# Data validation check

##### Dec und Dem #####
#some of the data experience mistakes in the field
#checking which ones are usuable and which ones arent

library(readr)
cicten_tidy <- read_csv("data/cicten_tidy.csv")
View(cicten_tidy)

#splitting the data into two groups 
# one group oncludes data points with questionable results 

library(readr)
library(dplyr)
library(ggplot2)

cicten_tidy <- read_csv("data/cicten_tidy.csv")

questionable_ids <- c(
  24, 39, 40, 42, 44, 47,
  57:61, 63,
  84:86,
  96,
  112,
  117:120, 122, 123,
  137, 138, 141, 145,
  148, 149, 151,
  161, 162
)

pee_data <- cicten_tidy %>%
  mutate(number = as.numeric(gsub("#", "", number))) %>%
  filter(action == "pee") %>%
  mutate(group = ifelse(number %in% questionable_ids, "Questionable", "Good"))

ggplot(pee_data, aes(x = group, y = weight, fill = group)) +
  geom_boxplot(width = 0.5, alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Pee weight in Good vs Questionable groups",
    x = "Group",
    y = "Pee weight"
  ) +
  theme(legend.position = "none")

# -> result shows a higher mean for the questionable ones
# suprising because the questionable ones ar edead ones or with holes 

# generating a 3rd list with maybes ( Individuals, who didnt die or lost excretion completly)
library(readr)
library(dplyr)
library(ggplot2)

cicten_tidy <- read_csv("data/cicten_tidy.csv")

# Define groups
maybe_ids <- c(57:61, 96, 123, 141)

questionable_ids <- c(
  24, 39, 40, 42, 44, 47,
  57:61, 63,
  84:86,
  96,
  112,
  117:120, 122, 123,
  137, 138, 141, 145,
  148, 149, 151,
  161, 162
)

# Remove "maybe" IDs from questionable
questionable_ids <- setdiff(questionable_ids, maybe_ids)

pee_data <- cicten_tidy %>%
  mutate(number = as.numeric(gsub("#", "", number))) %>%
  filter(action == "pee") %>%
  mutate(
    group = case_when(
      number %in% maybe_ids ~ "Maybe",
      number %in% questionable_ids ~ "Questionable",
      TRUE ~ "Good"
    )
  )

ggplot(pee_data, aes(x = group, y = weight, fill = group)) +
  geom_boxplot(width = 0.5, alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Pee weight across data quality groups",
    x = "Group",
    y = "Pee weight"
  ) +
  theme(legend.position = "none")

# Testing the statistical difference between good and maybe
wilcox.test(weight ~ group,
            data = pee_data %>% filter(group %in% c("Good", "Maybe")))

#Wilcoxon rank sum test with continuity correction

#data:  weight by group
#W = 124, p-value = 0.722
#alternative hypothesis: true location shift is not equal to 0

# Inducing higher precision for paper: 
#   removing possible pseudoreplication
pee_summary <- pee_data %>%
  group_by(number, group) %>%
  summarise(mean_pee = mean(weight, na.rm = TRUE), .groups = "drop")

wilcox.test(mean_pee ~ group,
            data = pee_summary %>% filter(group %in% c("Good", "Maybe")))

#Wilcoxon rank sum test with continuity correction

#data:  mean_pee by group
#W = 119, p-value = 0.7789
#alternative hypothesis: true location shift is not equal to 0

# Calculating Input rate and compaaring "Good" and "Maybe"
#   removing questionable data completly (dead in field)

library(readr)
library(dplyr)

cicten_tidy <- read_csv("data/cicten_tidy.csv")

maybe_ids <- c(57:61, 96, 123, 141)

questionable_ids <- c(
  24, 39, 40, 42, 44, 47,
  57:61, 63,
  84:86,
  96,
  112,
  117:120, 122, 123,
  137, 138, 141, 145,
  148, 149, 151,
  161, 162
)

input_summary <- cicten_tidy %>%
  mutate(number = as.numeric(gsub("#", "", number))) %>%
  
  # remove all questionable IDs first
  filter(!number %in% setdiff(questionable_ids, maybe_ids)) %>%
  
  # now only Good and Maybe remain
  mutate(
    group = ifelse(number %in% maybe_ids, "Maybe", "Good")
  ) %>%
  
  group_by(number, species, sex, group) %>%
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

# visualizing the difference first
library(ggplot2)

ggplot(input_summary, aes(x = group, y = input, fill = group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none")

# running the stats test 
wilcox.test(input ~ group, data = input_summary)

#Wilcoxon rank sum test with continuity correction

#data:  input by group
#W = 269.5, p-value = 0.6983
#alternative hypothesis: true location shift is not equal to 0

#-> maybe group behaves exactly like the good group, meaning data can be added
