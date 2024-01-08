library(rstatix)
library(afex)
library(car)
library(broom)
library(emmeans)
library(stringr)
library(lmerTest)
library(tidyverse)
library(MOTE)

# Data prep --------------------

data <- read_csv("kleg.csv") 

# Convert to long data set
anova_data <- data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "leg_stiffness") 

  
anova_data <- anova_data %>%
  mutate(MAS =  case_when(
    condition %in% c("60% MAS - 0% LOAD", "60% MAS - 10% LOAD", "60% MAS - 20% LOAD") ~ "MAS_60",
    condition %in% c("80% MAS - 0% LOAD", "80% MAS - 10% LOAD", "80% MAS - 20% LOAD") ~ "MAS_80",
    condition %in% c("100% MAS - 0% LOAD", "100% MAS - 10% LOAD", "100% MAS - 20% LOAD") ~ "MAS_100"
    ),
    LOAD = case_when(
      condition %in% c("60% MAS - 0% LOAD", "80% MAS - 0% LOAD", "100% MAS - 0% LOAD") ~ "LOAD_0",
      condition %in% c("60% MAS - 10% LOAD", "80% MAS - 10% LOAD", "100% MAS - 10% LOAD") ~ "LOAD_10",
      condition %in% c("60% MAS - 20% LOAD", "80% MAS - 20% LOAD", "100% MAS - 20% LOAD") ~ "LOAD_20"
    ))

anova_data$condition <- as.factor(anova_data$condition)
anova_data$MAS <- as.factor(anova_data$MAS)
anova_data$LOAD <- as.factor(anova_data$LOAD)

# ANOVA Test-----
# 3 x 3 ANOVA 

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_results <- afex::aov_4(leg_stiffness ~ MAS*LOAD + (MAS*LOAD|Participant), 
                                   data = anova_data,
                                   anova_table = list(es = "pes"))
anova_results

summary(anova_results)

anova_results %>% 
  emmeans::emmeans(~ MAS)  %>%        # Calculate Estimated Marginal Means for SE and CI
  contrast(adjust = "bonferroni")

anova_results %>% 
  emmeans::emmeans(~ LOAD)  %>%        # Calculate Estimated Marginal Means for SE and CI
  contrast(adjust = "bonferroni")

## Resolving assumptions --------

### Normality test -------

anova_data %>% 
  dplyr::group_by(condition) %>% 
  rstatix::shapiro_test(leg_stiffness) # shapiro-wilk test on individual groups

### Outliers check --------

anova_data %>%
  group_by(condition) %>%
  identify_outliers(leg_stiffness)

## Plots ---------

## violin

anova_data %>% 
  ggplot(aes(condition, leg_stiffness)) +  
  geom_violin(fill = "gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               color = "red",
               size = 5) +
  theme_bw()

# qq plot

anova_data %>% 
  ggplot(aes(sample = leg_stiffness)) +    # make sure to include "sample = DV"
  geom_qq() +                               
  stat_qq_line() +                          
  facet_wrap(~ condition,                   # Panel by group
             labeller = label_both) +    
  theme_bw()

# Remove outliers ---------

adjusted_data <- anova_data %>%
 filter(Participant != c("2")) %>%
 filter(Participant != c("8"))

### Outliers check --------

adjusted_data %>%
  group_by(condition) %>%
  identify_outliers(leg_stiffness)

# Replace another outlier with mean substitution

normal_data <- adjusted_data %>%
  filter(condition == "100% MAS - 0% LOAD") %>%
  mutate(leg_stiffness = replace(leg_stiffness, leg_stiffness > 15, NA),
         leg_stiffness = replace_na(leg_stiffness, mean(leg_stiffness, na.rm = TRUE)))

adjusted_data <- adjusted_data %>%
  filter(condition != "100% MAS - 0% LOAD")

normal_anova_data <- full_join(x = normal_data, 
                      y = adjusted_data, 
                      by = c("Participant","leg_stiffness", "condition", "MAS", "LOAD"))

### Normality test -------

normal_anova_data %>% 
  dplyr::group_by(condition) %>% 
  rstatix::shapiro_test(leg_stiffness) # shapiro-wilk test on individual groups

### Outliers check 

normal_anova_data %>%
  group_by(condition) %>%
  identify_outliers(leg_stiffness)

# Summary descriptives with outlier removed ------

adjusted_summary <- normal_anova_data %>% 
  group_by(condition) %>%
  summarize(overall_mean = mean(leg_stiffness,na.rm=TRUE),
            overall_sd = sd(leg_stiffness, na.rm = TRUE),
            count = n())

# Plots ---------

## violin

normal_anova_data %>% 
  ggplot(aes(condition, leg_stiffness)) +  
  geom_violin(fill = "gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               color = "red",
               size = 5) +
  theme_bw()

# ANOVA - outlier removed ------

anova_results2 <- afex::aov_4(leg_stiffness ~ 1 + (MAS*LOAD|Participant), 
                              data = normal_anova_data,
                              anova_table = list(es = "pes", correction = "HF")) # partial eta squared
anova_results2

summary(anova_results2)

# CI for ES

eta <- eta.F(dfm = anova_results2$anova_table$`num Df`[1], 
             dfe = anova_results2$anova_table$`den Df`[1], 
             Fvalue = anova_results2$anova_table$F[1], 
             a = 0.05)
eta

## Post hoc --------
# post hoc for MAS

mas_posthoc <- anova_results2 %>% 
  emmeans::emmeans(~ MAS)  %>%        # Calculate Estimated Marginal Means for SE and CI
  pairs(adjust = "bonf")
mas_posthoc
        
confint(mas_posthoc)

# post hoc for LOAD

load_posthoc <- anova_results2 %>% 
  emmeans::emmeans(~ LOAD)  %>%        # Calculate Estimated Marginal Means for SE and CI
  pairs(adjust = "bonf") 
load_posthoc

confint(load_posthoc)

# Replication test -----

pes_rep = anova_results2$anova_table$pes[1]
df_rep = anova_results2$anova_table$`den Df`[1]
pes_ori = 0.901 # reported ES in original study
df_ori = 24 # reported in original study

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test = TOSTER::compare_cor(r2 = rho_ori,
                               df2 = df_ori,
                               r1 = rho_rep,
                               df1 = df_rep,
                               alternative = "less")
rep_test

