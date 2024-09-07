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

vert_data <- read_csv("kvert.csv") 

# Convert to long data set
anova_vert_data <- vert_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "vert_stiffness") 


anova_vert_data <- anova_vert_data %>%
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

anova_vert_data$condition <- as.factor(anova_vert_data$condition)
anova_vert_data$MAS <- as.factor(anova_vert_data$MAS)
anova_vert_data$LOAD <- as.factor(anova_vert_data$LOAD)

# ANOVA Test-----
# 3 x 3 ANOVA 

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_results <- afex::aov_4(vert_stiffness ~ MAS*LOAD + (MAS*LOAD|Participant), 
                             data = anova_vert_data,
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

anova_vert_data %>% 
  dplyr::group_by(condition) %>% 
  rstatix::shapiro_test(vert_stiffness) # shapiro-wilk test on individual groups

### Outliers check --------

anova_vert_data %>%
  group_by(condition) %>%
  identify_outliers(vert_stiffness)

## Plots ---------

## violin

anova_vert_data %>% 
  ggplot(aes(condition, vert_stiffness)) +  
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

anova_vert_data %>% 
  ggplot(aes(sample = vert_stiffness)) +    # make sure to include "sample = DV"
  geom_qq() +                               
  stat_qq_line() +                          
  facet_wrap(~ condition,                   # Panel by group
             labeller = label_both) +    
  theme_bw()

# Remove outliers ---------

normal_vert_data <- anova_vert_data %>%
  filter(Participant != c("17")) %>%
  filter(Participant != c("8"))

### Outliers check --------

normal_vert_data %>%
  group_by(condition) %>%
  identify_outliers(vert_stiffness)

### Normality test -------

normal_vert_data %>% 
  dplyr::group_by(condition) %>% 
  rstatix::shapiro_test(vert_stiffness) # shapiro-wilk test on individual groups

# Summary descriptives with outlier removed ------

adjusted_summary <- normal_vert_data %>% 
  group_by(condition) %>%
  summarize(overall_mean = mean(vert_stiffness,na.rm=TRUE),
            overall_sd = sd(vert_stiffness, na.rm = TRUE),
            count = n())

speed_only_summary <-  normal_vert_data %>% 
  group_by(MAS) %>%
  summarize(overall_mean = mean(vert_stiffness,na.rm=TRUE),
            overall_sd = sd(vert_stiffness, na.rm = TRUE),
            count = n())

load_only_summary <-  normal_vert_data %>% 
  group_by(LOAD) %>%
  summarize(overall_mean = mean(vert_stiffness,na.rm=TRUE),
            overall_sd = sd(vert_stiffness, na.rm = TRUE),
            count = n())

# Plots ---------

## violin

normal_vert_data %>% 
  ggplot(aes(condition, vert_stiffness)) +  
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

anova_results2 <- afex::aov_4(vert_stiffness ~ 1 + (MAS*LOAD|Participant), 
                              data = normal_vert_data,
                              anova_table = list(es = "pes", correction = "HF")) # partial eta squared
anova_results2

summary(anova_results2)

# CI for ES

pes_eta <- eta.F(dfm = anova_results2$anova_table$`num Df`[1], 
                 dfe = anova_results2$anova_table$`den Df`[1], 
                 Fvalue = anova_results2$anova_table$F[1], 
                 a = 0.05)
pes_eta

## Post hoc --------
# post hoc for MAS

mas_posthoc <- anova_results2 %>% 
  emmeans::emmeans(~ MAS)  %>%        # Calculate Estimated Marginal Means for SE and CI
  pairs(adjust = "bonf")
mas_posthoc

confint(mas_posthoc)

# Replication Z-test using Reported Effect Size -----

pes_rep = anova_results2$anova_table$pes[1]
df_rep = anova_results2$anova_table$`den Df`[1]
pes_ori = 0.975 # reported ES in original study
df_ori = 24 # reported in original study

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test = TOSTER::compare_cor(r1 = rho_ori,
                               df1 = df_ori,
                               r2 = rho_rep,
                               df2 = df_rep,
                               alternative = "greater")
rep_test

# Original values

orig_values <- data.frame(
  fval = 319.497,
  df1 = 2,
  df2 = 24,
  reported_es = 0.975
)

## Confirming the reported effect size ----------

ori_es <- eta.F(dfm = orig_values$df1, dfe = orig_values$df2, 
                Fvalue = orig_values$fval, a = 0.05)
ori_es

# cannot confirm reported effect size using reported F-value and degrees of freedom

# Replication Z-test using Computed Effect Size -----

rho_ori_calc = 2*sqrt(ori_es$eta)-1

rep_test = TOSTER::compare_cor(r1 = rho_ori_calc,
                               df1 = df_ori,
                               r2 = rho_rep,
                               df2 = df_rep,
                               alternative = "greater")
rep_test
