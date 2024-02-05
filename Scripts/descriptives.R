library(rstatix)
library(tidyverse)

# Contact time data --------------------

contact_time_data <- read_csv("contact_time.csv") 

long_contact_time_data <- contact_time_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "contact_time") 

# Flight time data --------------------

flight_time_data <- read_csv("flight_time.csv") 

long_flight_time_data <- flight_time_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "flight_time") 

# Step frequency data --------------------

step_freq_data <- read_csv("step_frequency.csv") 

long_step_freq_data <- step_freq_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "step_freq") 

# Step length data --------------------

step_length_data <- read_csv("step_length.csv") 

long_step_length_data <- step_length_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "step_length") 


# Fpeak data --------------------

fpeak_data <- read_csv("fpeak.csv") 

long_fpeak_data <- fpeak_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "fpeak") 


# Change in leg length data --------------------

change_leg_length_data <- read_csv("change_leg_length.csv") 

long_change_leg_length_data <- change_leg_length_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "change_leg_length") 


# Change in Y data --------------------

change_y_data <- read_csv("change_y.csv") 

long_change_y_data <- change_y_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "change_y") 


# Vertical stiffness --------------------

vert_data <- read_csv("kvert.csv") 

# Convert to long data set
long_vert_data <- vert_data %>%
  select(Participant, `60% MAS - 0% LOAD`, `60% MAS - 10% LOAD`, `60% MAS - 20% LOAD`,
         `80% MAS - 0% LOAD`, `80% MAS - 10% LOAD`, `80% MAS - 20% LOAD`, 
         `100% MAS - 0% LOAD`, `100% MAS - 10% LOAD`, `100% MAS - 20% LOAD`) %>%
  pivot_longer(cols = contains("MAS"),
               names_to = "condition",
               values_to = "vert_stiffness") 

# Combine datasets ---------

combined_data <- long_contact_time_data %>%
  left_join(long_step_freq_data, by = c("Participant", "condition")) %>%
  left_join(long_step_length_data, by = c("Participant", "condition")) %>%
  left_join(long_change_y_data, by = c("Participant", "condition")) %>%
  left_join(long_change_leg_length_data, by = c("Participant", "condition")) %>%
  left_join(long_flight_time_data, by = c("Participant", "condition")) %>%
  left_join(long_fpeak_data, by = c("Participant", "condition")) %>%
  left_join(long_vert_data, by = c("Participant", "condition"))

# Descriptives -------------

summary <- combined_data %>% 
  group_by(condition) %>%
  summarize(count = n(),
            mean_contacttime = mean(contact_time,na.rm=TRUE),
            sd_contacttime = sd(contact_time, na.rm = TRUE),
            mean_flighttime = mean(flight_time,na.rm=TRUE),
            sd_flighttime = sd(flight_time, na.rm = TRUE),
            mean_steplength = mean(step_length,na.rm=TRUE),
            sd_steplength = sd(step_length, na.rm = TRUE),
            mean_stepfreq = mean(step_freq,na.rm=TRUE),
            sd_step_freq = sd(step_freq, na.rm = TRUE),
            mean_change_y = mean(change_y,na.rm=TRUE),
            sd_change_y = sd(change_y, na.rm = TRUE),
            mean_change_leg_length = mean(change_leg_length,na.rm=TRUE),
            sd_change_leg_length = sd(change_leg_length, na.rm = TRUE),
            mean_fpeak = mean(fpeak,na.rm=TRUE),
            sd_fpeak = sd(fpeak, na.rm = TRUE),
            mean_vert = mean(vert_stiffness,na.rm=TRUE),
            sd_vert = sd(vert_stiffness, na.rm = TRUE))

# exporting a file as a .csv
#summary %>%
#  select(everything()) %>% 
#  readr::write_csv("descriptives.csv", na="")