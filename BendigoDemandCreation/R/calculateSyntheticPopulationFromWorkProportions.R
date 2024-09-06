# Author : Sapan Tiwari
# 29 August 2024

# This file filtered out the Synthetic Pop from VITM2 and gives output which has all persons from CoGB and people who travel to CoGB for work.

library(dplyr)
library(tidyr)
library(data.table)

# SA3 to SA3 JTW data

sa3_work_trips <- read.csv("../data/victoria_sa3_to_sa3.csv")

sa3_work_bendigo <- sa3_work_trips %>%
  select(sa3, X20201, X20202, X20203)

# Number of residents in each sa3

sa3_residence_count <- read.csv("../data/sa3_residence.csv")

sa3_work_bendigo <- sa3_work_bendigo %>%
  left_join(sa3_residence_count, by = "sa3")

# Calculate work proportions and remove the population column

sa3_work_proportions <- sa3_work_bendigo %>%
  mutate(
    X20201 = X20201 / population,
    X20202 = X20202 / population,
    X20203 = X20203 / population,
    combined_work_proportion = X20201 + X20202 + X20203
  ) %>%
  select(-population)

sa3_work_proportions <- sa3_work_proportions %>%
  mutate(across(everything(), as.character))

# get the synthetic household and person data from VITM2

household_pop <- read.csv("../data/synthetic_households.csv")


person_pop <- read.csv("../data/synthetic_persons.csv")

person_pop <- person_pop%>%
  mutate(sa3 = substr(SA2, 1, 5))  # Extract the first 5 digits to get SA3

person_pop <- person_pop %>%
  mutate(across(everything(), as.character))

# append the work_proportion_probabilty for each sa3 to another 

person_pop <- person_pop %>%
  left_join(
    sa3_work_proportions %>% select(sa3, combined_work_proportion), 
    by = "sa3"
  )

# account for the MAIN_ACTIVITY = 7 (Full-time workers)

activity_ratio <- person_pop %>%
  group_by(sa3) %>%
  summarise(
    total_population = n(),
    activity_7_population = sum(MAIN_ACTIVITY == "7", na.rm = TRUE),
    ratio = ifelse(total_population > 0, activity_7_population / total_population, 0)
  )

# adjust the proportion to account for full_time_person

sa3_work_proportions_adjusted <- sa3_work_proportions %>%
  left_join(activity_ratio, by = "sa3") %>%
  mutate(
    combined_work_proportion = as.numeric(combined_work_proportion),
    ratio = as.numeric(ratio),
    inverted_ratio = ifelse(ratio > 0, 1 / ratio, 0),
    adjusted_work_proportion = combined_work_proportion * inverted_ratio
  ) %>%
  select(sa3, adjusted_work_proportion)

# create the final synthetic pop-------------------------------------------------------------------------

# All persons with household in CoGB

bendigo_pop <- person_pop %>%
  filter(sa3 %in% c("20201", "20202", "20203"))

# Persons travelling to CoGB for work based on the adjusted probability--------------and all the CoGB based----------------------------

filtered_persons <- person_pop %>%
  left_join(sa3_work_proportions_adjusted, by = "sa3") %>%
  rowwise() %>%
  mutate(keep_entry = ifelse(sa3 %in% c("20201", "20202", "20203"),
                             TRUE,  # Keep all entries for SA3 20201, 20202, 20203
                             rbinom(1, 1, as.numeric(adjusted_work_proportion)) == 1& MAIN_ACTIVITY == "7")) %>%
  filter(keep_entry) %>%
  select(-keep_entry)


##------------Estimate the difference in 2016 ABS and 2018 VITM population size and use that in estimating the required outside persons

real_population_2016 <- sum(sa3_residence_count$population, na.rm = TRUE)

synthetic_pop_2018 <- nrow(person_pop)

percentage_improvement <- ((synthetic_pop_2018 - real_population_2016) / real_population_2016) * 100


total_work_trips_ABS <- sa3_work_bendigo %>%
  filter(!sa3 %in% c("20201", "20202", "20203")) %>%
  summarise(
    total_X20201 = sum(X20201, na.rm = TRUE),
    total_X20202 = sum(X20202, na.rm = TRUE),
    total_X20203 = sum(X20203, na.rm = TRUE)
  )



grand_total_work_trips_filtered = sum(total_work_trips_ABS)

required_outside_persons <- grand_total_work_trips_filtered * (1 + (percentage_improvement / 100))

created_outside_persons <- nrow(filtered_persons) - nrow(bendigo_pop)

outside_persons_difference <- required_outside_persons - created_outside_persons 

outside_persons_difference_perc <- outside_persons_difference /required_outside_persons*100


# Filter out the relevant households

household_ids_in_filtered_persons <- filtered_persons$household_id

filtered_household_pop <- household_pop %>%
  filter(household_id %in% household_ids_in_filtered_persons)
