# This code takes the SA1 population from ABS (Including their gender and age information). The other input is the synthetic population from VITM 2.
# the output is the updated synthetic population with the assigned SA1 locations to each entry, which can be directly used in the sampling step.


library(data.table)
library(dplyr)
library(sf)
library(tidyr)



population <- read.csv("../data/sa1_population_sex_age.csv")
sa1Aust <- read.csv("../data/SA1_2016_AUST.csv")
syntPop <- read.csv("../data/persons.csv")

population <- population %>%
  mutate(SA1_7DIGCODE = as.character(SA1_7DIGCODE))

sa1Aust <- sa1Aust %>%
  mutate(SA1_7DIGITCODE_2016 = as.character(SA1_7DIGITCODE_2016))

population <- population %>%
  fill(SA1_7DIGCODE, .direction = "down")

# Merge the datasets based on the SA1 codes

population <- population %>%
  left_join(sa1Aust, by = c("SA1_7DIGCODE" = "SA1_7DIGITCODE_2016")) %>%
  select(SA1_7DIGCODE, SA1_MAINCODE_2016, SA2_MAINCODE_2016, sex, X0.9.years, X10.19.years, X20.29.years, X30.39.years, 
         X40.49.years, X50.59.years, X60.69.years, X70.79.years, X80.89.years, X90.99.years, X100.years.and.over)

# Expand the dataset

population_long <- population %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "age_range",
               values_to = "count")

# add the respective total of each SA2 and estimate the proportions

population_long <- population_long %>%
  group_by(SA2_MAINCODE_2016, sex, age_range) %>%
  mutate(total_sa2_population = sum(count, na.rm = TRUE)) %>%
  ungroup()

population_long <- population_long %>%
  mutate(pop_probability = count / total_sa2_population)

# modify the NA probability to 0

population_long <- population_long %>%
  mutate(pop_probability = ifelse(is.na(pop_probability), 0, pop_probability))

# Convert SA2_MAINCODE_2016 in both dataset to character

population_long <- population_long %>%
  mutate(SA2_MAINCODE_2016 = as.character(SA2_MAINCODE_2016))

syntPop <- syntPop %>%
  mutate(SA2 = as.character(SA2))

# Add the sex and divided syn pop age in the age group

syntPop <- syntPop %>%
  mutate(sex = case_when(
    SEXP == 1 ~ "Male",
    SEXP == 2 ~ "Female"
  ))

syntPop <- syntPop %>%
  mutate(age_range = case_when(
    AGE_YRS <= 9 ~ "X0.9.years",
    AGE_YRS >= 10 & AGE_YRS <= 19 ~ "X10.19.years",
    AGE_YRS >= 20 & AGE_YRS <= 29 ~ "X20.29.years",
    AGE_YRS >= 30 & AGE_YRS <= 39 ~ "X30.39.years",
    AGE_YRS >= 40 & AGE_YRS <= 49 ~ "X40.49.years",
    AGE_YRS >= 50 & AGE_YRS <= 59 ~ "X50.59.years",
    AGE_YRS >= 60 & AGE_YRS <= 69 ~ "X60.69.years",
    AGE_YRS >= 70 & AGE_YRS <= 79 ~ "X70.79.years",
    AGE_YRS >= 80 & AGE_YRS <= 89 ~ "X80.89.years",
    AGE_YRS >= 90 & AGE_YRS <= 99 ~ "X90.99.years",
    AGE_YRS >= 100 ~ "X100.years.and.over"
  ))


# The function to assign SA1 based on sex and age range
assign_sa1 <- function(sa2_code, sex, age_range, prob_df) {
  # Filter the population probabilities for the given SA2, sex, and age range
  probs <- prob_df %>%
    filter(SA2_MAINCODE_2016 == sa2_code, sex == sex, age_range == age_range) %>%
    select(SA1_MAINCODE_2016, SA1_7DIGCODE, pop_probability)
  
  # If no SA1 found for this SA2, sex, and age range, return NAs
  
  if (nrow(probs) == 0) {
    return(data.frame(SA1_MAINCODE_2016 = NA, SA1_7DIGCODE = NA))
  }
  
  # Sample an SA1 based on the population probabilities
  
  selected_sa1 <- probs %>%
    sample_n(1, weight = pop_probability)
  
  return(selected_sa1)
}

# Assign SA1_MAINCODE_2016 and SA1_7DIGCODE to each entry in syntPop based on sex and age range

syntPop <- syntPop %>%
  rowwise() %>%
  mutate(assigned_sa1 = list(assign_sa1(SA2, sex, age_range, population_long))) %>%
  unnest(cols = c(assigned_sa1)) %>%
  ungroup()

sa1_counts <- syntPop %>%
  count(SA1_MAINCODE_2016, name = "count")

# Remove the NA SA1s

syntPop <- syntPop %>%
  filter(!is.na(SA1_MAINCODE_2016) & !is.na(SA1_7DIGCODE))

# Remove the processed columns which are not required 

syntPop_cleaned <- syntPop %>%
  select(-combined_work_proportion, -adjusted_work_proportion, -sex, -age_range, -pop_probability)



write.csv(syntPop_cleaned, "../data/final_persons.csv", row.names = FALSE)
