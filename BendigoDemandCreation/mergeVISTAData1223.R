# Merging the raw datafiles of new VISTA data and creating the final files for analysis
# Author: Sapan Tiwari
# 23 July 2024

# The input is the VISTA 12-20 and 22-23 household, person and trip  data, and at the end it provides the final data for year 12-23, with the relavant columns needed for the analysis.


rm(list = ls())


library(dplyr)
library(readr)

# Analysis and merge trips data-----------------------------------------------------------------------------------------------------------------
trip_12_20 <- read_csv("VISTA_2012_20/T_r1.csv")


trip_22_23 <- read_csv("VISTA_2022_23/VISTA_2022_23_Trips.csv")

trip_12_20 <- trip_12_20 %>%
  rename_all(tolower)

trip_22_23 <- trip_22_23 %>%
  rename(hhid = hhid_TRIPS)

# Get the column names
columns_12_20 <- colnames(trip_12_20)
columns_22_23 <- colnames(trip_22_23)

# Find common columns
common_columns <- intersect(columns_12_20, columns_22_23)
num_common_columns <- length(common_columns)

# Convert column names to lowercase and columns to character type

trip_12_20 <- trip_12_20 %>%
  select(all_of(common_columns)) %>%
  mutate(across(everything(), as.character))

trip_22_23 <- trip_22_23 %>%
  select(all_of(common_columns)) %>%
  mutate(across(everything(), as.character))

unique_to_12_20 <- setdiff(columns_12_20, common_columns)
unique_to_22_23 <- setdiff(columns_22_23, common_columns)

cat("Columns unique to trip_12_20:\n")
print(unique_to_12_20)

cat("\nColumns unique to trip_22_23:\n")
print(unique_to_22_23)


# Merge datasets based on common columns

trip_final <- full_join(trip_12_20, trip_22_23, by = common_columns)

write_csv(trip_final, "../data/trip_final.csv")


# Analysis and merge household data-----------------------------------------------------------------------------------------------------------------

home_12_20 <- read_csv("Vista Data 12-23/VISTA_2012_20/H.csv")

home_22_23 <- read_csv("VISTA_2022_23/VISTA_2022_23_Household.csv")

geo_data <- read_csv("VISTA_2012_20/GEO.csv")

geo_data <- geo_data %>%
  rename_all(tolower) %>%
  mutate(across(everything(), as.character))


home_12_20 <- home_12_20 %>%
  rename_all(tolower) %>%
  mutate(across(everything(), as.character))

home_22_23 <- home_22_23 %>%
  rename_all(tolower) %>%
  mutate(across(everything(), as.character))

# Merge household data with GEO data

home_12_20 <- home_12_20 %>%
  left_join(geo_data %>% select(mb, sa1, sa2, sa2_name, sa3, sa3_name), by = c("homemeshblock" = "mb"))

# change the column names and get the sa1-sa3 data from 22-23 data

home_22_23 <- home_22_23 %>%
  rename_all(tolower) %>%
  rename(
    sa1 = home_sa1_code,
    sa2 = home_sa2_code,
    sa2_name = home_sa2_name,
    sa3 = home_sa3_code,
    sa3_name = home_sa3_name
  ) %>%
  mutate(across(everything(), as.character))

home_22_23 <- home_22_23 %>%
  rename(centroid_long = randhomelong, centroid_lat = randhomelat)

# Get the column names
columns_home_12_20 <- colnames(home_12_20)
columns_home_22_23 <- colnames(home_22_23)

# Find common columns
common_columns_home <- intersect(columns_home_12_20, columns_home_22_23)
num_common_columns_home <- length(common_columns_home)

unique_to_home_12_20 <- setdiff(columns_home_12_20, common_columns_home)
unique_to_home_22_23 <- setdiff(columns_home_22_23, common_columns_home)

cat("Columns unique to home_12_20:\n")
print(unique_to_home_12_20)

cat("\nColumns unique to home_22_23:\n")
print(unique_to_home_22_23)

home_12_20 <- home_12_20 %>%
  select(all_of(common_columns_home))

home_22_23 <- home_22_23 %>%
  select(all_of(common_columns_home))


home_final <- full_join(home_12_20, home_22_23, by = common_columns_home)


write_csv(home_final, "../data/home_final.csv")



# Analysis and merge persons data-----------------------------------------------------------------------------------------------------------------

person_12_20 <- read_csv("Vista Data 12-23/VISTA_2012_20/P.csv")

person_22_23 <- read_csv("VISTA_2022_23/VISTA_2022_23_Person.csv")


person_12_20 <- person_12_20 %>%
  rename_all(tolower) %>%
  mutate(across(everything(), as.character))

person_22_23 <- person_22_23 %>%
  rename_all(tolower) %>%
  mutate(across(everything(), as.character))

# Get the column names
columns_person_12_20 <- colnames(person_12_20)
columns_person_22_23 <- colnames(person_22_23)

# Find common columns
common_columns_person <- intersect(columns_person_12_20, columns_person_22_23)
num_common_columns_person <- length(common_columns_person)

# Identify unique columns
unique_to_person_12_20 <- setdiff(columns_person_12_20, common_columns_person)
unique_to_person_22_23 <- setdiff(columns_person_22_23, common_columns_person)


cat("Columns unique to person_12_20:\n")
print(unique_to_person_12_20)

cat("\nColumns unique to person_22_23:\n")
print(unique_to_person_22_23)

# Select only the common columns
person_12_20 <- person_12_20 %>%
  select(all_of(common_columns_person))

person_22_23 <- person_22_23 %>%
  select(all_of(common_columns_person))

# Merge datasets based on common columns
person_final <- full_join(person_12_20, person_22_23, by = common_columns_person)

write_csv(person_final, "../data/person_final.csv")
