# Readind patterns of the new VISTA data
# Author: Sapan Tiwari
# 24 July 2024

# The input is the outputs from mergeVISTAData1223 code. The input files contain the combine trip, house and person data for 12-23.
# This code filters out the trips for CoGB and learn about the activity and mode patterns.


rm(list = ls())


library(dplyr)
library(readr)
library(ggplot2)

# Analysis  trips data-----------------------------------------------------------------------------------------------------------------
trip_data <- read_csv("../data/trip_final.csv")

person_data <- read_csv("../data/person_final.csv")

home_data <- read_csv("../data/home_final.csv")

# Add the home locations to each trips 

expanded_trip_data <- trip_data %>%
  left_join(home_data %>% select(hhid, sa1, sa2, sa2_name, sa3, sa3_name), by = "hhid")

# Filter home locations for CoGB

filter_values <- c(20201, 20202, 20203)

filtered_trip_data <- expanded_trip_data %>%
  filter(sa3 %in% filter_values)

# print out trips for Greater Bendigo region

write_csv(filtered_trip_data, "../data/Greater_Bendigo_trip_data.csv")


# Count the values for each distinct purpose and mode

origpurp1_counts <- filtered_trip_data %>%
  group_by(origpurp1) %>%
  summarise(count = n())

destpurp1_counts <- filtered_trip_data %>%
  group_by(destpurp1) %>%
  summarise(count = n())

linkmode_counts <- filtered_trip_data %>%
  group_by(linkmode) %>%
  summarise(count = n())

