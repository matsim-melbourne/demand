# File to create the csv plan for dummy trips
# Author: Sapan Tiwari
# 23 May 2024

#  The input file here is the table downloaded from ABS, which contains the home locations for entire victoria (SA4 level), work location CoGB (SA2) and mode of travel

# IN the end it produces a dummyPlan.csv file, which can be used to get the dummy plans in csv

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Read the table extraced from ABS

data <- read_csv("../dataJTW/SA4_home_to_SA2_work_withMode.csv")

# Fill the empty rows with the location name apprearing just before this row

fill_down <- function(x) {
  # Use na.locf from zoo package to carry forward last observation
  library(zoo)
  x <- na.locf(x, na.rm = FALSE) # Set na.rm = FALSE to keep trailing NAs if any
  return(x)
}

data$Home <- ifelse(data$Home == "", NA, data$Home)
data$Work <- ifelse(data$Work == "", NA, data$Work)

data$Home <- fill_down(data$Home)
data$Work <- fill_down(data$Work)

# Remove the non applicable mode

filtered_data <- data %>%
  filter(Mode != "Not applicable")

# Merge all the Greater Melbourne locations

merge_locations <- c(
  "Melbourne - North East", 
  "Melbourne - South East", 
  "Melbourne - Inner South", 
  "Melbourne - Inner East", 
  "Melbourne - Outer East"
)

filtered_data2  <- filtered_data %>%
  mutate(Home = ifelse(Home %in% merge_locations, "Melbourne - All South and East", Home))

#Check the number of trips for each home location
home_trips <- filtered_data2 %>%
  group_by(Home) %>%
  summarize(Total_Trips = sum(Trip, na.rm = TRUE))

# Filter home locations with more than 100 trips
home_locations_over_100 <- home_trips %>%
  filter(Total_Trips > 100) %>%
  pull(Home)

filtered_data3 <- filtered_data2 %>%
  filter(Home %in% home_locations_over_100)

# Remove 'Bendigo' location to only focus on outside trips
filtered_data3 <- filtered_data3 %>%
  filter(Home != "Bendigo")

# Check the remaining trips
home_trips2 <- filtered_data3 %>%
  group_by(Home) %>%
  summarize(Total_Trips = sum(Trip, na.rm = TRUE))


## Creating the Dummy Trip Plans ------------------------------------------------------------------------------------------------------------

# Simplify mode names 
simplified_data <- filtered_data3 %>%
  mutate(Mode = case_when(
    Mode == "Public Transport" ~ "PT",
    Mode == "Vehicle" ~ "Car",
    Mode == "Active Transport" ~ "Bike",
    Mode == "Worked at home or Did not go to work" ~ "WFH",
    Mode == "Mode not stated" ~ "Car",
    Mode == "Other Mode" ~ "Car"
  )) %>%
  uncount(Trip)

# making the dummies and removing WFH

dummy_activities <- simplified_data %>%
  filter(Mode != "WFH") %>%
  mutate(
    Trip_ID = row_number(),
    Trip_Start_Time = 540
  ) %>%
  select(Trip_ID, Home, Work, Mode, Trip_Start_Time)

#count the trips for each mode

mode_counts <- dummy_activities %>%
  group_by(Mode) %>%
  summarize(Number_of_Trips = n())


# Write the dummy plans to a CSV file ---------------------------------
write_csv(dummy_activities, "../dataJTW/Dummy_Activities2.csv")