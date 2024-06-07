# File to create the csv plan for dummy trips with coordinates
# Author: Sapan Tiwari
# 7 June 2024

#  The input file here is the table downloaded from ABS, which contains the home locations for entire victoria (SA2 level), work location CoGB (SA2) and mode of travel

# IN the end it produces a dummyPlan.csv file, which can be used to get the dummy plans in csv with the coordinates. It uses start time distribution from readRuralWorkTripsVISTA.R, which generates

# distributions based on the work trips from VISTA, which goes to other rural areas.

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Read the table extraced from ABS

data <- read_csv("../dataJTW/SA2_Codes_home_to_SA2_CodesPOW_Modes.csv")

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

# Remove the non applicable mode and rows with 0 trips

filtered_data <- data %>%
  filter(Mode != "Not applicable", Trip > 0)


#Check the number of trips for each home location
home_trips <- filtered_data %>%
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

# Define the sampling function (chooses randomly)

sample_trips <- function(data, sample_size) {
  sampled_data <- data %>% sample_frac(sample_size)
  return(sampled_data)
}

#Publish 10% population
sampled_data <- sample_trips(simplified_data, 0.1)

# making the dummies and removing WFH

dummy_activities <- simplified_data %>%
  filter(Mode != "WFH") %>%
  mutate(
    Trip_ID = row_number()) 


# load the probability distribution

probability_data <- read_csv("../data1216/output/probability_data.csv")

start_time_with_distribution <- function(probability_data) {
  sample(probability_data$BIN, 1, prob = probability_data$PROB_STARTIME)
}

dummy_activities <- dummy_activities %>%
  rowwise() %>%
  mutate(Trip_Start_Time = start_time_with_distribution(probability_data)) %>%
  ungroup() %>%
  select(Trip_ID, Home, Work, Mode, Trip_Start_Time)

#count the trips for each mode

mode_counts <- dummy_activities %>%
  group_by(Mode) %>%
  summarize(Number_of_Trips = n())

# plot the travel time
ggplot(dummy_activities, aes(x = Trip_Start_Time)) +
  geom_histogram(binwidth = 15, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Trip Start Times",
       x = "Trip Start Time (in minutes from midnight)",
       y = "Count of Trips")

# add the coordinates (centroids)-------------------------------------------------------------------------------

# Read the coordinates data file for SA2 (extracted from ABS)
centroidSA2 <- read_csv("../dataJTW//centroidSA2.csv")

# Convert Home and Work columns to character type in dummy_activities2
dummy_activities <- dummy_activities %>%
  mutate(Home = as.character(Home),
         Work = as.character(Work))

# Convert SA2_CODE21 to character type in centroidSA2
centroidSA2 <- centroidSA2 %>%
  mutate(SA2_CODE21 = as.character(SA2_CODE21))

# Rename columns in centroidSA2 for easier merging

centroidSA2_home <- centroidSA2 %>%
  rename(Home = SA2_CODE21, x_orig = x_coord, y_orig = y_coord) %>%
  select(Home, x_orig, y_orig)

centroidSA2_work <- centroidSA2 %>%
  rename(Work = SA2_CODE21, x_dest = x_coord, y_dest = y_coord) %>%
  select(Work, x_dest, y_dest)

# Merge coordinates for Home and work

dummy_activities <- dummy_activities %>%
  left_join(centroidSA2_home, by = "Home")

dummy_activities <- dummy_activities %>%
  left_join(centroidSA2_work, by = "Work")

# Filter out rows with NA coordinates

dummy_activities <- dummy_activities %>%
  filter(!is.na(x_orig) & !is.na(y_orig) & !is.na(x_dest) & !is.na(y_dest))


# Write the dummy plans to a CSV file ---------------------------------
write_csv(dummy_activities, "../dataJTW/Dummy_Activities_with_Coord.csv")