# Finding the rural trips based on the existing VISTA 12-16 trip dataset
# Author: Sapan Tiwari
# 27 May 2024

rm(list = ls())


library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)

data <- read_csv("../data1216/T_VISTA12_16_SA1_V1.csv")

relevant_data <- data %>% select(
  TRIPID, PERSID, HHID, STARTIME, ARRTIME, ORIGSA1, ORIGSA3, ORIGLGA,
  ORIGPLACE1, ORIGPLACE2, ORIGPURP1, ORIGPURP2, DESTPLACE1, DESTPLACE2,
  DESTPURP1, DESTPURP2, DESTSA1, DESTSA3, DESTLGA, LINKMODE, Mode1
)

work_related_data <- relevant_data %>% filter(ORIGPURP1 == "Work Related" | DESTPURP1 == "Work Related")


# Define the list of Victorian region names (https://www.rdv.vic.gov.au/victorias-regions#region-listing)

victorian_regions <- c("Alpine", "Ararat", "Ballarat", "Bass Coast", "Baw Baw", "Benalla", "Buloke", 
                       "Campaspe", "Central Goldfields", "Colac Otway", "Corangamite", "East Gippsland", 
                       "Gannawarra", "Glenelg", "Golden Plains", "Greater Bendigo", "Greater Geelong", 
                       "Greater Shepparton", "Hepburn", "Hindmarsh", "Horsham", "Indigo", "Latrobe", 
                       "Loddon", "Macedon Ranges", "Mansfield", "Mildura", "Mitchell", "Moira", 
                       "Moorabool", "Mount Alexander", "Moyne", "Murrindindi", "Northern Grampians", 
                       "Pyrenees", "Queenscliffe", "South Gippsland", "Southern Grampians", "Strathbogie", 
                       "Surf Coast", "Swan Hill", "Towong", "Wangaratta", "Warrnambool", "Wellington", 
                       "West Wimmera", "Wodonga", "Yarriambiack")

# Extract trips with origins and destinations in these rural locations
rural_trip_data <- work_related_data %>%
  filter(
    str_detect(ORIGLGA, paste(victorian_regions, collapse = "|")) |
      str_detect(DESTLGA, paste(victorian_regions, collapse = "|"))
  )

unique_values_ORIGLGA <- unique(rural_trip_data$ORIGLGA)

# Convert STARTIME and ARRITIME to numeric
travel_time_data <- rural_trip_data %>%
  select(TRIPID, STARTIME, ARRTIME) %>%
  mutate(
    STARTIME = as.numeric(STARTIME),
    ARRTIME = as.numeric(ARRTIME)
  )

#  function to bin the times (15 minutes bins)
bin_time <- function(time) {
  time_wrapped <- time %% 1440
  cut(time_wrapped, breaks = seq(0, 1440, by = 15), right = FALSE, labels = FALSE) * 15
}

# Creating a complete sequence of bins
all_bins <- data.frame(BIN = seq(15, 1440, by = 15))

# converting trips times to bins
travel_time_data <- travel_time_data %>%
  mutate(
    STARTIME_BIN = bin_time(STARTIME),
    ARRTIME_BIN = bin_time(ARRTIME)
  )

# Counting the number of trips in each bin
startime_counts <- travel_time_data %>%
  group_by(STARTIME_BIN) %>%
  summarise(NUM_TRIPS_STARTIME = n())

arrtime_counts <- travel_time_data %>%
  group_by(ARRTIME_BIN) %>%
  summarise(NUM_TRIPS_ARRTIME = n())

# Merging with the counts to include all bins
startime_counts <- all_bins %>%
  left_join(startime_counts, by = c("BIN" = "STARTIME_BIN")) %>%
  mutate(NUM_TRIPS_STARTIME = ifelse(is.na(NUM_TRIPS_STARTIME), 0, NUM_TRIPS_STARTIME))

arrtime_counts <- all_bins %>%
  left_join(arrtime_counts, by = c("BIN" = "ARRTIME_BIN")) %>%
  mutate(NUM_TRIPS_ARRTIME = ifelse(is.na(NUM_TRIPS_ARRTIME), 0, NUM_TRIPS_ARRTIME))

# Merging all the data in one data frame for analysis
trip_counts <- full_join(startime_counts, arrtime_counts, by = "BIN")

# total number of trips for STARTIME and ARRTIME
total_startime_trips <- sum(trip_counts$NUM_TRIPS_STARTIME)
total_arrtime_trips <- sum(trip_counts$NUM_TRIPS_ARRTIME)

# Calculating the probabilities (Which is the number of trips in that bin over total number of trips)
trip_counts <- trip_counts %>%
  mutate(
    PROB_STARTIME = NUM_TRIPS_STARTIME / total_startime_trips,
    PROB_ARRTIME = NUM_TRIPS_ARRTIME / total_arrtime_trips
  )


probability_data <- trip_counts %>%
  select(BIN, PROB_STARTIME, PROB_ARRTIME)


# print probability to a csv
write_csv(probability_data, "../data1216/output/probability_data.csv")

print(head(probability_data))


# Plot the arrival and departure times over the day -----------------------------

trip_counts_long <- trip_counts %>%
  pivot_longer(cols = c(NUM_TRIPS_STARTIME, NUM_TRIPS_ARRTIME),
               names_to = "Time_Type", values_to = "Num_Trips")

# Bar plot
ggplot(trip_counts_long, aes(x = BIN, y = Num_Trips, fill = Time_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = seq(0, 1440, by = 60)) +
  labs(title = "Number of Trips by Time Bin",
       x = "Time Bin (minutes)",
       y = "Number of Trips",
       fill = "Time Type") +
  theme_minimal()


# Line plot
ggplot(trip_counts_long, aes(x = BIN, y = Num_Trips, fill = Time_Type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 1440, by = 60)) +
  labs(title = "Number of Trips by Time Bin",
       x = "Time Bin (minutes)",
       y = "Number of Trips",
       color = "Time Type") +
  theme_minimal()








