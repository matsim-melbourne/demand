# File to create the csv plan for dummy trips
# Author: Sapan Tiwari
# 07 June 2024


# This script uses the dummyPlan.csv created and gives an xml file containing the plans.

library(dplyr)
library(readr)
library(xml2)

writePlanAsMATSimXML <- function(outxml, writeInterval) {
  options(scipen=999)
  
  # Read in the plans
  plans <- read_csv("../dataJTW/Dummy_Activities_with_Coord.csv")
  
  # Write XML header
  cat('<?xml version="1.0" encoding="utf-8"?>\n', file = outxml)
  cat('<!DOCTYPE population SYSTEM "http://www.matsim.org/files/dtd/population_v6.dtd">\n', file = outxml, append = TRUE)
  cat('<population>\n', file = outxml, append = TRUE)
  
  # Initialize variables
  popnWriteBuffer <- ""
  processed <- 0
  i <- 1
  
  # Process each row to create XML structure
  while(i <= nrow(plans)) {
    current_plan <- plans[i,]
    
    # Start a new person element
    processed <- processed + 1
    person_id <- paste0('person_dummy_', processed)
    popnWriteBuffer <- paste0(popnWriteBuffer, '<person id="', person_id, '">\n')
    popnWriteBuffer <- paste0(popnWriteBuffer, '  <plan selected="yes">\n')
    
    # Add the activity and leg
    popnWriteBuffer <- paste0(popnWriteBuffer, '    <activity type="home" x="', current_plan$x_orig, '" y="', current_plan$y_orig, '" end_time="' , current_plan$Trip_Start_Time, '"/>\n')
    popnWriteBuffer <- paste0(popnWriteBuffer, '    <leg mode="', current_plan$Mode, '"/>\n')
    popnWriteBuffer <- paste0(popnWriteBuffer, '    <activity type="work" x="', current_plan$x_dest, '" y="', current_plan$y_dest, '"/>\n')
    
    # Close the person element
    popnWriteBuffer <- paste0(popnWriteBuffer, '  </plan>\n')
    popnWriteBuffer <- paste0(popnWriteBuffer, '</person>\n')
    
    # Write buffer to file at intervals
    if (processed %% writeInterval == 0 || i == nrow(plans)) {
      cat(popnWriteBuffer, file = outxml, append = TRUE)
      popnWriteBuffer <- "" # Clear buffer
    }
    
    i <- i + 1
  }
  
  # Write XML footer
  cat('</population>\n', file = outxml, append = TRUE)
  
  cat(paste0('Wrote ', processed, ' plans to ', outxml, '\n'))
}

# print the xml plans
writePlanAsMATSimXML('../dataJTW/Dummy_Activities.xml', 100)
