suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# load general purpose utility functions
source("util.R")

outputDir="../dataWorkLocation/dataOutput"

dir.create(paste0('../',outputDir), showWarnings = FALSE, recursive=TRUE)



# collates the population into a single table, adding in household ids
source('collatePopulation.R', local=TRUE); 
collateMelbourne2016Population(
  outputDir
)

# using census data determines if agents should be employed or not
source('determineEmployed.R', local=TRUE); 
determineEmployed(
  outputDir
) 

# gets the census and other datasets ready for assigning work locations.
source('cleanWorkData.R', local=TRUE); 
prepWorkData(
  outputDir
)

# assigns a work SA1 location for those that work.
# takes about 2 days to run for the entire population
#might want to run this one interactively
source('assignWorkLocations.R', local=TRUE); 
assignWorkLocations(
  outputDir
)

# add in extra census data

# add in home and work coordinates based on sa1 region

# reformat to match Manchester data







