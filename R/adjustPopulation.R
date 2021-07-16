# function to adjust population around given locations
# by adding specified number of people

adjustPopulation <- function(dataDir = "../data",
                             regionLayer = "sa2_2016_aust",
                             locationFile = "srl_stg1.sqlite",  # must be in dataDir
                             locationLayer = "stations",
                             bufferDistance = 1000,  # distance in metres
                             newPeople = 5000,  # number of additional people per location
                             outputDir = "../output"
                             ) {

  # load populations and regions
  # --------------------------------------
  # table of SA2 names and codes
  SA2Table <- read.csv(gzfile(paste0(dataDir, "/sa1_2016_aust.csv.gz"))) %>%
    distinct(SA2_MAINCODE_2016, SA2_NAME_2016)
  
  # dataframe to hold existing and new populations, by SA2
  populations <- data.frame(file = list.files(path = dataDir, 
                                            pattern = "\\persons.csv.gz$", 
                                            recursive = TRUE, 
                                            full.names = TRUE), 
                           stringsAsFactors=FALSE) %>%
    mutate(SA2_NAME = stringr::str_extract(string = file, pattern = "(?<=SA2/).*(?=/population)")) %>%
    left_join(., SA2Table, by = c("SA2_NAME" = "SA2_NAME_2016"))
  
  echo(paste0("Reading census 2016 population for Melbourne's ", nrow(populations), " SA2 areas (can take a while)\n"))
  for (i in 1:nrow(populations)) {
    printProgress(i, ".")
    populations[i, "census_pop"] <- as.numeric(nrow(read.csv(gzfile(populations[i, "file"])))) - 1
  }
  cat('\n')
  
  populations <- populations %>%
    # mutate(adjusted_pop = census_pop) %>%
    dplyr::select(SA2_NAME, SA2_MAINCODE_2016, census_pop) #, adjusted_pop)
  
  # regions (with area and population)
  regions <- st_read(paste0(dataDir, "/absRegionsReprojected.sqlite"),
                     layer = regionLayer) %>%
    mutate(orig_area = as.numeric(st_area(.))) %>%
    left_join(populations, by = c("sa2_maincode_2016" = "SA2_MAINCODE_2016")) %>%
    # replace NAs with 0 (Essendon and Moorabbin airports; Melbourne docks)
    mutate(census_pop = ifelse(is.na(census_pop), 0, census_pop))
  
  
  # buffer locations, intersect with SA2s and calculate population of each intersection area
  # ---------------------------------------------------------------------------
  locations <- st_read(paste0(dataDir, "/", locationFile),
                       layer = locationLayer) %>%
    st_buffer(., bufferDistance) %>%
    mutate(location_id = row_number()) %>%
    st_intersection(., regions) %>%
    mutate(isec_area = as.numeric(st_area(.)),
           isec_pop = (isec_area/orig_area) * census_pop) %>%
    st_drop_geometry()
  
  
  # for each location, apportion the additional people based on existing population
  # ---------------------------------------------------------------------------
  # sum of intersection populations for each location
  location_pops <- locations %>%
    group_by(location_id) %>%
    summarise(location_pop = sum(isec_pop))
  
  # apportioned additional population for each intersection area 
  for (i in 1: nrow(locations)) {
    location_pop <- location_pops[location_pops$location_id == locations[i, "location_id"], "location_pop"]
    locations[i, "additional_pop"] <- (locations[i, "isec_pop"] / location_pop) * newPeople
  }
  
  # sum of additional population for each SA2
  region_pops <- locations %>%
    group_by(sa2_maincode_2016) %>%
    summarise(additional_pop = sum(additional_pop))
  
  # add to populations
  populations <- populations %>%
    left_join(., region_pops, by = c("SA2_MAINCODE_2016" = "sa2_maincode_2016")) %>%
    mutate(additional_pop = ifelse(is.na(additional_pop), 0, additional_pop),
           adjusted_pop = census_pop + additional_pop)
}
