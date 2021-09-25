library(sf)

samplePrecinctPopulation <- function(dataDir,
                                     baseOutputDir,
                                     precinctOutputDir,
                                     samplePercent,
                                     locationFile,
                                     locationLayer,
                                     bufferDistance,
                                     newPeople
                                     ) {
  
  # setting up: base sample and SA1 populations
  #----------------------------------------------------------------------------
  # read in base sample (used to ensure new precinct people don't appear in base sample)
  baseSample <- read.csv(gzfile(paste0('../', baseOutputDir, "/2.sample/sample.csv.gz")))
  
  # make table of SA2 names and codes
  SA2Table <- read.csv(gzfile(paste0(dataDir, "/sa1_2016_aust.csv.gz"))) %>%
    distinct(SA2_MAINCODE_2016, SA2_NAME_2016)
  
  # make table of SA2 population files, with names and codes
  SA2s <- data.frame(file = list.files(path = dataDir, 
                                       pattern = "\\persons.csv.gz$", 
                                       recursive = TRUE, 
                                       full.names = TRUE), 
                     stringsAsFactors=FALSE) %>%
    mutate(SA2_NAME = stringr::str_extract(string = file, pattern = "(?<=SA2/).*(?=/population)")) %>%
    left_join(., SA2Table, by = c("SA2_NAME" = "SA2_NAME_2016"))
  
  # determine SA1 populations, from SA2 population files
  SA1.pops <- NULL
  
  echo(paste0("Extracting SA1 populations figures from ", nrow(SA2s), " Melbourne SA2 population files\n"))
  for(i in 1:nrow(SA2s)) {
    printProgress(i,".")
    
    SA1.pop <- read.csv(gzfile(df$file[i])) %>%
      group_by(SA1_7DIGCODE) %>%
      summarise(census_pop = n()) 
    
    SA1.pops <- rbind(SA1.pops, SA1.pop)
  }
  cat("\n")
  echo(paste0("Extracted SA1 population figures for  ", nrow(SA1.pops), " SA1s\n"))
  
  
  # calculating current census population of SA1s in precincts, with population 
  # for partial SA1 apportioned on basis of area 
  #----------------------------------------------------------------------------
  # read in SA1 locations, calculate area and join to population figures
  SA1s <- st_read(paste0(dataDir, "/absRegionsReprojected.sqlite"),
                  layer = "sa1_2016_aust") %>%
    mutate(orig_area = as.numeric(st_area(.))) %>%
    left_join(SA1.pops, by = c("sa1_7digitcode_2016" = "SA1_7DIGCODE")) %>%
    # replace NAs with 0 (these are SA1s with zero population, eg parks)
    mutate(census_pop = ifelse(is.na(census_pop), 0, census_pop))
  
  # read in precinct location points and buffer to create precinct areas
  precinct.locations <- st_read(paste0(dataDir, "/", locationFile),
                                layer = locationLayer) %>%
    st_buffer(., bufferDistance)
  
  # intersect precincts with SA1s and calculate population of each intersection area
  precincts <- precinct.locations %>%
    mutate(precinct_id = row_number()) %>%
    st_intersection(., SA1s) %>%
    mutate(isec_area = as.numeric(st_area(.)),
           isec_pop = (isec_area/orig_area) * census_pop) %>%
    st_drop_geometry()
  
  
  # for each precinct, apportion the additional people required in each SA1
  #----------------------------------------------------------------------------
  # sum of intersection populations for each precinct
  precinct.pops <- precincts %>%
    group_by(precinct_id) %>%
    summarise(precinct_pop = sum(isec_pop))
  
  # apportioned additional population for each intersection area (additional population
  # for precinct apportioned between intersection areas based on their current populations)
  for (i in 1: nrow(precincts)) {
    precinct.pop <- precinct.pops[precinct.pops$precinct_id == precincts[i, "precinct_id"], "precinct_pop"]
    precincts[i, "additional_pop"] <- (precincts[i, "isec_pop"] / precinct.pop) * newPeople
  }
  
  # sum of additional population for each SA1, joined to SA2 codes and names
  SA1.additional.pops <- precincts %>%
    group_by(sa1_maincode_2016) %>%
    # additional pop for each SA1 is the sum of the pops for its intersection areas 
    summarise(additional_pop = sum(additional_pop)) %>%
    # join SA2 codes and names
    left_join(., SA1s %>% dplyr::select(sa1_maincode_2016, sa1_7digitcode_2016, sa2_maincode_2016), 
              by = c("sa1_maincode_2016")) %>%
    left_join(., SA2s %>% dplyr::select(SA2_NAME, SA2_MAINCODE_2016),
              by = c("sa2_maincode_2016" = "SA2_MAINCODE_2016")) %>%
    dplyr::select(-GEOMETRY)
  
  
  # for each SA1, determine  sample required, based on samplePercent
  #----------------------------------------------------------------------------
  SA1.additional.pops <- SA1.additional.pops %>%
    mutate(sample = additional_pop * samplePercent / 100)
  
  # where using small sample size, too many SA1s may be rounded to zero
  # function to keep largest remainders, resulting in total remaining correct, from
  # https://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum
  round_preserve_sum <- function(x, digits = 0) {
    up <- 10 ^ digits
    x <- x * up
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y / up
  } 
  
  SA1.additional.pops$sample <- round_preserve_sum(SA1.additional.pops$sample)
  
  
  # obtain the sample, taking the required number from each SA1
  #----------------------------------------------------------------------------
  precinctSample <- NULL
  
  echo(paste0("Selecting a ", samplePercent, "% population sample of additional precinct residents from ", nrow(SA1.additional.pops), " SA1 areas (can take a while)\n"))
  for (i in 1:nrow(SA1.additional.pops)) {
    printProgress(i,".")
    
    # extract all people who live in the SA1 and were not in base sample
    file <- SA2s %>%
      filter(SA2_NAME == as.character(SA1.additional.pops[i, "SA2_NAME"])) %>%
      .$file
    candidates <- read.csv(gzfile(file)) %>%
      filter(SA1_7DIGCODE == as.character(SA1.additional.pops[i, "sa1_7digitcode_2016"])) %>%
      filter(!AgentId %in% baseSample$AgentId)
    
    # sample the relevant number of people from the candidate people
    sampleSet <- candidates[sample(nrow(candidates), 
                                   as.numeric(SA1.additional.pops[i, "sample"])),]
    
    # add SA1_MAINCODE (required for consistency with base sample)
    sampleSet <- sampleSet %>%
      mutate(SA1_MAINCODE_2016 = as.numeric(SA1.additional.pops[i, "sa1_maincode_2016"]))
    
    precinctSample <- rbind(precinctSample, sampleSet)
  }
  cat("\n")
  echo(paste0("Extracted a sample of ", nrow(precinctSample), " people\n"))
  
  # write output
  write.csv(precinctSample, file=gzfile(paste0('../',precinctOutputDir,'/2.sample/sample.csv.gz')), quote=TRUE, row.names = FALSE)
  
}