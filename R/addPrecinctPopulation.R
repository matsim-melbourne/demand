# Function to add an additional sample of people ('newPeople') from defined
# precincts to an existing sample population ('base')

# Uses pre-processed Vista and locations data from base folder '1.setup' and
# the  sample from base folder '2.sample' as inputs

# Writes outputs for the new people into folders '2.sample' to '8.xml' (matching
# base folder structure) and additional combined output into folder '9.combined'

addPrecinctPopulation <- function(dataDir = "../data",
                                  baseOutputDir = "output",
                                  precinctOutputDir = "output_precinct",
                                  samplePercent = 0.1,
                                  locationFile = "srl_stg1.sqlite",
                                  locationLayer = "stations",
                                  bufferDistance,
                                  newPeople,
                                  do.steps=c(T,T,T,T,T,T,T,T,T)) {
  
  # load general purpose utility functions
  source("util.R")

  # Step 0: create output directory structure
  # ---------------------------------------------------------------------------
  dir.create(paste0('../', precinctOutputDir), showWarnings = FALSE, recursive=TRUE)
  
  outdirs <- c(
    paste0('../',precinctOutputDir,'/2.sample'),
    paste0('../',precinctOutputDir,'/3.match'),
    paste0('../',precinctOutputDir,'/4.plan'),
    paste0('../',precinctOutputDir,'/5.locate'),
    paste0('../',precinctOutputDir,'/6.place'),
    paste0('../',precinctOutputDir,'/7.time'),
    paste0('../',precinctOutputDir,'/8.xml'),
    paste0('../',precinctOutputDir,'/9.combined')
  )
  
  for (outdir in outdirs) {
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  }
  
  
  # Step 1: pre-process VISTA and locations data
  # ---------------------------------------------------------------------------
  # Not required, as pre-processed data is loaded from Base Output Dir
  # when needed - see Steps 4 and 56
  
  
  # Step 2: get the required sample of Melbourne's synthetic population, for the precincts
  # ---------------------------------------------------------------------------
  if(do.steps[2]) {
    source('samplePrecinct.R', local = TRUE) 
    samplePrecinctPopulation(
      dataDir,
      baseOutputDir,
      precinctOutputDir,
      samplePercent,
      locationFile,
      locationLayer,
      bufferDistance,
      newPeople
    )
  }
  

  
  # Step 3: match the precinct census persons to VISTA groups
  # ---------------------------------------------------------------------------
  if(do.steps[3]) {
    source('match.R', local = TRUE) 
    matchPersons(
      getGroups('../data/vistaCohorts.csv.gz'),
      paste0('../',precinctOutputDir,'/2.sample/sample.csv.gz'), 
      paste0('../',precinctOutputDir,'/3.match/match_')
    )
  }
  
  
  # Step 4: generate the VISTA-like trip chains
  # ---------------------------------------------------------------------------
  if(do.steps[4]) {
    source('plan.R', local = TRUE)
    generatePlansByGroup(
      getGroupIds('../data/vistaCohorts.csv.gz'),
      paste0('../',precinctOutputDir,'/3.match/match_'),
      paste0('../',baseOutputDir,'/1.setup/vista_2012_18_extracted_activities_weekday_time_bins_'),
      paste0('../',baseOutputDir,'/1.setup/vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins_'),
      3:50, # specifies that columns 3-50 correspond to 48 time bins, i.e., 30-mins each
      paste0('../',precinctOutputDir,'/4.plan/'),
      500 # write to file every 1000 plans
    )
    combinePlans(getGroupIds('../data/vistaCohorts.csv.gz'),
                 paste0('../',precinctOutputDir,'/4.plan/'),
                 paste0('../',precinctOutputDir,'/4.plan/plan.csv')
    )
    writePlan2Agent2GroupMap(getGroupIds('../data/vistaCohorts.csv.gz'),
                             paste0('../',precinctOutputDir,'/3.match/match_'),
                             paste0('../',precinctOutputDir,'/4.plan/plan.csv'),
                             paste0('../',precinctOutputDir,'/4.plan/plan2agent2group.csv')
    )
  }
    
  
  # Step 5: locate plans
  # ---------------------------------------------------------------------------
  if(do.steps[5]) {
    source('locations.R')
    loadLocationsData(
      paste0('../',baseOutputDir,'/1.setup/locDistanceMatrix.rds'),
      paste0('../',baseOutputDir,'/1.setup/locDistanceMatrixIndex.rds'),
      paste0('../',baseOutputDir,'/1.setup/locSa1Aattributed.rds'),
      paste0('../',baseOutputDir,'/1.setup/locSa1Centroids.rds'),
      paste0('../',baseOutputDir,'/1.setup/locAddresses.rds'),
      paste0('../',baseOutputDir,'/1.setup/expectedDistances.rds'),
      paste0('../',baseOutputDir,'/1.setup/destinationProbabilitiesSA3.rds')
    )
    source('locateParallel.R')
    # uses doParallel which must be run from the project root 
    # to ensure packrat libraries are sourced correctly by the workers.
    # See https://stackoverflow.com/a/36901524.
    wd<-getwd()
    setwd("..")
    locatePlans(
      paste0('./',precinctOutputDir,'/2.sample/sample.csv.gz'),
      paste0('./',precinctOutputDir,'/4.plan/plan.csv'),
      paste0('./',precinctOutputDir,'/4.plan/plan2agent2group.csv'),
      paste0('./',precinctOutputDir,'/5.locate'),
      paste0('./',precinctOutputDir,'/5.locate/plan.csv')
    )  ## note - this step doesn't address Alan's comment about reading in base distributions
    setwd(wd)
    planToSpatial(
      read.csv(paste0('../',precinctOutputDir,'/5.locate/plan.csv')),
      paste0('../',precinctOutputDir,'/5.locate/plan.sqlite')
    )
    source('locateVISTA.R', local = TRUE)
    analyseLocate(paste0('../',precinctOutputDir,'/5.locate'))
  }

      
  # Step 6: add specific non-centroid locations within SA1
  # ---------------------------------------------------------------------------
  if(do.steps[6]) {
    if(!do.steps[5]) { # if not already loaded in the last step
      source("locations.R")
      loadLocationsData(
        paste0('../',baseOutputDir,'/1.setup/locDistanceMatrix.rds'),
        paste0('../',baseOutputDir,'/1.setup/locDistanceMatrixIndex.rds'),
        paste0('../',baseOutputDir,'/1.setup/locSa1Aattributed.rds'),
        paste0('../',baseOutputDir,'/1.setup/locSa1Centroids.rds'),
        paste0('../',baseOutputDir,'/1.setup/locAddresses.rds'),
        paste0('../',baseOutputDir,'/1.setup/expectedDistances.rds'),
        paste0('../',baseOutputDir,'/1.setup/destinationProbabilitiesSA3.rds')
      )
    }
    source('placeParallel.R')
    # uses doParallel which must be run from the project root 
    # to ensure packrat libraries are sourcecorrectly by the workers.
    # See https://stackoverflow.com/a/36901524.
    wd<-getwd()
    setwd("..")
    assignLocationsToActivities(
      paste0('./',precinctOutputDir,'/5.locate/plan.csv'), 
      paste0('./',precinctOutputDir,'/6.place')
    )
    setwd(wd)
    placeToSpatial(
      read.csv(paste0('../',precinctOutputDir,'/6.place/plan.csv')),
      paste0('../',precinctOutputDir,'/6.place/plan.sqlite')
    )
  }

      
  # Step 7: add times
  # ---------------------------------------------------------------------------
  if(do.steps[7]) {
    source('time.R', local = TRUE)
    # uses doParallel which must be run from the project root 
    # to ensure packrat libraries are sourcecorrectly by the workers.
    # See https://stackoverflow.com/a/36901524.
    wd<-getwd()
    setwd("..")
    assignTimesToActivities(
      paste0('./',precinctOutputDir,'/6.place/plan.csv'), 
      30, # bin size in minutes 
      paste0('./',precinctOutputDir,'/7.time'), 
      paste0('./',precinctOutputDir,'/7.time/plan.csv'), 
      500 # write to file every so many plans
    )
    setwd(wd)
  }

      
  # Step 8: write to XML
  # ---------------------------------------------------------------------------
  if(do.steps[8]) {
    source('xml.R', local = TRUE)
    writePlanAsMATSimXML(
      paste0('../',precinctOutputDir,'/7.time/plan.csv'), 
      paste0('../',precinctOutputDir,'/8.xml/plan.xml'), 
      500 # write to file in blocks of this size
    )
  }
  
    
  # Step 9: combine precinct with base and write to output
  # ---------------------------------------------------------------------------
  if(do.steps[9]) {
    basePlan <- read.csv(paste0('../',baseOutputDir,'/7.time/plan.csv'))
    # highestBasePlan <- max(basePlan$PlanId)
    
    precinctPlan <- read.csv(paste0('../',precinctOutputDir,'/7.time/plan.csv')) #%>%
      # make PlanIDs sequential from baseplan [commented out - PlanIDs not used in XML]
      # mutate(PlanId = PlanId + highestBasePlan)
    
    combinedPlan <- rbind(basePlan, precinctPlan)
    write.csv(combinedPlan, file = paste0('../',precinctOutputDir,'/9.combined/plan.csv'))
    
    source('xml.R', local = TRUE)
    writePlanAsMATSimXML(
      paste0('../',precinctOutputDir,'/9.combined/plan.csv'), 
      paste0('../',precinctOutputDir,'/9.combined/plan.xml'), 
      500 # write to file in blocks of this size
    )
  }
}


# running the function
setwd("R")

addPrecinctPopulation(
  dataDir = "../data",
  # baseOutputDir = "output",
  baseOutputDir = "output-Sep10-10pct",
  precinctOutputDir = "output_precinct",
  # samplePercent = 0.1,
  samplePercent = 10,
  locationFile = "srl_stg1.sqlite",  # must be in dataDir
  locationLayer = "stations",  # must be contained in locationFile
  bufferDistance = 800,  # distance in metres (radius of precinct area around location point)
  newPeople = 5000,  # number of additional people per precinct
  do.steps=c(T,T,T,T,T,T,T,T,T)
)

