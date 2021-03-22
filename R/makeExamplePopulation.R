makeExamplePopulation<-function(samplePercent, numPlans, do.steps=c(T,T,T,T,T,T,T,T)) {
  # samplePercent:
  #   percent of the 2016 census-based Melbourne synthetic population to sample
  # numPlans:
  #   number of 2012-2018 VISTA-like trip chains (plans) to generate
  # do.steps:
  #   can be used to disable some plan steps such as when re-running the process 
  #   and we want to resume from where the previous run stopped/failed.
  
 
  # set any global options
  # see https://www.tidyverse.org/blog/2020/05/dplyr-1-0-0-last-minute-additions/
  options(dplyr.summarise.inform = FALSE)

  # load general purpose utility functions
  source("util.R")
  
  
  dir.create('../output', showWarnings = FALSE, recursive=TRUE)
  sink('../output/makeExamplePopulation.log', append=FALSE, split=TRUE) # sink to both console and log file

  tryCatch({
  
    # Step 0: check input data and create the output dirs
    source("checksum.R", local=TRUE)
    if(!checksum()) {
      cat("Input data files did not match expectation so will stop here.\n")
      do.steps <- c(F,F,F,F,F,F,F,F) 
    }
    
    outdirs <- c(
      '../output/1.setup',
      '../output/2.sample',
      '../output/3.match',
      '../output/4.plan',
      '../output/5.locate',
      '../output/6.place',
      '../output/7.time',
      '../output/8.xml'
    )
    for (outdir in outdirs) {
      dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
    }
    
    # Step 1: pre-process VISTA and locations data
    if(do.steps[1]) {
      source("group.R", local=TRUE)
      source("vista.R", local=TRUE)
      source('setup.R', local=TRUE); 
      source('locations.R', local=TRUE);
      make_groups(
        '../data/VISTA_12_18_CSV.zip.dir/P_VISTA1218_V1.csv',
        '../data/VISTA_12_18_CSV.zip.dir/T_VISTA1218_V1.csv',
        '../data/vistaCohorts.csv.gz',
        '../output/1.setup', 
        '../output/1.setup/vista_2012_18_extracted_persons_weekday.csv.gz',
        'vista_2012_18_extracted_group_weekday_',
        'vista_2012_18_extracted_trips_weekday_',
        NULL, NULL, NULL # ignoring weekends
      )
      demand_setup_groups(
        getGroupIds('../data/vistaCohorts.csv.gz'),
        '../output/1.setup', 
        'vista_2012_18_extracted_trips_weekday_',
        'vista_2012_18_extracted_activities_weekday_',
        'vista_2012_18_extracted_activities_weekday_time_bins_',
        'vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins_',
        NULL, NULL # ignoring weekends
      )
      locations_setup(
        '../output/1.setup', 
        '../data/distanceMatrix.rds', 
        '../data/distanceMatrixIndex.csv', 
        '../data/SA1attributed.csv.gz', 
        '../data/SA1centroids.csv.gz', 
        '../data/addresses.csv.gz',
        '../data/expectedDistances.rds'
      )
    }
    
    # Step 2: get the required sample of Melbourne's synthetic population
    if(do.steps[2]) {
      source('sample.R', local=TRUE); 
      sampleMelbourne2016Population(
        '../data', 
        samplePercent, 
        '../output/2.sample/sample.csv.gz'
      )
    }

    # Step 3: match the census persons to VISTA groups
    if(do.steps[3]) {
      source('match.R', local=TRUE); 
      matchPersons(
        getGroups('../data/vistaCohorts.csv.gz'),
        '../output/2.sample/sample.csv.gz', 
        '../output/3.match/match_'
      )
    }

    # Step 4: generate the VISTA-like trip chains
    if(do.steps[4]) {
      source('plan.R', local=TRUE);
      generatePlansByGroup(
        getGroupIds('../data/vistaCohorts.csv.gz'),
        '../output/3.match/match_',
        '../output/1.setup/vista_2012_18_extracted_activities_weekday_time_bins_',
        '../output/1.setup/vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins_',
        3:50, # specifies that columns 3-50 correspond to 48 time bins, i.e., 30-mins each
        '../output/4.plan/',
        500 # write to file every 1000 plans
      )
      combinePlans(getGroupIds('../data/vistaCohorts.csv.gz'),
                   '../output/4.plan/',
                   '../output/4.plan/plan.csv'
      )
      writePlan2Agent2GroupMap(getGroupIds('../data/vistaCohorts.csv.gz'),
                         '../output/3.match/match_',
                         '../output/4.plan/plan.csv',
                         '../output/4.plan/plan2agent2group.csv'
      )
    }
    
    if(do.steps[5]) {
      source('locations.R')
      loadLocationsData(
        '../output/1.setup/locDistanceMatrix.rds',
        '../output/1.setup/locDistanceMatrixIndex.rds',
        '../output/1.setup/locSa1Aattributed.rds',
        '../output/1.setup/locSa1Centroids.rds',
        '../output/1.setup/locAddresses.rds',
        '../output/1.setup/expectedDistances.rds'
      )
      source('locateParallel.R')
      # uses doParallel which must be run from the project root 
      # to ensure packrat libraries are sourced correctly by the workers.
      # See https://stackoverflow.com/a/36901524.
      wd<-getwd()
      setwd("..")
      locatePlans(
        './output/2.sample/sample.csv.gz',
        './output/4.plan/plan.csv',
        './output/4.plan/plan2agent2group.csv',
        './output/5.locate',
        './output/5.locate/plan.csv'
      )
      setwd(wd)
      planToSpatial(
        read.csv("../output/5.locate/plan.csv"),
        '../output/5.locate/plan.sqlite'
      )
      source('locateVISTA.R', local=TRUE);
      analyseLocate('../output/5.locate')
    }
    if(do.steps[6]) {
      if(!do.steps[5]) { # if not already loaded in the last step
        source("locations.R")
        loadLocationsData(
          '../output/1.setup/locDistanceMatrix.rds', 
          '../output/1.setup/locDistanceMatrixIndex.rds',
          '../output/1.setup/locSa1Aattributed.rds', 
          '../output/1.setup/locSa1Centroids.rds', 
          '../output/1.setup/locAddresses.rds',
          '../output/1.setup/expectedDistances.rds'
        )
      }
      source('placeParallel.R')
      # uses doParallel which must be run from the project root 
      # to ensure packrat libraries are sourcecorrectly by the workers.
      # See https://stackoverflow.com/a/36901524.
      wd<-getwd()
      setwd("..")
      assignLocationsToActivities(
        './output/5.locate/plan.csv', 
        './output/6.place'
      )
      setwd(wd)
      placeToSpatial(
        read.csv("../output/6.place/plan.csv"),
        '../output/6.place/plan.sqlite'
      )
    }
    if(do.steps[7]) {
      source('time.R', local=TRUE)
      # uses doParallel which must be run from the project root 
      # to ensure packrat libraries are sourcecorrectly by the workers.
      # See https://stackoverflow.com/a/36901524.
      wd<-getwd()
      setwd("..")
      assignTimesToActivities(
        './output/6.place/plan.csv', 
        30, # bin size in minutes 
        './output/7.time', 
        './output/7.time/plan.csv', 
        500 # write to file every so many plans
      )
      setwd(wd)
      
    }
    if(do.steps[8]) {
      source('xml.R', local=TRUE)
      writePlanAsMATSimXML(
        '../output/7.time/plan.csv', 
        '../output/8.xml/plan.xml', 
        500 # write to file in blocks of this size
      )
    }
  },
  finally = {
    sink() # end the diversion
  })
}

runexample<-function() {
  samplePercent<- 0.1 # use 0.1% sample of the census-like synthetic population (<5k persons)
  do.steps <- c(T,T,T,T,T,T,T,T) # which algorithm steps to run
  makeExamplePopulation(samplePercent, numPlans, do.steps) 
}
