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
  
  
  dir.create('../outputSmall', showWarnings = FALSE, recursive=TRUE)
  sink('../outputSmall/makeExamplePopulation.log', append=FALSE, split=TRUE) # sink to both console and log file

  tryCatch({
  
    # Step 0: check input data and create the output dirs
    source("checksum.R", local=TRUE)
    if(!checksum()) {
      cat("Input data files did not match expectation so will stop here.\n")
      do.steps <- c(F,F,F,F,F,F,F,F) 
    }
    
    outdirs <- c(
      '../outputSmall/1.setup',
      '../outputSmall/2.sample',
      '../outputSmall/3.match',
      '../outputSmall/4.plan',
      '../outputSmall/5.locate',
      '../outputSmall/6.place',
      '../outputSmall/7.time',
      '../outputSmall/8.xml'
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
        '../outputSmall/1.setup', 
        '../outputSmall/1.setup/vista_2012_18_extracted_persons_weekday.csv.gz',
        'vista_2012_18_extracted_group_weekday_',
        'vista_2012_18_extracted_trips_weekday_',
        NULL, NULL, NULL # ignoring weekends
      )
      demand_setup_groups(
        getGroupIds('../data/vistaCohorts.csv.gz'),
        '../outputSmall/1.setup', 
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
        '../data/expectedDistances.rds',
        plansFile="../../small-region/filteredRegions.csv"
      )
    }
    
    # Step 2: get the required sample of Melbourne's synthetic population
    if(do.steps[2]) {
      source('sample.R', local=TRUE); 
      sampleMelbourne2016Population(
        '../data', 
        samplePercent, 
        '../outputSmall/2.sample/sample.csv.gz',
        plansFile="../../small-region/filteredRegions.csv"
      )
    }

    # Step 3: generate the VISTA-like trip chains
    if(do.steps[3]) {
      source('sample.R', local=TRUE); 
      source('plan.R', local=TRUE);
      numPlans<-ceiling((samplePercent/100)* countMelbourne2016Population('../data',plansFile="../../small-region/filteredRegions.csv")) + 100 # generate a few extra
      generatePlans(
        numPlans, 
        '../outputSmall/1.setup/vista_2012_18_extracted_activities_weekday_time_bins.csv.gz', 
        '../outputSmall/1.setup/vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins.csv.gz', 
        3:50, # specifies that columns 3-50 correspond to 48 time bins, i.e., 30-mins each
        '../outputSmall/3.plan', 
        500 # write to file every 1000 plans
      )
    }
    
    if(do.steps[4]) {
      source('match.R', local=TRUE); 
      matchPersons(
        '../outputSmall/2.sample/sample.csv.gz', 
        '../outputSmall/3.plan/plan.csv', 
        '../outputSmall/4.match/match.csv.gz'
      )
    }
    if(do.steps[5]) {
      source('locations.R')
      loadLocationsData(
        '../outputSmall/1.setup/locDistanceMatrix.rds',
        '../outputSmall/1.setup/locDistanceMatrixIndex.rds',
        '../outputSmall/1.setup/locSa1Aattributed.rds',
        '../outputSmall/1.setup/locSa1Centroids.rds',
        '../outputSmall/1.setup/locAddresses.rds'
      )
      source('locateParallel.R')
      # uses doParallel which must be run from the project root 
      # to ensure packrat libraries are sourced correctly by the workers.
      # See https://stackoverflow.com/a/36901524.
      wd<-getwd()
      setwd("..")
      locatePlans(
        './outputSmall/2.sample/sample.csv.gz',
        './outputSmall/3.plan/plan.csv',
        './outputSmall/4.match/match.csv.gz',
        './outputSmall/5.locate',
        './outputSmall/5.locate/plan.csv'
      )
      setwd(wd) 
      planToSpatial(
        read.csv("../outputSmall/5.locate/plan.csv"),
        '../outputSmall/5.locate/plan.sqlite'
      )
    }
    if(do.steps[6]) {
      if(!do.steps[5]) { # if not already loaded in the last step
        source("locations.R")
        loadLocationsData(
          '../outputSmall/1.setup/locDistanceMatrix.rds', 
          '../outputSmall/1.setup/locDistanceMatrixIndex.rds',
          '../outputSmall/1.setup/locSa1Aattributed.rds', 
          '../outputSmall/1.setup/locSa1Centroids.rds', 
          '../outputSmall/1.setup/locAddresses.rds'
        )
      }
      source('placeParallel.R')
      # uses doParallel which must be run from the project root 
      # to ensure packrat libraries are sourcecorrectly by the workers.
      # See https://stackoverflow.com/a/36901524.
      wd<-getwd()
      setwd("..")
      assignLocationsToActivities(
        './outputSmall/5.locate/plan.csv', 
        './outputSmall/6.place'
      )
      setwd(wd)
      placeToSpatial(
        read.csv("../outputSmall/6.place/plan.csv"),
        '../outputSmall/6.place/plan.sqlite'
      )
    }
    if(do.steps[7]) {
      source('time.R', local=TRUE)
      # uses doParallel which must be run from the project root 
      # to ensure packrat libraries are sourced correctly by the workers.
      # See https://stackoverflow.com/a/36901524.
      wd<-getwd()
      setwd("..")
      assignTimesToActivities(
        './outputSmall/6.place/plan.csv', 
        30, # bin size in minutes 
        './outputSmall/7.time', 
        './outputSmall/7.time/plan.csv', 
        500 # write to file every so many plans
      )
      setwd(wd)
      
    }
    if(do.steps[8]) {
      source('xml.R', local=TRUE)
      writePlanAsMATSimXML(
        '../outputSmall/7.time/plan.csv', 
        '../outputSmall/8.xml/plan.xml', 
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
