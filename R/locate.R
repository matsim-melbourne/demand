assignActivityAreasAndTravelModes <-function(censuscsv, vistacsv, matchcsv, outdir, outcsv, writeInterval) {

  options(scipen=999) # disable scientific notation for more readable filenames with small sample sizes
  
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(stringi))
  suppressPackageStartupMessages(library(doParallel))

  # internal function to replace activity tags with location tags
  replaceActivityWithLocationTags<-function (tc) {
    # convert activity-based tags to location-based tags (from SA1_attributes.sqlite) being:
    # Home* -> home
    # Work -> work
    # Study -> education
    # Shop -> commercial
    # Personal -> commercial
    # Social/Recreational -> commercial,park
    # Pickup/Dropoff/Deliver -> work,education,commercial,park (but not home)
    # Other -> work,education,commercial,park (but not home)
    tc<-replace(tc, tc=="Home", "home")
    tc<-replace(tc, tc=="Home Morning", "home")
    tc<-replace(tc, tc=="Home Daytime", "home")
    tc<-replace(tc, tc=="Home Night", "home")
    tc<-replace(tc, tc=="Work", "work")
    tc<-replace(tc, tc=="Study", "education")
    tc<-replace(tc, tc=="Shop", "commercial")
    tc<-replace(tc, tc=="Personal", "commercial")
    # KISS: replace 'With Someone' with Other for now
    tc<-replace(tc, tc=="With Someone", "Other")
    # KISS: assuming Social/Recreational is equally likely to occur in commercial or park locations ; improve later on
    tc<-as.vector(sapply(tc, function(x) replace(x, x=="Social/Recreational", sample(c("commercial","park"), 1))))
    # KISS: assuming Pickup/Dropoff/Deliver is equally likely to occur in any location; improve later on
    tc<-as.vector(sapply(tc, function(x) replace(x, x=="Pickup/Dropoff/Deliver", sample(c("work","education","commercial","park"), 1))))
    # KISS: assuming Other is equally likely to occur in any location; improve later on; improve later on
    tc<-as.vector(sapply(tc, function(x) replace(x, x=="Other", sample(c("work","education","commercial","park"), 1))))
    return(tc)
  }
  
  # Read in the persons
  gz1<-gzfile(censuscsv, 'rt')
  echo(paste0('Loading ABS census-like persons from ', censuscsv, '\n'))
  persons<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)

  
  # Read in the plans
  gz1<-gzfile(vistacsv, 'rt')
  echo(paste0('Loading VISTA-like plans from ', vistacsv, '\n'))
  origplans<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)
  
  # Read in the matches
  gz1<-gzfile(matchcsv, 'rt')
  echo(paste0('Loading matched plans to persons from ', matchcsv, '\n'))
  matches<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)

  # set.seed(20200406) # for when we want to have the same LocationType each time
  plans<- origplans %>%
    # Remove all plans that are not matched
    filter(PlanId %in% matches$PlanId) %>% 
    # Assign matched PersonId (very fast since we assume row number equals Id number)
    mutate(AgentId = matches[as.numeric(PlanId),]$AgentId) %>%
    # Tag home SA1 with PersonId
    mutate(SA1_MAINCODE_2016 = ifelse(grepl("Home", Activity), AgentId, "")) %>%
    # Add location type tag
    mutate(LocationType=replaceActivityWithLocationTags(Activity)) %>%
    # Add a column for mode taken from last activity to this
    mutate(ArrivingMode=NA) %>%
    mutate(Distance=NA)

  echo('Assigning home SA1 locations\n')
  plans[plans$SA1_MAINCODE_2016!="",]$SA1_MAINCODE_2016<-
    apply(plans[plans$SA1_MAINCODE_2016!="",], 1, function(x) {
    persons[persons$AgentId==x["AgentId"],]$SA1_MAINCODE_2016
  })
  
  plans<-plans %>%
    mutate(SA1_MAINCODE_2016=as.numeric(SA1_MAINCODE_2016))

  echo('Assigning activities\' SA1s and travel modes (can take a while)\n')
  write.table(plans[FALSE,], file=outcsv, append=FALSE, row.names=FALSE, sep = ',')
  discarded<-persons[FALSE,]
  doutfile<-paste0(outdir, '/persons.discarded.csv')
  write.table(discarded, file=doutfile, append=FALSE, row.names=FALSE, sep = ',')
  wplans<-NULL
  # pp<-plans[1:95,] # just the first 10 plans
  pp<-plans
  i<-0
  homeSA1<-NA
  nextHome<-1
  returnTripLength<-NA
  processed<-0; ndiscarded<-0
  # set.seed(20200406)
  while(i<nrow(pp)) {
    i<-i+1
    # cat(paste0(i,"\n"))
    startOfDay <- i==1 || (pp[i,]$PlanId != pp[i-1,]$PlanId && 
                             pp[i,]$LocationType == "home")
    endOfDay <- i==nrow(pp) || (pp[i,]$AgentId != pp[i+1,]$AgentId)
    
    if(startOfDay) {
      homeSA1<-pp[i,]$SA1_MAINCODE_2016 # used for calculating return probabilities
      
      # nothing to do since home SA1s are already assigned; just save and continue
      wplans<-rbind(wplans, pp[i,])
    } else {
      # is the previous LocationType home?
      allowModeChange<-!startOfDay && !endOfDay && pp[i-1,]$LocationType=="home" # allow mode change at home during the day
      mode<-pp[i-1,]$ArrivingMode
      if(allowModeChange) {
        mode<-chooseMode(pp[i-1,]$SA1_MAINCODE_2016,pp[i,]$ArrivingMode) # choose a new mode
        nextHome<-which(pp$LocationType[(i):nrow(pp)]=="home")%>%first()+i-1 # find next home
        returnTripLength<-getReturnTripLength(pp[i-1,]$SA1_MAINCODE_2016,mode)
      }
      allowedSA1<-returnTripLength
      allowedSA1[allowedSA1>nextHome-i] <- NA
      allowedSA1[!is.na(allowedSA1)] <- 1
      
      
      modeAndSa1<-findLocationKnownMode(as.numeric(pp[i-1,]$SA1_MAINCODE_2016), pp[i,]$LocationType, mode, allowedSA1)
      if(pp[i,]$LocationType=="home") modeAndSa1<-c(mode,homeSA1)
      
      if(!is.null(modeAndSa1)) {
        # assign the mode and SA1
        pp[i,]$ArrivingMode<-modeAndSa1[1]
        # never change the SA1 of home regions
        if(pp[i,]$LocationType != "home") pp[i,]$SA1_MAINCODE_2016<-modeAndSa1[2]
        # add in the distance between the regions
        pp[i,]$Distance<-calcDistance(pp[i-1,]$SA1_MAINCODE_2016,pp[i,]$SA1_MAINCODE_2016)
        # add it to our list
        wplans<-rbind(wplans, pp[i,])
      }
      if(is.null(modeAndSa1)) {
        # failed to find a suitable SA1/mode for this activity, so will just discard this person
        person<-persons[persons$AgentId==pp[i,]$AgentId,]
        discarded<-rbind(discarded,person)
        # mark all modes for this plan with 'x' (will delete these later)
        pp[pp$PlanId==pp[i,]$PlanId,]$ArrivingMode<-'x'
        # cat(paste0("\n","found an error at line ",i,"\n"))
        # move to the first row of the next plan
        i<-as.numeric(last(rownames(pp[pp$PlanId==pp[i,]$PlanId,])))
      }
    }
    # record progress for each person
    if(i==nrow(pp) || pp[i,]$AgentId != pp[i+1,]$AgentId) {
      processed<-processed+1
      if(is.null(modeAndSa1)) {
        printProgress(processed, 'x')
      } else {
        printProgress(processed, '.')
      }
    }
    # write it out at regular intervals
    if (processed%%writeInterval==0 || i==nrow(pp)) {
      write.table(wplans, file=outcsv, append=TRUE, row.names=FALSE, col.names=FALSE, sep = ',')
      wplans<-wplans[FALSE,] # remove all rows
      if(nrow(discarded)>0) {
        ndiscarded<-ndiscarded + nrow(discarded)
        write.table(discarded, file=doutfile, append=TRUE, row.names=FALSE, col.names=FALSE, sep = ',')
        discarded<-discarded[FALSE,]
      }
    }
  }
  cat('\n')
  echo(paste0('Wrote ',(processed-ndiscarded),' plans to ', outcsv , '\n'))
  echo(paste0('Wrote ',ndiscarded,' discarded persons to ', doutfile , '\n'))
  
}

calculatePlanSubset <- function(planGroup,plans,outcsv,doutfile) {
  setDTthreads(1) # only one thread for data.table since we'll be operating in parallel
  
  discarded<-persons[FALSE,]
  wplans<-NULL
  pp<-plans%>%filter(ceiling(PlanId/100)==planGroup)
  i<-0
  homeSA1<-NA
  nextHome<-1
  returnTripLength<-NA
  processed<-0
  # set.seed(20200406)
  while(i<nrow(pp)) {
    i<-i+1
    startOfDay <- i==1 || (pp[i,]$PlanId != pp[i-1,]$PlanId && 
                             pp[i,]$LocationType == "home")
    endOfDay <- i==nrow(pp) || (pp[i,]$AgentId != pp[i+1,]$AgentId)
    
    if(startOfDay) {
      homeSA1<-pp[i,]$SA1_MAINCODE_2016 # used for calculating return probabilities
      
      # nothing to do since home SA1s are already assigned; just save and continue
      wplans<-rbind(wplans, pp[i,])
    } else {
      # is the previous LocationType home?
      allowModeChange<-!startOfDay && !endOfDay && pp[i-1,]$LocationType=="home" # allow mode change at home during the day
      mode<-pp[i-1,]$ArrivingMode
      if(allowModeChange) {
        mode<-chooseMode(pp[i-1,]$SA1_MAINCODE_2016,pp[i,]$ArrivingMode) # choose a new mode
        nextHome<-which(pp$LocationType[(i):nrow(pp)]=="home")%>%first()+i-1 # find next home
        returnTripLength<-getReturnTripLength(pp[i-1,]$SA1_MAINCODE_2016,mode)
      }
      allowedSA1<-returnTripLength
      allowedSA1[allowedSA1>nextHome-i] <- NA
      allowedSA1[!is.na(allowedSA1)] <- 1
      
      
      modeAndSa1<-findLocationKnownMode(as.numeric(pp[i-1,]$SA1_MAINCODE_2016), pp[i,]$LocationType, mode, allowedSA1)
      if(pp[i,]$LocationType=="home") modeAndSa1<-c(mode,homeSA1)
      
      if(!is.null(modeAndSa1)) {
        # assign the mode and SA1
        pp[i,]$ArrivingMode<-modeAndSa1[1]
        # never change the SA1 of home regions
        if(pp[i,]$LocationType != "home") pp[i,]$SA1_MAINCODE_2016<-modeAndSa1[2]
        # add in the distance between the regions
        pp[i,]$Distance<-calcDistance(pp[i-1,]$SA1_MAINCODE_2016,pp[i,]$SA1_MAINCODE_2016)
        # add it to our list
        wplans<-rbind(wplans, pp[i,])
      }
      if(is.null(modeAndSa1)) {
        # failed to find a suitable SA1/mode for this activity, so will just discard this person
        person<-persons[persons$AgentId==pp[i,]$AgentId,]
        discarded<-rbind(discarded,person)
        # mark all modes for this plan with 'x' (will delete these later)
        pp[pp$PlanId==pp[i,]$PlanId,]$ArrivingMode<-'x'
        # cat(paste0("\n","found an error at line ",i,"\n"))
        # move to the first row of the next plan
        i<-as.numeric(last(rownames(pp[pp$PlanId==pp[i,]$PlanId,])))
      }
    }
    # record progress for each person
    if(i==nrow(pp) || pp[i,]$AgentId != pp[i+1,]$AgentId) {
      processed<-processed+1
    }
  }
  write.table(wplans, file=outcsv, append=TRUE, row.names=FALSE, col.names=FALSE, sep = ',')
  write.table(discarded, file=doutfile, append=TRUE, row.names=FALSE, col.names=FALSE, sep = ',')
  return(data.frame(plan_group=planGroup,plans=processed,discarded=nrow(discarded)))
}


