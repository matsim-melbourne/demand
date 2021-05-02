locatePlans <- function(censuscsv, vistacsv, matchcsv, outdir, outcsv, rseed = NULL) {
  # example inputs
  # censuscsv <- '../output/2.sample/sample.csv.gz'
  # vistacsv  <- '../output/4.plan/plan.csv'
  # matchcsv  <- '../output/4.plan/plan2agent2group.csv'
  # outdir    <- '../output/5.locate'
  # outcsv    <- '../output/5.locate/plan.csv'
  # planGroup=1
  
calculatePlanSubset <- function(outdir,planGroup,plans) {
  setDTthreads(1) # only one thread for data.table since we'll be operating in parallel
  # read in all distances
  distanceCounts<<-readDistanceDistributions(outdir) # needs to be global
  distanceCountsCurrent<-data.frame(distance=seq(250,163250,500),
                                    walk_count=0,bike_count=0,pt_count=0,car_count=0)
  
  wplans<-NULL
  pp<-plans%>%filter(ceiling(PlanId/1000)==planGroup)#%>%dplyr::mutate(Activity=0,AgentId=0)
  # plans with 1 or 2 entries are automatically discarded.
  discarded<-pp%>%group_by(AgentId)%>%filter(n()<3)%>%slice_head()%>%dplyr::select(AgentId,SA1_MAINCODE_2016)%>%as.data.frame()
  pp<-pp%>%group_by(AgentId)%>%filter(n()>2)%>%ungroup()%>%as.data.frame()
  i<-0
  homeSA1<-NA
  nextHome<-NA
  returnTripLength<-NA
  processed<-0
  primary_mode<-NA
  anchor_region<-FALSE

  # tmp<-pp[(i-10):(i+10),]
  # tmp<-pp%>%group_by(AgentId)%>%filter(n()<3)%>%ungroup()
  # set.seed(20200406)
  # while(i<100) {
  while(i<nrow(pp)-1) {
    i<-i+1
    # if at home
    # if LocationType_{i}==home and LocationType_{i+1}!=home
    if(pp[i,"LocationType"] == "home" & pp[i+1,"LocationType"] != "home") {
      nextHome<-which(pp$LocationType[(i+1):nrow(pp)]=="home")%>%first()+i # find next home
      anchor_region<-FALSE
      # primary_mode <- chooseMode( SA1_MAINCODE_2016_{i} )
      primary_mode<-chooseMode(pp[i,"SA1_MAINCODE_2016"]) # SA1_MAINCODE_2016_{i} choose a new mode
      
      # validRegions<-getValidRegions(SA1_MAINCODE_2016_{nextHome}, primary_mode, nextHome-i)
      validRegions<-getValidRegions(pp[nextHome,6], primary_mode, nextHome-i)
      #validProportion=sum(validRegions,na.rm=T)/length(validRegions)

      # ArrivingMode_{i+1} <- primary_mode
      pp[i+1,"ArrivingMode"] <- primary_mode
      # SA1_MAINCODE_2016_{i+1} <- findLocationKnownMode( SA1_MAINCODE_2016_{i}, LocationType_{i+1}, primary_mode, allowedSA1 )
      pp[i+1,"SA1_MAINCODE_2016"] <- findLocationKnownMode(pp[i,"SA1_MAINCODE_2016"], # SA1_MAINCODE_2016_{i}
                                                           pp[i+1,"LocationType"],    # LocationType_{i+1}
                                                           primary_mode,              # primary_mode
                                                           validRegions)              # allowedSA1
      
      # ArrivingMode_{nextHome} <- primary_mode i.e. set the arriving mode of the next home region
      pp[nextHome,"ArrivingMode"] <- primary_mode
    }
    # if it is possible to change modes
    if( pp[i,"LocationType"] != "home" & (nextHome-i>2 | (nextHome-i==2 & primary_mode%in%c('walk','pt'))) ) {
      # ArrivingMode_{i+1} <- chooseMode( SA1_MAINCODE_2016_{i}, primary_mode, anchor_region )
      pp[i+1,"ArrivingMode"] <- chooseMode(pp[i,"SA1_MAINCODE_2016"], primary_mode, anchor_region)
      
      # setting anchor region and mode needed to get there
      if(anchor_region==FALSE & primary_mode%in%c('bike','car') & primary_mode!=pp[i+1,"ArrivingMode"]) {
        # set the region before home to the current region
        # SA1_MAINCODE_2016_{nextHome-1} <- SA1_MAINCODE_2016_{i}
        pp[nextHome-1,"SA1_MAINCODE_2016"] <- pp[i,"SA1_MAINCODE_2016"]
        # set the region before home's arriving mode to the next arriving mode
        # ArrivingMode_{nextHome-1} <- ArrivingMode_{i+1}
        pp[nextHome-1,"ArrivingMode"] <- pp[i+1,"ArrivingMode"]
        anchor_region <- TRUE
      }
      # if a walker has switched to pt, then the home region's arriving mode must be pt
      if(primary_mode=='walk' & pp[i+1,8]=='pt') {
        # ArrivingMode_{nextHome} <- 'pt'
        pp[nextHome,8] <- 'pt'
      }
      
      if(anchor_region==TRUE) {
        # valid regions are the ones reachable within the remaining steps to the
        # anchor region (the one before the next home region) using the anchor
        # region's arriving mode.
        # validRegions<-getValidRegions(SA1_MAINCODE_2016_{nextHome-1}, ArrivingMode_{nextHome-i-1}, nextHome-i-1)
        validRegions<-getValidRegions(pp[nextHome-1,"SA1_MAINCODE_2016"],
                                      pp[nextHome-1,"ArrivingMode"],
                                      nextHome-i-2)
        # SA1_MAINCODE_2016_{i+1} <- findLocationKnownMode( SA1_MAINCODE_2016_{i}, LocationType_{i+1}, primary_mode, allowedSA1 )
        pp[i+1,"SA1_MAINCODE_2016"] <- findLocationKnownMode(pp[i,"SA1_MAINCODE_2016"],
                                                             pp[i+1,"LocationType"],
                                                             pp[i+1,"ArrivingMode"],
                                                             validRegions)
      }
      if(anchor_region==FALSE) {
        # valid regions are the ones reachable within the remaining steps to the
        # home region using its arriving mode.
        # validRegions<-getValidRegions(SA1_MAINCODE_2016_{nextHome}, ArrivingMode_{nextHome-i-1}, nextHome-i-1)
        validRegions<-getValidRegions(pp[nextHome,"SA1_MAINCODE_2016"],
                                      pp[nextHome,"ArrivingMode"],
                                      nextHome-i-1)
        # SA1_MAINCODE_2016_{i+1} <- findLocationKnownMode( SA1_MAINCODE_2016_{i}, LocationType_{i+1}, primary_mode, allowedSA1 )
        pp[i+1,"SA1_MAINCODE_2016"] <- findLocationKnownMode(pp[i,"SA1_MAINCODE_2016"],
                                                             pp[i+1,"LocationType"],
                                                             pp[i+1,"ArrivingMode"],
                                                             validRegions)
      }
    }
    if( pp[i,"LocationType"] != "home" & nextHome-i==2 & primary_mode%in%c('bike','car') & anchor_region==FALSE ) {
      # ArrivingMode_{i+1} <- ArrivingMode_{i}
      pp[i+1,"ArrivingMode"] <- pp[i,"ArrivingMode"]
      # valid regions are the ones reachable within the remaining steps to the
      # home region using its arriving mode.
      # validRegions<-getValidRegions(SA1_MAINCODE_2016_{nextHome}, ArrivingMode_{nextHome-i-1}, nextHome-i-1)
      validRegions<-getValidRegions(pp[nextHome,"SA1_MAINCODE_2016"],
                                    pp[nextHome,"ArrivingMode"],
                                    nextHome-i-1)
      # SA1_MAINCODE_2016_{i+1} <- findLocationKnownMode( SA1_MAINCODE_2016_{i}, LocationType_{i+1}, primary_mode, allowedSA1 )
      pp[i+1,"SA1_MAINCODE_2016"] <- findLocationKnownMode(pp[i,"SA1_MAINCODE_2016"],
                                                           pp[i+1,"LocationType"],
                                                           primary_mode,
                                                           validRegions)
    }
      
    # if the next LocationType isn't home, calculate distance
    if( pp[i,"LocationType"] != 'home' | (pp[i,"LocationType"] == 'home' & pp[i+1,"LocationType"] != 'home') ) {
      # Distance_{i+1} <- calcDistance( Distance_{i}, Distance_{i+1} )
      currentDistance <- calcDistance(pp[i,"SA1_MAINCODE_2016"],pp[i+1,"SA1_MAINCODE_2016"])
      if(!is.na(currentDistance)) {
        currentCol<-which(colnames(distanceCounts)==paste0(pp[i+1,"ArrivingMode"],"_count"))
        currentRow<-findInterval(currentDistance,seq(0,163500,500))
        distanceCounts[currentRow,currentCol]<-distanceCounts[currentRow,currentCol]+1
        distanceCountsCurrent[currentRow,currentCol]<-distanceCountsCurrent[currentRow,currentCol]+1
        pp[i,"Distance"] <- currentDistance
      }
    }
    
    # if SA1_MAINCODE_2016_{i+1} is null
    if(pp[i+1,"SA1_MAINCODE_2016"]==-1) {
      # failed to find a suitable SA1/mode for this activity, so will just discard this person
      person<-persons[persons$AgentId==pp[i,]$AgentId,]
      discarded<-rbind(discarded,person)
      # mark all modes for this plan with 'x' (will delete these later)
      pp[pp$PlanId==pp[i,]$PlanId,]$ArrivingMode<-'x'
      # cat(paste0("\n","found an error at line ",i,"\n"))
      # move to the first row of the next plan
      i<-as.numeric(last(rownames(pp[pp$PlanId==pp[i,]$PlanId,])))
    }
    # record progress for each person
    if(i==nrow(pp) || pp[i,]$AgentId != pp[i+1,]$AgentId) {
      processed<-processed+1
    }
    
    # if SA1_MAINCODE_2016_{i} is not null
    if(pp[i,"SA1_MAINCODE_2016"]!=-1) {
      wplans<-rbind(wplans, pp[i,])
    }
  }
  # removing discarded plans
  wplans<-wplans%>%filter(ArrivingMode!='x'|is.na(ArrivingMode))
  saveRDS(distanceCountsCurrent,file=paste0(outdir,'/distanceCounts/',planGroup,'.rds'))
  write.table(wplans, file=paste0(outdir,'/plan/',planGroup,'.csv'),
              append=FALSE, row.names=FALSE, col.names=FALSE, sep = ',')
  write.table(discarded, file=paste0(outdir,'/discarded/',planGroup,'.csv'),
              append=FALSE, row.names=FALSE, col.names=FALSE, sep = ',')
  return(data.frame(plan_group=planGroup,plans=processed,discarded=nrow(discarded)))
}

# assignActivityAreasAndTravelModesParallel <-function(censuscsv, vistacsv, matchcsv, outdir, outcsv) {

options(scipen=999) # disable scientific notation for more readable filenames with small sample sizes

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(doRNG))

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
persons<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)%>%
  dplyr::select(AgentId,SA1_MAINCODE_2016) %>%
  mutate(AgentId=as.factor(AgentId))
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
plans<-origplans[,c("PlanId","Activity","StartBin","EndBin")] %>%
  # Remove all plans that are not matched
  filter(PlanId %in% matches$PlanId) %>% 
  # Assign matched PersonId (very fast since we assume row number equals Id number)
  mutate(AgentId = matches[as.numeric(PlanId),]$AgentId) %>%
  mutate(AgentId=as.factor(AgentId))

echo('Assigning home SA1 locations\n')
plans<-plans %>%
  # Tag home SA1 with PersonId
  inner_join(persons,by="AgentId") %>%
  mutate(SA1_MAINCODE_2016 = ifelse(Activity=="Home",SA1_MAINCODE_2016,NA)) %>%
  # Add location type tag
  mutate(LocationType=replaceActivityWithLocationTags(Activity)) %>%
  # Add a column for mode taken from last activity to this
  mutate(ArrivingMode=NA) %>%
  mutate(Distance=NA) %>%
  mutate(AgentId=as.character(AgentId))



echo('Assigning activities\' SA1s and travel modes (can take a while)\n')
# processing 1000 plans before saving.
planGroups <- 1:ceiling(max(plans$PlanId,na.rm=T)/1000)

dir.create(paste0(outdir,'/plan'), showWarnings = FALSE, recursive=TRUE)
dir.create(paste0(outdir,'/discarded'), showWarnings = FALSE, recursive=TRUE)

dir.create(paste0(outdir,"/distanceCounts"), showWarnings = FALSE, recursive=TRUE)
unlink(paste0(outdir,"/distanceCounts/*"))
saveRDS(data.frame(distance=seq(250,163250,500),
                   walk_count=0,bike_count=0,pt_count=0,car_count=0),
        paste0(outdir,"/distanceCounts/0.rds"))

number_cores <- max(1,floor(as.integer(detectCores())*0.8))
cl <- makeCluster(number_cores)
echo(paste0("About to start processing density in parallel, using ",number_cores," cores\n"))
echo(paste0("Now processing the ",length(planGroups)," plan groups\n"))

registerDoParallel(cl)
if (!is.null(rseed)) {
  registerDoRNG(seed = rseed)
}
start_time = Sys.time()
results <- foreach(planGroup=planGroups,
                   .combine=rbind,
                   .verbose=FALSE,
                   .packages=c("doParallel", "sf","dplyr","scales","data.table"),
                   .export = c("calcDistance", "calculateProbabilities", "chooseMode", 
                               "findLocationKnownMode", "getReturnTripLength", 
                               "SA1_attributed", "SA1_attributed_dt", 
                               "distanceMatrix", "distanceMatrixIndex", "distanceMatrixIndex_dt",
                               "getValidRegions", "readDistanceDistributions", "expectedDistances")
) %dopar% 
  calculatePlanSubset(outdir,planGroup,plans)
end_time = Sys.time()
end_time - start_time
stopCluster(cl)

#echo(paste0("Processing done, here are the results:\n"))
#results

echo(paste0("Combining plans into single file:\n"))

# planFiles<-list.files(paste0(outdir,'/plan'),pattern="*.csv",full.names=T)
planFilesDF <- data.frame(
  location=list.files(paste0(outdir,'/plan'),pattern="*.csv",full.names=T),
  order=list.files(paste0(outdir,'/plan'),pattern="*.csv",full.names=F)%>%gsub(".csv", "",.)%>%as.numeric(),
  stringsAsFactors=FALSE
) %>% arrange(order)

plansCombined<-lapply(planFilesDF$location,read.csv,header=F) %>%
  bind_rows()
colnames(plansCombined)<-c("PlanId","Activity","StartBin","EndBin","AgentId",
                           "SA1_MAINCODE_2016","LocationType","ArrivingMode",
                           "Distance")
write.table(plansCombined, file=outcsv, append=FALSE, row.names=FALSE, sep = ',')

discardedFiles<-list.files(paste0(outdir,'/discarded'),pattern="*.csv",full.names=T)

# some files will be empty, so need a more complex function
discardedCombined<-lapply(discardedFiles, function(x) {
  tmp <- try(read.csv(x,header=F),silent=T)
  if (!inherits(tmp, 'try-error')) tmp
}) %>%
  bind_rows()
colnames(discardedCombined)<-c("AgentId","SA1_MAINCODE_2016")
doutfile<-paste0(outdir, '/persons.discarded.csv')
write.table(discardedCombined, file=doutfile, append=FALSE, row.names=FALSE, sep = ',')

}
