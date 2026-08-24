getHouseholdHomeLocations <- function(plans,coordinateFunction=getAddressCoordinates) {
  requiredColumns<-c("HouseholdId","SA1_MAINCODE_2016","LocationType")
  if(!all(requiredColumns%in%colnames(plans))) return(NULL)

  householdHomes<-unique(
    plans[plans$LocationType=="home" & !is.na(plans$HouseholdId),
          c("HouseholdId","SA1_MAINCODE_2016")]
  )
  sa1sPerHousehold<-table(householdHomes$HouseholdId)
  if(any(sa1sPerHousehold>1)) {
    stop("Household members have inconsistent home SA1 locations")
  }
  if(nrow(householdHomes)==0) return(NULL)

  coordinates<-t(vapply(seq_len(nrow(householdHomes)),function(row) {
    coordinateFunction(householdHomes$SA1_MAINCODE_2016[row],"home")
  },numeric(2)))
  householdHomes$x<-coordinates[,1]
  householdHomes$y<-coordinates[,2]
  return(householdHomes)
}

assignLocationsToActivities <- function(plancsv,outdir,rseed=NULL) {
  # plancsv='../output/5.locate/plan.csv'
  # outdir='../output/6.place'
  # rseed=NULL
  # planGroup=1
  
  options(scipen=999) # disable scientific notation for more readable filenames with small sample sizes
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(data.table))
  suppressPackageStartupMessages(library(doParallel))
  suppressPackageStartupMessages(library(doRNG))
  
  assignLocationsSubset <- function(outdir,planGroup,plans) {
    setDTthreads(1) # only one thread for data.table since we'll be operating in parallel
    # assignLocationsToActivitiesSubset <- function(plancsv, outcsv, writeInterval) {
    
    pp<-plans%>%filter(ceiling(PlanId/1000)==planGroup)#%>%dplyr::mutate(Activity=0,AgentId=0)
    pp$x<-0; pp$y<-0
    processed<-0
    homexy<-NULL
    i=0
    while(i<nrow(pp)) {
      i<-i+1
      newPerson<-i==1 || pp[i,]$AgentId != pp[i-1,]$AgentId
      if(newPerson) {
        homexy<-NULL 
      }
      householdHomeIndex<-NA_integer_
      if(!is.null(householdHomeLocations) && pp[i,]$LocationType=="home") {
        householdHomeIndex<-match(pp[i,]$HouseholdId,householdHomeLocations$HouseholdId)
      }
      if(!is.na(householdHomeIndex)) {
        pp[i,]$x<-householdHomeLocations$x[householdHomeIndex]
        pp[i,]$y<-householdHomeLocations$y[householdHomeIndex]
      } else if(pp[i,]$LocationType=="home") {
        if(is.null(homexy)) {
          homexy <- getAddressCoordinates(pp[i,]$SA1_MAINCODE_2016, pp[i,]$LocationType)
        }
        pp[i,]$x <- homexy[1]
        pp[i,]$y <- homexy[2]
      } else {
        xy<-getAddressCoordinates(pp[i,]$SA1_MAINCODE_2016, pp[i,]$LocationType)
        pp[i,]$x<-xy[1]
        pp[i,]$y<-xy[2]
      }
      # record progress for each person
      if(i==nrow(pp) || pp[i,]$AgentId != pp[i+1,]$AgentId) processed<-processed+1
    }
    
    write.table(pp, file=paste0(outdir,'/plan/',planGroup,'.csv'),
                append=FALSE, row.names=FALSE, col.names=FALSE, sep = ',')
    return(data.frame(plan_group=planGroup,plans=processed))
  }
  
  # Read in the plans
  gz1<-gzfile(plancsv, 'rt')
  echo(paste0('Loading VISTA-like plans from ', plancsv, '\n'))
  plans<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)

  if(!is.null(rseed)) set.seed(rseed)
  householdHomeLocations<-getHouseholdHomeLocations(plans)
  
  # processing 1000 plans before saving.
  planGroups <- 1:ceiling(max(plans$PlanId,na.rm=T)/1000)
  
  dir.create(paste0(outdir,'/plan'), showWarnings = FALSE, recursive=TRUE)

  number_cores <- max(1,floor(as.integer(detectCores())*0.8))
  cl <- makeCluster(number_cores)
  echo(paste0("About to start assigning coordinates to activities in SA1s, using ",number_cores," cores\n"))
  echo(paste0("Now processing the ",length(planGroups)," plan groups(can take a while)\n"))

  registerDoParallel(cl)
  if (!is.null(rseed)) {
    registerDoRNG(seed = rseed)
  }
  start_time = Sys.time()
  results <- foreach(planGroup=planGroups,
                     .combine=rbind,
                     .verbose=FALSE,
                     .packages=c("doParallel", "dplyr","data.table"),
                     .export = c("getAddressCoordinates","addresses_dt","sa1_centroids_dt",
                                 "householdHomeLocations")
  ) %dopar% 
    assignLocationsSubset(outdir,planGroup,plans)
  end_time = Sys.time()
  end_time - start_time
  stopCluster(cl)
  
  #echo(paste0("Processing done, here are the results:\n"))
  #results
  
  echo(paste0("Combining plans into single file:\n"))
  
  planFilesDF <- data.frame(
    location=list.files(paste0(outdir,'/plan'),pattern="*.csv",full.names=T),
    order=list.files(paste0(outdir,'/plan'),pattern="*.csv",full.names=F)%>%gsub(".csv", "",.)%>%as.numeric(),
    stringsAsFactors=FALSE
  ) %>% arrange(order)
  
  plansCombined<-lapply(planFilesDF$location,read.csv,header=F) %>%
    bind_rows()
  outputColumns<-c(colnames(plans),setdiff(c("x","y"),colnames(plans)))
  colnames(plansCombined)<-outputColumns
  write.table(plansCombined, file=paste0(outdir,'/plan.csv'), append=FALSE, row.names=FALSE, sep = ',')
}
