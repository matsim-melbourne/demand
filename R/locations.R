suppressPackageStartupMessages(library(sf)) # for spatial things
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(scales)) # for scaling datasets
suppressPackageStartupMessages(library(data.table)) # for sa1_main16 indexing for faster lookups

loadLocationsData <- function(distanceMatrixFile, distanceMatrixIndexFile,
                             sa1AttributedFile, sa1CentroidsFile, addressesFile, distancesFile, destinationFile) {
  # Read in the distance matrix. This matrix is symmetric so it doesn't matter if
  # you do lookups by column or row.
  echo(paste0("Reading ", distanceMatrixFile, "\n"))
  distanceMatrix <<- readRDS(file=distanceMatrixFile) # note '<<' to make it global
  
  # Some SA1s ended up snapping their centroid to the same node in the road
  # network so we need to use an index.
  echo(paste0("Reading ", distanceMatrixIndexFile, "\n"))
  distanceMatrixIndex <<- readRDS(file=distanceMatrixIndexFile)
  distanceMatrixIndex_dt <<- data.table(distanceMatrixIndex) # note '<<' to make it global
  setkey(distanceMatrixIndex_dt, sa1_maincode_2016)
  
  # Reading in the attributed SA1 regions. I'm removing the geometry since it
  # won't be used here. Joining with the distance matrix index so the regions are
  # in the correct order.
  echo(paste0("Reading ", sa1AttributedFile, "\n"))
  SA1_attributed <<- readRDS(file=sa1AttributedFile)
  SA1_attributed_dt <<- data.table(SA1_attributed)  # note '<<' to make it global
  setkey(SA1_attributed_dt,sa1_maincode_2016)
  
  # Reading in the addresses. I'm removing the geometry and converting it to X,Y.
  # These coordinates are in EPSG:28355, which is a projected coordinate system.
  echo(paste0("Reading ", addressesFile, "\n"))
  addresses <<- readRDS(file=addressesFile)
  addresses_dt <<- data.table(addresses)  # note '<<' to make it global
  setkey(addresses_dt, sa1_maincode_2016)
  
  # Need the x and y locations of the centroids
  echo(paste0("Reading ", sa1CentroidsFile, "\n"))
  sa1_centroids <<- readRDS(file=sa1CentroidsFile)
  sa1_centroids_dt <<- data.table(sa1_centroids)  # note '<<' to make it global
  setkey(sa1_centroids_dt, sa1_maincode_2016)
  
  expectedDistances <<- readRDS(file=distancesFile)
  expectedDestinations <<- readRDS(file=destinationFile)
  expectedDestinations_dt <<- data.table(expectedDestinations)  # note '<<' to make it global
}

# This returns a dataframe with possible SA1_ids and their probabilities.
# There are three probabilities returned:
# 1: distProb. The probability of choosing a destination based on distance.
# 2: attractProb. The probability of choosing a destination based on that
#                 destination's attractiveness.
# 3: combinedProb. The combined probability of choosing a destination based on
#                  distance and destination attractiveness. Distance is
#                  currently weighted 4x higher since distance probability is a
#                  lot more spread out.
# calculateProbabilities(20604112202,"commercial","car")
calculateProbabilities <- function(SA1_id,destination_category,mode,allowedSA1=NULL) {
  # SA1_id=20607113907;destination_category="work";mode="walk"
  # allowedSA1<-getValidRegions(20607113907,"walk",1)
  # SA1_id=20607113907;destination_category="work";mode="car"
  # allowedSA1<-getValidRegions(20607113907,"car",1)
  # if you don't include allowedSA1, then we assume all regions are allowed
  if(is.null(allowedSA1)) allowedSA1<-rep(1,nrow(distanceMatrix))
  index <- distanceMatrixIndex_dt[.(as.numeric(SA1_id))] %>%
    pull(index)
  distances <-data.frame(index=1:nrow(distanceMatrix),
                         distance=distanceMatrix[index,]) %>%
    inner_join(distanceMatrixIndex, by=c("index"="index")) %>%
    arrange(sa1_maincode_2016) %>%
    pull(distance)
  
  modeMean <- NULL
  # modeMean <- SA1_attributed[which(SA1_attributed$sa1_maincode_2016==as.numeric(SA1_id)),
  #                            which(colnames(SA1_attributed_dt)==paste0("meanlog_",mode))]
  modeSD <- NULL
  expected_prop <- NULL
  actual_count <- NULL
  filteredset<-SA1_attributed_dt[.(as.numeric(SA1_id))]
  if(mode=="walk"){
    modeMean <- filteredset$meanlog_walk
    modeSD <- filteredset$sdlog_walk
    expected_prop <- expectedDistances$walk_prop
    actual_count <- distanceCounts$walk_count
  } else if(mode=="car"){
    modeMean <- filteredset$meanlog_car
    modeSD <- filteredset$sdlog_car
    expected_prop <- expectedDistances$car_prop
    actual_count <- distanceCounts$car_count
  } else if(mode=="pt"){
    modeMean <- filteredset$meanlog_pt
    modeSD <- filteredset$sdlog_pt
    expected_prop <- expectedDistances$pt_prop
    actual_count <- distanceCounts$pt_count
  } else if(mode=="bike"){
    modeMean <- filteredset$meanlog_bike
    modeSD <- filteredset$sdlog_bike
    expected_prop <- expectedDistances$bike_prop
    actual_count <- distanceCounts$pt_count
  }
  
  # calculating global distance distribution probabilities
  actual_prop=actual_count*0 # default to 0
  if(sum(actual_count,na.rm=T)>0) actual_prop=actual_count/sum(actual_count,na.rm=T)
  diff_prop=expected_prop-actual_prop
  diff_prop[diff_prop<0]<-0 # negative values could break things
  globalProb <- data.frame(interval=expectedDistances$distance,
                           expected_prop=diff_prop)
  expectedProbability <- rep(0,length(distances))
  attractionProbability <- SA1_attributed[,match(destination_category,colnames(SA1_attributed))] %>%
    unlist() %>% as.vector()
    
  attractionProbability<-attractionProbability/sum(attractionProbability,na.rm=T) # normalise
  
  # calculating global destination attraction probabilities
  expected_prob_dest <- expectedDestinations_dt[location_type == destination_category, .(prob_expected)][[1]]
  actual_count_dest <- destinationCounts[,which(colnames(destinationCounts)==paste0(destination_category,"_count"))]
  actual_prop_dest=actual_count_dest*0 # default to 0
  if(sum(actual_count_dest,na.rm=T)>0) actual_prop_dest=actual_count_dest/sum(actual_count_dest,na.rm=T)
  diff_prop_dest=expected_prob_dest-actual_prop_dest
  diff_prop_dest[diff_prop_dest<0]<-0 # negative values could break things
  expectedDestinationsSA1 <- merge(
    data.table(sa1_maincode_2016=distanceMatrixIndex_dt$sa1_maincode_2016,
               # the sa3 number is just the first 5 digits of the sa1 number
               sa3_code_2016=as.integer(substr(distanceMatrixIndex_dt$sa1_maincode_2016,1,5))),
    data.table(sa3_code_2016=destinationCounts$sa3_code_2016,
               expected_prob=diff_prop_dest),
    # discard regions with no valid destination types
    all=FALSE)[order(sa1_maincode_2016),][,valid := ifelse(is.na(attractionProbability),NA,1)][,expected_prob := expected_prob*valid]
  expectedDestinationsSA1weighted <- merge(
    expectedDestinationsSA1,
    expectedDestinationsSA1[, .(countValid=sum(valid,na.rm=T)), by = sa3_code_2016],
    all=FALSE
  )[,expected_prob := expected_prob/countValid]
  globalAttraction <- expectedDestinationsSA1weighted$expected_prob
  if(sum(globalAttraction,na.rm=T)>0) globalAttraction<-globalAttraction/sum(globalAttraction,na.rm=T) # normalise
  
  # alternative way to compute distance probabilities for SA1s clipped to a much smaller set
  # within 2 standard deviations of the mode mean - Dhi, 21/Feb/20
  dd <- distances 
  dd[dd<200] <- 200 # Distances less than 200m are set to 200. This is to account for intra-SA1 trips
  if(mode=="walk") dd[dd>5000] <- NA # Shouldn't ever walk more then 5km (i.e. ~ 1 hour)
  dd[dd<qlnorm(0.05,modeMean,modeSD) | dd>qlnorm(0.95,modeMean,modeSD)]<- NA # discard anything >2SDs either side
  dd[is.na(attractionProbability)] <- NA # discard regions with no valid destination types
  dd[is.na(allowedSA1)] <- NA # discard regions too far away from home
  distProbability <- dd
  if(sum(!is.na(dd)) == 0) return(NULL) # return NULL if nothing is left
  if(sum(!is.na(dd)) == 1) { # if only one possible destination
    distProbability[!is.na(distProbability)]<-1
  } else {
    distProbability<-plnorm(dd+0.5,modeMean,modeSD)-plnorm(dd-0.5,modeMean,modeSD)
    # distanceInterval<-findInterval(dd,seq(0,max(dd,na.rm=T),500))*500-250
    groupProb<-data.frame(interval=seq(0,max(dd,na.rm=T),500)+250) %>%
      mutate(group_prob=plnorm(interval+250,modeMean,modeSD)-plnorm(interval-250,modeMean,modeSD)) %>%
      inner_join(globalProb,by="interval")
      
    
    # There are a lot less short distances than longer distances, we need to take
    # this into account for the distributions to look right
    proportionDF <- data.frame(distance=dd,distance_prob=distProbability) %>%
      mutate(interval=findInterval(distance,seq(0,max(dd,na.rm=T),500))*500-250) %>%
      group_by(interval) %>%
      mutate(group_proportion=distance_prob/sum(distance_prob,na.rm=T),
             group_count=n()) %>%
      ungroup() %>%
      left_join(groupProb, by="interval") %>%
      mutate(corrected_proportion=group_proportion*group_prob,
             global_proportion=expected_prop/group_count)
    distProportion<-proportionDF$corrected_proportion
    # expected distance probability
    expectedProbability<-proportionDF$global_proportion
    if(sum(expectedProbability,na.rm=T)>0) expectedProbability<-expectedProbability/sum(expectedProbability,na.rm=T) # normalising

    distProbability<-distProportion/sum(distProportion,na.rm=T) # normalise
  }
  
  # I've set distance probability to 4x more important than destination
  # attraction. This is arbitrary.
  # multiplier=1 #  changed this from 4 to 1 - Dhi, 2020/02/21
  # attractionMultiplier<-1
  # if(destination_category=="commercial"|destination_category=="park") attractionMultiplier<-5
  # if(destination_category=="work") attractionMultiplier<-10
  # # combinedDensity <- multiplier*distProbability+expectedProbability
  # combinedDensity <- distProbability+attractionMultiplier*attractionProbability+expectedProbability*5
  # combinedDensity <- distProbability+attractionProbability+expectedProbability*2+globalAttraction*4
  # combinedDensity <- attractionProbability*2+globalAttraction*4
  combinedDensity <- distProbability+expectedProbability*2 + attractionProbability*2+globalAttraction*5
  # combinedDensity <- attractionProbability
  # combinedDensity <- expectedProbability # this won't work on its own, sometimes all probs are 0.
  combinedProbability <- combinedDensity
  if(sum(combinedDensity,na.rm=T)>0) combinedProbability <- combinedDensity/sum(combinedDensity, na.rm=TRUE) # normalising here so the sum of the probabilities equals 1
  probabilityDF <- data.frame(sa1_maincode_2016=sort(SA1_attributed$sa1_maincode_2016),
                              # distProb=distProbability,
                              # attractProb=attractionProbability,
                              combinedProb=combinedProbability) %>%
                              filter(!is.na(combinedProb))
  return(probabilityDF)
}




getValidRegions <- function(SA1_id,mode,stops) {
  # SA1_id=20607113907
  # mode="walk"
  # stops=1
  
  index <- distanceMatrixIndex_dt[.(as.numeric(SA1_id)),2]%>%as.numeric()
  
  distances <-data.frame(index=1:nrow(distanceMatrix),
                         distance=distanceMatrix[index,]) %>% # look down columns
    inner_join(distanceMatrixIndex, by=c("index"="index")) %>%
    arrange(sa1_maincode_2016) %>%
    pull(distance)
  distances[distances<1] <- 1 # don't want to divide by 0
  
  filteredset<-SA1_attributed_dt %>% # get mean and sd for current mode
    dplyr::select(Mean=paste0("meanlog_",mode),SD=paste0("sdlog_",mode))
  
  distance95 <- qlnorm(0.95,filteredset$Mean,filteredset$SD)
  numberJumpsBack <- ceiling(distances/distance95) #how many jumps to get back to home SA1
  # View(data.frame(distance=distances,distance95=distance95,numberJumpsBack=numberJumpsBack))
  
  allowedSA1<-numberJumpsBack
  allowedSA1[allowedSA1>stops] <- NA
  allowedSA1[!is.na(allowedSA1)] <- 1
  return(allowedSA1)
}

getReturnTripLength <- function(SA1_id,mode) {
  # SA1_id=20604112202
  # mode="car"
  
  index <- distanceMatrixIndex_dt[.(as.numeric(SA1_id))] %>%
    pull(index)
  distances <-data.frame(index=1:nrow(distanceMatrix),
                         distance=distanceMatrix[index,]) %>% # look down columns
    inner_join(distanceMatrixIndex, by=c("index"="index")) %>%
    arrange(sa1_maincode_2016) %>%
    pull(distance)
  distances[distances<1] <- 1 # don't want to divide by 0
  
  filteredset<-SA1_attributed_dt %>% # get mean and sd for current mode
    dplyr::select(Mean=paste0("meanlog_",mode),SD=paste0("sdlog_",mode))
  
  distance95 <- qlnorm(0.95,filteredset$Mean,filteredset$SD)
  numberJumpsBack <- ceiling(distances/distance95) #how many jumps to get back to home SA1
  # View(data.frame(distance=distances,distance95=distance95,numberJumpsBack=numberJumpsBack))
  return(numberJumpsBack)
}


# This will be made more detailed later, and actually take destination category
# into account.
# chooseMode(20604112202)
chooseMode <- function(SA1_id, primary_mode=NA, anchor_region=FALSE) {
  # SA1_id=20604112202

  if(is.na(primary_mode)) primary_mode<-''
  # a list of the four mode probabilities for this SA1
  modeProbability <- SA1_attributed_dt[.(SA1_id),walk_proportion:car_proportion] %>%
    unlist()
  
  modeProbabilityDF <- data.table(mode=c("walk","bike","pt","car"),
                                  modeProbability,
                                  stringsAsFactors=FALSE)
  if(primary_mode=='bike' & anchor_region==FALSE) {
    modeProbabilityDF <- modeProbabilityDF[mode%in%c('walk','bike','pt')]
  }
  if(primary_mode=='car' & anchor_region==FALSE) {
    modeProbabilityDF <- modeProbabilityDF[mode%in%c('walk','pt','car')]
  }
  if(anchor_region==TRUE | (primary_mode%in%c('walk','pt') & anchor_region==FALSE) ) {
    modeProbabilityDF <- modeProbabilityDF[mode%in%c('walk','pt')]
  }
  mode<-sample(modeProbabilityDF$mode, size=1,
                prob=modeProbabilityDF$modeProbability)
  return(mode)
}

# This will be made more detailed later, and actually take destination category
# into account.
# chooseMode(20604112202,"commercial")
chooseModeOld <- function(SA1_id,destination_category) {
  # SA1_id=20604112202
  # destination_category="commercial"
  
  # a list of the four mode probabilities for this SA1
  modeProbability <- SA1_attributed_dt[.(as.numeric(SA1_id))] %>%
    dplyr::select(walk_proportion:car_proportion) %>%
    unlist()
  
  modeProbabilityDF <- data.frame(mode=c("walk","bike","pt","car"),
                                  modeProbability,
                                  stringsAsFactors=FALSE)
  mode<-sample(modeProbabilityDF$mode, size=1,
               prob=modeProbabilityDF$modeProbability)
  return(mode)
}
# Assuming the transport mode is restricted, this will find a destination SA1
# findLocationKnownMode(20604112202,"commercial","car")
findLocationKnownMode <- function(SA1_id,destination_category,mode,allowedSA1) {
  #cat(paste0("\nSA1_id=[",SA1_id,"] destination_category=[",destination_category,"] mode=[",mode,"]\n"))
  probabilityDF <- calculateProbabilities(SA1_id,destination_category,mode,allowedSA1)
  #cat(str(probabilityDF))
  if(is.null(probabilityDF)) return(-1)
  if(length(probabilityDF$sa1_maincode_2016)==1) {
    destinationSA1<-probabilityDF$sa1_maincode_2016
  } else {
    destinationSA1 <- sample(probabilityDF$sa1_maincode_2016, size=1,
                           prob=probabilityDF$combinedProb)
  }
  return(destinationSA1)
}

# Find a destination SA1 given a source SA1 and destination category
# findLocation(20604112202,"commercial")
findLocation <- function(SA1_id,destination_category,canReachHome) {
  return(findLocationKnownMode(SA1_id,destination_category,chooseMode(SA1_id,destination_category),canReachHome))
}


# Determine the chances of returning home for a given destination and transport mode
# getReturnProbability(20604112202,20604112210,"car")
getReturnProbability <- function(source_SA1,destination_SA1,mode) {
  # source_SA1=20607113903
  # destination_SA1=20803119308
  # mode="car"
  probabilityDF <- calculateProbabilities(destination_SA1,"home",mode)
  sourceProb <- probabilityDF %>%
    filter(sa1_maincode_2016==source_SA1) %>%
    pull(distProb) # Note that we only use the distance probability here, not
                   # the combined probability.
  # sourceProb only returns regions within 2sd of the mean, so if the 
  # destination is too far away, we need to manually set the probability to zero
  if(length(sourceProb)==0) (sourceProb=0)
  sourceProb <- sourceProb*nrow(probabilityDF)
  # multiplying by the number of possible regions to give the proportion of
  # choosing that region, where > 1 is more likely than choosing the region
  # completely at random.
  # The idea being that a value > 1 means that it's suitable.
  return(sourceProb)
}

# Assign coordinates to a location within a specified SA1 with a specified category 
# getAddressCoordinates(20604112202,"commercial")
# getAddressCoordinates(21005144422,"home")

getAddressCoordinates <- function(SA1_id,destination_category) {
  # SA1_id=21005144422
  # destination_category="home"
  potentialAddresses <- addresses_dt[.(SA1_id),]
  potentialAddresses <- potentialAddresses[potentialAddresses$category==destination_category,]
  if(nrow(potentialAddresses)==0) {
    # if no suitable destinations are found, default to the centroid of the SA1 region
    return(sa1_centroids_dt[.(SA1_id), .(X,Y)]%>%unlist())
  }
  address_id <- sample(1:nrow(potentialAddresses), size=1,
                    prob=potentialAddresses$count)
  
  # finding X and Y for selected address, then converting to Named num
  return(potentialAddresses[address_id,4:5]%>%unlist())
}

# Returns the distance between two regions
calcDistance <- function(from_sa1,to_sa1) {
  distanceMatrix[distanceMatrixIndex_dt[.(as.numeric(from_sa1))]$index,
                 distanceMatrixIndex_dt[.(as.numeric(to_sa1))]$index]
}

# converts an SA1 id to an SA3 id
toSA3 <- function(id) as.integer(substr(id,1,5))


# Takes a plan with completed SA1 locations and turns them into a series of
# lines where the non-spatial data is for the destination.
# Need to supply a plan and an output file location. I recommend using the 
# .sqlite extension instead of shapefiles.
# planToSpatial(read.csv("output/5.locate/plan.csv"),'output/5.locate/plan.sqlite')
planToSpatial <- function(pp,fileLocation) {
  
  ppp <- pp %>%
    mutate(SA1_MAINCODE_2016=as.numeric(SA1_MAINCODE_2016)) %>%
    # Need the previous SA1 region
    mutate(prev_sa1=lag(SA1_MAINCODE_2016)) %>%
    # Ignore the first entries for a person as they won't have a valid prev SA1
    filter(!is.na(ArrivingMode)) %>%
    inner_join(sa1_centroids, by=c("prev_sa1"="sa1_maincode_2016")) %>%
    inner_join(sa1_centroids, by=c("SA1_MAINCODE_2016"="sa1_maincode_2016")) %>%
    # turn the two SA1 centroids into line geometry
    mutate(GEOMETRY=paste0("LINESTRING(",X.x," ",Y.x,",",X.y," ",Y.y,")")) %>%
    st_as_sf(wkt = "GEOMETRY", crs = 28355) %>%
    dplyr::select(PlanId,Activity,StartBin,EndBin,AgentId,SA1_MAINCODE_2016,
                  LocationType,ArrivingMode,Distance)
  # Write the spatial dataframe to file
  st_write(ppp,fileLocation,delete_dsn=TRUE)
}

# Takes a plan with places and turns them into a series of
# lines where the non-spatial data is for the destination.
# Need to supply a plan and an output file location. I recommend using the 
# .sqlite extension instead of shapefiles.
# placeToSpatial(read.csv("output/6.place/plan.csv"),'output/6.place/plan.sqlite')
placeToSpatial <- function(pp,fileLocation) {
  # pp=read.csv("../output/6.place/plan.csv")
  # fileLocation='output/6.place/plan.sqlite'
  ppp <- pp %>%
    # Ignore the first entries for a person as they won't have a valid previous location
    # turn the two SA1 centroids into line geometry
    mutate(GEOMETRY=paste0("LINESTRING(",lag(x)," ",lag(y),",",x," ",y,")")) %>%
    filter(!is.na(ArrivingMode)) %>%
    st_as_sf(wkt = "GEOMETRY", crs = 28355) # %>%
    # some legs end up at the same address, this would remove them
    # filter(st_is_valid(.))
  # Write the spatial dataframe to file
  st_write(ppp,fileLocation,delete_layer=TRUE,layer="lines",quiet=TRUE)
  
  ppp2 <- pp %>%
    # Ignore the first entries for a person as they won't have a valid previous location
    # turn the two SA1 centroids into line geometry
    mutate(GEOMETRY=paste0("POINT(",x," ",y,")")) %>%
    filter(!is.na(ArrivingMode)) %>%
    st_as_sf(wkt = "GEOMETRY", crs = 28355)
  st_write(ppp2,fileLocation,delete_layer=TRUE,layer="points",quiet=TRUE)
}

# don't need
calculateExpected <- function(transport_mode) {
  # transport_mode="walk"
  # distanceCounts<-distanceCounts%>%mutate(walk_count=sample(0:30, nrow(expectedHistograms), replace=T))
  expected_prop<-NULL
  actual_count<-NULL
  if(transport_mode=="walk"){
    expected_prop <- expectedDistances$walk_prop
    actual_count <- distanceCounts$walk_count
  } else if(transport_mode=="car"){
    expected_prop <- expectedDistances$car_prop
    actual_count <- distanceCounts$car_count
  } else if(transport_mode=="pt"){
    expected_prop <- expectedDistances$pt_prop
    actual_count <- distanceCounts$pt_count
  } else if(transport_mode=="bike"){
    expected_prop <- expectedDistances$bike_prop
    actual_count <- distanceCounts$pt_count
  }
  actual_prop=actual_count*0
  if(sum(actual_count,na.rm=T)>0) actual_prop=actual_count/sum(actual_count,na.rm=T)
  diff_prop=expected_prop-actual_prop
  diff_prop[diff_prop<0]<-0 # negative values could break things
  df <- data.frame(distance=expectedDistances$distance,
                   expected_prop=diff_prop)
  return(df)
}

createExpectedDistances <- function(vistaLocation,outdir) {
  # outdir='../output/1.setup';vistaLocation='../data/vistaSummaries/distanceHistograms.rds'
  
expDist<-readRDS(vistaLocation) %>%
  dplyr::select(mode=transport_mode,logmean,logsd) %>%
  distinct() %>%
  mutate(mode=as.character(mode))

expectedDistances <- data.frame(distance=seq(250,163250,500)) %>%
  mutate(logmean=expDist[expDist$mode=='walk',]$logmean,
         logsd=expDist[expDist$mode=='walk',]$logsd) %>%
  mutate(walk_prop=plnorm(distance+250,logmean,logsd)-plnorm(distance-250,logmean,logsd)) %>%
  mutate(logmean=expDist[expDist$mode=='bike',]$logmean,
         logsd=expDist[expDist$mode=='bike',]$logsd) %>%
  mutate(bike_prop=plnorm(distance+250,logmean,logsd)-plnorm(distance-250,logmean,logsd)) %>%
  mutate(logmean=expDist[expDist$mode=='pt',]$logmean,
         logsd=expDist[expDist$mode=='pt',]$logsd) %>%
  mutate(pt_prop=plnorm(distance+250,logmean,logsd)-plnorm(distance-250,logmean,logsd)) %>%
  mutate(logmean=expDist[expDist$mode=='car',]$logmean,
         logsd=expDist[expDist$mode=='car',]$logsd) %>%
  mutate(car_prop=plnorm(distance+250,logmean,logsd)-plnorm(distance-250,logmean,logsd)) %>%
  dplyr::select(-logmean,-logsd) %>%
  as.data.frame()

  saveRDS(expectedDistances,paste0(outdir,"/","expectedDistances.rds"))
}

# distanceCounts<-readDistanceDistributions(outdir)
# reads in the currently used distance histograms

readDistanceDistributions <- function(outdir) {
  # outdir<-'../output/5.locate'
  distCountDir<-paste0(outdir,"/distanceCounts")
  
  distFiles<-list.files(distCountDir,pattern="*.rds",full.names=T)
  Sys.sleep(2)
  distanceCounts<-lapply(distFiles,readRDS) %>%
    bind_rows() %>%
    group_by(distance) %>%
    summarise(walk_count=sum(walk_count,na.rm=T),
              bike_count=sum(bike_count,na.rm=T),
              pt_count=sum(pt_count,na.rm=T),
              car_count=sum(car_count,na.rm=T)) %>%
    as.data.frame()
  return(distanceCounts)
}

readDestinationDistributions <- function(outdir) {
  # outdir<-'../output/5.locate'
  distCountDir<-paste0(outdir,"/destinationCounts")
  
  distFiles<-list.files(distCountDir,pattern="*.rds",full.names=T)
  Sys.sleep(2)
  destinationCounts<-lapply(distFiles,readRDS) %>%
    bind_rows() %>%
    group_by(sa3_code_2016) %>%
    summarise(commercial_count=sum(commercial_count,na.rm=T),
              education_count=sum(education_count,na.rm=T),
              park_count=sum(park_count,na.rm=T),
              work_count=sum(work_count,na.rm=T)) %>%
    as.data.frame()
  return(destinationCounts)
}
# EXAMPLES

# A dataframe of suitable homes for each SA1 along with the total number of 
# unique addresses.
#suitableHomes <- addresses %>%
#  filter(category=="home") %>%
#  group_by(sa1_main16) %>%
#  summarise(count=sum(count)) %>%
#  ungroup()

# Here, we're finding a commercial destination starting in SA1 20604112202
#test <- findLocation(20604112202,"commercial")

# We then try to find the probability of returning to this destination.
# Could do a loop with findLocation to iterate until this is > 1.
#returnProb <- getReturnProbability(20604112202,test[2],"home",test[1])

# Assign our locations coordinates. This will break if there's no addresses
# for your specified category within the SA1 region!!!
#originCoordinates <- getAddressCoordinates(20604112202,"home")
#destinationCoordinates <- getAddressCoordinates(test[2],"commercial")


runexample <- function() {
  distanceMatrixFile <- "../output/1.setup/locDistanceMatrix.rds"
  distanceMatrixIndexFile <- "../output/1.setup/locDistanceMatrixIndex.rds"
  sa1AttributedFile <- "../output/1.setup/locSa1Aattributed.rds"
  sa1CentroidsFile <- "../output/1.setup/locSa1Centroids.rds"
  addressesFile <- "../output/1.setup/locAddresses.rds"
  loadLocationsData(distanceMatrixFile, distanceMatrixIndexFile,
                    sa1AttributedFile, sa1CentroidsFile, addressesFile)
}
