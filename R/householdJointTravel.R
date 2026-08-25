normaliseVistaPurpose <- function(purpose) {
  purpose<-as.character(purpose)
  purpose[purpose%in%c("At Home","Go Home","Unknown Purpose (at start of day)",
                       "Home Morning","Home Daytime","Home Night")]<-"Home"
  purpose[purpose=="Work Related"]<-"Work"
  purpose[purpose=="Education"]<-"Study"
  purpose[purpose=="Buy Something"]<-"Shop"
  purpose[purpose=="Personal Business"]<-"Personal"
  purpose[purpose%in%c("Social","Recreational")]<-"Social/Recreational"
  purpose[purpose%in%c("Pick-up or Drop-off Someone",
                       "Pick-up or Deliver Something")]<-"Pickup/Dropoff/Deliver"
  purpose[purpose=="Accompany Someone"]<-"With Someone"
  purpose[purpose=="Change Mode"]<-"Mode Change"
  purpose[purpose%in%c("Other Purpose","Not Stated")]<-"Other"
  return(purpose)
}

readVistaCarRoleTrips <- function(setupDir,
                                  prefix="vista_2012_18_extracted_car_roles_weekday_") {
  files<-list.files(setupDir,paste0("^",prefix,"[0-9]+\\.csv$"),full.names=TRUE)
  if(length(files)==0) {
    stop(paste0("No VISTA car-role source files found in ",setupDir))
  }

  sourceTrips<-lapply(files,function(file) {
    groupId<-sub(paste0("^",prefix,"([0-9]+)\\.csv$"),"\\1",basename(file))
    sourceTripsForGroup<-read.csv(file,stringsAsFactors=FALSE,strip.white=TRUE)
    sourceTripsForGroup$GroupId<-as.integer(groupId)
    sourceTripsForGroup
  })
  return(do.call(rbind,sourceTrips))
}

assignVistaCarRoles <- function(plans,planGroups,sourceTrips,binSizeInMins=30,
                                timeToleranceBins=2,rseed=NULL) {
  requiredPlanColumns<-c("PlanId","AgentId","Activity","StartBin","EndBin",
                         "ArrivingMode")
  requiredGroupColumns<-c("PlanId","GroupId")
  requiredSourceColumns<-c("GroupId","VistaTripId","VistaPersonId",
                            "VistaHouseholdId","StartTime","ArrivalTime",
                            "OriginPurpose","DestinationPurpose","VistaCarRole",
                            "Weight")
  missingPlanColumns<-setdiff(requiredPlanColumns,colnames(plans))
  missingGroupColumns<-setdiff(requiredGroupColumns,colnames(planGroups))
  missingSourceColumns<-setdiff(requiredSourceColumns,colnames(sourceTrips))
  if(length(missingPlanColumns)>0) {
    stop(paste0("Generated plans are missing required columns: ",
                paste(missingPlanColumns,collapse=", ")))
  }
  if(length(missingGroupColumns)>0) {
    stop(paste0("Plan groups are missing required columns: ",
                paste(missingGroupColumns,collapse=", ")))
  }
  if(length(missingSourceColumns)>0) {
    stop(paste0("VISTA source trips are missing required columns: ",
                paste(missingSourceColumns,collapse=", ")))
  }
  if(anyDuplicated(planGroups$PlanId)>0) {
    stop("Each generated plan must have exactly one group")
  }
  missingPlanGroups<-setdiff(unique(plans$PlanId),planGroups$PlanId)
  if(length(missingPlanGroups)>0) {
    stop(paste0("Generated plans are missing group assignments: ",
                paste(missingPlanGroups,collapse=", ")))
  }
  if(!all(sourceTrips$VistaCarRole%in%c("driver","passenger"))) {
    stop("VISTA source trips must be labelled driver or passenger")
  }

  if(!is.null(rseed)) set.seed(rseed)

  plans$VistaCarRole<-NA_character_
  plans$VistaRoleSourceTripId<-NA_character_
  plans$VistaRoleSourcePersonId<-NA_character_
  plans$VistaRoleSourceHouseholdId<-NA_character_
  plans$VistaRoleSourceStartTime<-NA_real_
  plans$VistaRoleSourceArrivalTime<-NA_real_
  plans$VistaRoleMatchLevel<-NA_character_

  planGroupId<-planGroups$GroupId[match(plans$PlanId,planGroups$PlanId)]
  originPurpose<-rep(NA_character_,nrow(plans))
  departureBin<-rep(NA_real_,nrow(plans))
  if(nrow(plans)>1) {
    rows<-2:nrow(plans)
    samePlan<-plans$PlanId[rows]==plans$PlanId[rows-1] &
      plans$AgentId[rows]==plans$AgentId[rows-1]
    legRows<-rows[samePlan]
    originPurpose[legRows]<-plans$Activity[legRows-1]
    departureBin[legRows]<-plans$EndBin[legRows-1]
  }
  originPurpose<-normaliseVistaPurpose(originPurpose)
  destinationPurpose<-normaliseVistaPurpose(plans$Activity)

  sourceTrips$OriginPurposeGroup<-normaliseVistaPurpose(sourceTrips$OriginPurpose)
  sourceTrips$DestinationPurposeGroup<-normaliseVistaPurpose(sourceTrips$DestinationPurpose)
  sourceTrips$DepartureBin<-floor(as.numeric(sourceTrips$StartTime)/binSizeInMins)+1

  chooseSourceTrip<-function(candidateRows) {
    weights<-suppressWarnings(as.numeric(sourceTrips$Weight[candidateRows]))
    weights[is.na(weights) | weights<0]<-0
    if(sum(weights)==0) weights<-rep(1,length(candidateRows))
    candidateRows[sample(seq_along(candidateRows),1,prob=weights)]
  }

  carRows<-which(!is.na(plans$ArrivingMode) & plans$ArrivingMode=="car")
  for(row in carRows) {
    groupCandidates<-which(as.character(sourceTrips$GroupId)==as.character(planGroupId[row]))
    matchLevel<-"unmatched"
    candidateRows<-integer()

    if(length(groupCandidates)>0) {
      timeCompatible<-abs(sourceTrips$DepartureBin[groupCandidates]-departureBin[row])<=timeToleranceBins
      sameOrigin<-sourceTrips$OriginPurposeGroup[groupCandidates]==originPurpose[row]
      sameDestination<-sourceTrips$DestinationPurposeGroup[groupCandidates]==destinationPurpose[row]
      timeCompatible[is.na(timeCompatible)]<-FALSE
      sameOrigin[is.na(sameOrigin)]<-FALSE
      sameDestination[is.na(sameDestination)]<-FALSE

      candidateRows<-groupCandidates[timeCompatible & sameOrigin & sameDestination]
      matchLevel<-"purpose_pair_time"
      if(length(candidateRows)==0) {
        candidateRows<-groupCandidates[timeCompatible & sameDestination]
        matchLevel<-"destination_time"
      }
      if(length(candidateRows)==0) {
        candidateRows<-groupCandidates[sameDestination]
        matchLevel<-"destination"
      }
      if(length(candidateRows)==0) {
        candidateRows<-groupCandidates
        matchLevel<-"group"
      }
    }

    if(length(candidateRows)>0) {
      sourceRow<-chooseSourceTrip(candidateRows)
      plans$VistaCarRole[row]<-sourceTrips$VistaCarRole[sourceRow]
      plans$VistaRoleSourceTripId[row]<-sourceTrips$VistaTripId[sourceRow]
      plans$VistaRoleSourcePersonId[row]<-sourceTrips$VistaPersonId[sourceRow]
      plans$VistaRoleSourceHouseholdId[row]<-sourceTrips$VistaHouseholdId[sourceRow]
      plans$VistaRoleSourceStartTime[row]<-sourceTrips$StartTime[sourceRow]
      plans$VistaRoleSourceArrivalTime[row]<-sourceTrips$ArrivalTime[sourceRow]
      plans$VistaRoleMatchLevel[row]<-matchLevel
    } else {
      plans$VistaRoleMatchLevel[row]<-"unmatched"
    }
  }
  return(plans)
}

assignVistaCarRolesToPlanFile <- function(plancsv,planGroupCsv,setupDir,outcsv,
                                          binSizeInMins=30,timeToleranceBins=2,
                                          rseed=NULL) {
  plans<-read.csv(plancsv,stringsAsFactors=FALSE,strip.white=TRUE)
  planGroups<-read.csv(planGroupCsv,stringsAsFactors=FALSE,strip.white=TRUE)
  sourceTrips<-readVistaCarRoleTrips(setupDir)
  plans<-assignVistaCarRoles(plans,planGroups,sourceTrips,binSizeInMins,
                             timeToleranceBins,rseed)
  write.table(plans,file=outcsv,row.names=FALSE,sep=',')
  return(invisible(plans))
}

planTimeToSeconds <- function(time) {
  time<-as.character(time)
  vapply(time,function(value) {
    if(is.na(value) || !nzchar(value)) return(NA_real_)
    parts<-suppressWarnings(as.numeric(strsplit(value,":",fixed=TRUE)[[1]]))
    if(length(parts)!=3 || any(is.na(parts))) return(NA_real_)
    parts[1]*60*60+parts[2]*60+parts[3]
  },numeric(1),USE.NAMES=FALSE)
}

emptyHouseholdJointTravelCandidates <- function() {
  data.frame(
    CandidateId=character(),
    HouseholdId=character(),
    DriverAgentId=character(),
    DriverLegId=character(),
    PassengerAgentId=character(),
    PassengerLegId=character(),
    DriverDepartureTimeSeconds=numeric(),
    DriverArrivalTimeSeconds=numeric(),
    PassengerDepartureTimeSeconds=numeric(),
    PassengerArrivalTimeSeconds=numeric(),
    EstimatedPickupTimeSeconds=numeric(),
    EstimatedDropoffTimeSeconds=numeric(),
    PickupWindowStartSeconds=numeric(),
    PickupWindowEndSeconds=numeric(),
    DropoffWindowStartSeconds=numeric(),
    DropoffWindowEndSeconds=numeric(),
    PickupDistanceInMeters=numeric(),
    DropoffDistanceInMeters=numeric(),
    SharedRouteStartX=numeric(),
    SharedRouteStartY=numeric(),
    SharedRouteEndX=numeric(),
    SharedRouteEndY=numeric(),
    SharedRouteDistanceInMeters=numeric(),
    PassengerSeatsRequired=integer(),
    VehicleCapacityRequired=integer(),
    stringsAsFactors=FALSE
  )
}

getJointTravelLegs <- function(plans) {
  requiredColumns<-c("PlanId","AgentId","HouseholdId","LegId","ArrivingMode",
                     "VistaCarRole","x","y","act_start_hhmmss",
                     "act_end_hhmmss")
  missingColumns<-setdiff(requiredColumns,colnames(plans))
  if(length(missingColumns)>0) {
    stop(paste0("Generated plans are missing required joint-travel columns: ",
                paste(missingColumns,collapse=", ")))
  }

  roleRows<-which(!is.na(plans$VistaCarRole) & nzchar(plans$VistaCarRole))
  if(length(roleRows)==0) {
    return(data.frame())
  }
  if(!all(plans$VistaCarRole[roleRows]%in%c("driver","passenger"))) {
    stop("Generated car roles must be labelled driver or passenger")
  }
  if(!all(plans$ArrivingMode[roleRows]=="car")) {
    stop("Generated driver and passenger roles can only be attached to car legs")
  }
  if(any(roleRows==1)) {
    stop("A joint-travel role cannot be attached to the first activity")
  }
  previousRows<-roleRows-1
  samePerson<-plans$PlanId[roleRows]==plans$PlanId[previousRows] &
    plans$AgentId[roleRows]==plans$AgentId[previousRows]
  if(!all(samePerson)) {
    stop("Each role-labelled leg must follow an activity for the same person")
  }

  legs<-data.frame(
    HouseholdId=as.character(plans$HouseholdId[roleRows]),
    AgentId=as.character(plans$AgentId[roleRows]),
    LegId=as.character(plans$LegId[roleRows]),
    Role=as.character(plans$VistaCarRole[roleRows]),
    OriginX=suppressWarnings(as.numeric(plans$x[previousRows])),
    OriginY=suppressWarnings(as.numeric(plans$y[previousRows])),
    DestinationX=suppressWarnings(as.numeric(plans$x[roleRows])),
    DestinationY=suppressWarnings(as.numeric(plans$y[roleRows])),
    DepartureTimeSeconds=planTimeToSeconds(plans$act_end_hhmmss[previousRows]),
    ArrivalTimeSeconds=planTimeToSeconds(plans$act_start_hhmmss[roleRows]),
    stringsAsFactors=FALSE
  )
  requiredValues<-c("HouseholdId","AgentId","LegId","OriginX","OriginY",
                    "DestinationX","DestinationY","DepartureTimeSeconds",
                    "ArrivalTimeSeconds")
  incomplete<-!complete.cases(legs[,requiredValues]) |
    !nzchar(legs$HouseholdId) | !nzchar(legs$AgentId) | !nzchar(legs$LegId)
  if(any(incomplete)) {
    stop(paste0("Role-labelled car legs have missing household, location, time ",
                "or leg identifiers: ",paste(legs$LegId[incomplete],collapse=", ")))
  }
  if(any(legs$ArrivalTimeSeconds<legs$DepartureTimeSeconds)) {
    stop("Role-labelled car legs cannot arrive before they depart")
  }
  if(anyDuplicated(legs$LegId)>0) {
    stop("LegId must uniquely identify each role-labelled car leg")
  }
  return(legs)
}

findHouseholdJointTravelCandidates <- function(
    plans,maxTimeDifferenceInMins=30,routeToleranceInMeters=1000,
    minSharedDistanceInMeters=0) {
  if(length(maxTimeDifferenceInMins)!=1 || is.na(maxTimeDifferenceInMins) ||
     maxTimeDifferenceInMins<0) {
    stop("maxTimeDifferenceInMins must be one non-negative number")
  }
  if(length(routeToleranceInMeters)!=1 || is.na(routeToleranceInMeters) ||
     routeToleranceInMeters<0) {
    stop("routeToleranceInMeters must be one non-negative number")
  }
  if(length(minSharedDistanceInMeters)!=1 || is.na(minSharedDistanceInMeters) ||
     minSharedDistanceInMeters<0) {
    stop("minSharedDistanceInMeters must be one non-negative number")
  }

  legs<-getJointTravelLegs(plans)
  if(nrow(legs)==0) return(emptyHouseholdJointTravelCandidates())
  drivers<-legs[legs$Role=="driver",,drop=FALSE]
  passengers<-legs[legs$Role=="passenger",,drop=FALSE]
  if(nrow(drivers)==0 || nrow(passengers)==0) {
    return(emptyHouseholdJointTravelCandidates())
  }

  timeTolerance<-maxTimeDifferenceInMins*60
  matches<-list()
  matchCount<-0
  for(passengerRow in seq_len(nrow(passengers))) {
    passenger<-passengers[passengerRow,,drop=FALSE]
    possibleDrivers<-which(
      drivers$HouseholdId==passenger$HouseholdId &
        drivers$AgentId!=passenger$AgentId
    )
    for(driverRow in possibleDrivers) {
      driver<-drivers[driverRow,,drop=FALSE]
      routeX<-driver$DestinationX-driver$OriginX
      routeY<-driver$DestinationY-driver$OriginY
      routeLengthSquared<-routeX^2+routeY^2
      if(routeLengthSquared==0) next

      pickupFraction<-((passenger$OriginX-driver$OriginX)*routeX+
                         (passenger$OriginY-driver$OriginY)*routeY)/routeLengthSquared
      dropoffFraction<-((passenger$DestinationX-driver$OriginX)*routeX+
                          (passenger$DestinationY-driver$OriginY)*routeY)/routeLengthSquared
      if(pickupFraction<0 || dropoffFraction>1 || pickupFraction>dropoffFraction) next

      sharedStartX<-driver$OriginX+pickupFraction*routeX
      sharedStartY<-driver$OriginY+pickupFraction*routeY
      sharedEndX<-driver$OriginX+dropoffFraction*routeX
      sharedEndY<-driver$OriginY+dropoffFraction*routeY
      pickupDistance<-sqrt((passenger$OriginX-sharedStartX)^2+
                             (passenger$OriginY-sharedStartY)^2)
      dropoffDistance<-sqrt((passenger$DestinationX-sharedEndX)^2+
                              (passenger$DestinationY-sharedEndY)^2)
      sharedDistance<-(dropoffFraction-pickupFraction)*sqrt(routeLengthSquared)
      if(pickupDistance>routeToleranceInMeters ||
         dropoffDistance>routeToleranceInMeters ||
         sharedDistance<minSharedDistanceInMeters) next

      driverDuration<-driver$ArrivalTimeSeconds-driver$DepartureTimeSeconds
      estimatedPickup<-driver$DepartureTimeSeconds+pickupFraction*driverDuration
      estimatedDropoff<-driver$DepartureTimeSeconds+dropoffFraction*driverDuration
      if(abs(estimatedPickup-passenger$DepartureTimeSeconds)>timeTolerance ||
         abs(estimatedDropoff-passenger$ArrivalTimeSeconds)>timeTolerance) next

      matchCount<-matchCount+1
      matches[[matchCount]]<-data.frame(
        CandidateId=paste0("joint_travel_",matchCount),
        HouseholdId=passenger$HouseholdId,
        DriverAgentId=driver$AgentId,
        DriverLegId=driver$LegId,
        PassengerAgentId=passenger$AgentId,
        PassengerLegId=passenger$LegId,
        DriverDepartureTimeSeconds=driver$DepartureTimeSeconds,
        DriverArrivalTimeSeconds=driver$ArrivalTimeSeconds,
        PassengerDepartureTimeSeconds=passenger$DepartureTimeSeconds,
        PassengerArrivalTimeSeconds=passenger$ArrivalTimeSeconds,
        EstimatedPickupTimeSeconds=estimatedPickup,
        EstimatedDropoffTimeSeconds=estimatedDropoff,
        PickupWindowStartSeconds=max(estimatedPickup-timeTolerance,
                                     passenger$DepartureTimeSeconds-timeTolerance),
        PickupWindowEndSeconds=min(estimatedPickup+timeTolerance,
                                   passenger$DepartureTimeSeconds+timeTolerance),
        DropoffWindowStartSeconds=max(estimatedDropoff-timeTolerance,
                                      passenger$ArrivalTimeSeconds-timeTolerance),
        DropoffWindowEndSeconds=min(estimatedDropoff+timeTolerance,
                                    passenger$ArrivalTimeSeconds+timeTolerance),
        PickupDistanceInMeters=pickupDistance,
        DropoffDistanceInMeters=dropoffDistance,
        SharedRouteStartX=sharedStartX,
        SharedRouteStartY=sharedStartY,
        SharedRouteEndX=sharedEndX,
        SharedRouteEndY=sharedEndY,
        SharedRouteDistanceInMeters=sharedDistance,
        PassengerSeatsRequired=1L,
        VehicleCapacityRequired=2L,
        stringsAsFactors=FALSE
      )
    }
  }
  if(matchCount==0) return(emptyHouseholdJointTravelCandidates())
  return(do.call(rbind,matches))
}

writeHouseholdJointTravelCandidates <- function(
    plancsv,outcsv,maxTimeDifferenceInMins=30,routeToleranceInMeters=1000,
    minSharedDistanceInMeters=0) {
  plans<-read.csv(plancsv,stringsAsFactors=FALSE,strip.white=TRUE)
  candidates<-findHouseholdJointTravelCandidates(
    plans,maxTimeDifferenceInMins,routeToleranceInMeters,
    minSharedDistanceInMeters
  )
  write.table(candidates,file=outcsv,row.names=FALSE,sep=',')
  return(invisible(candidates))
}
