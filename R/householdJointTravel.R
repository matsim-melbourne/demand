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

getPassengerHouseholdDriverStatus <- function(plans) {
  requiredColumns<-c("AgentId","HouseholdId","VistaCarRole")
  missingColumns<-setdiff(requiredColumns,colnames(plans))
  if(length(missingColumns)>0) {
    stop(paste0("Role-labelled plans are missing required household columns: ",
                paste(missingColumns,collapse=", ")))
  }
  status<-rep(NA,nrow(plans))
  passengerRows<-which(!is.na(plans$VistaCarRole) &
                         plans$VistaCarRole=="passenger")
  if(length(passengerRows)==0) return(status)
  driverRows<-which(!is.na(plans$VistaCarRole) & plans$VistaCarRole=="driver")
  driverPairs<-unique(data.frame(
    HouseholdId=as.character(plans$HouseholdId[driverRows]),
    AgentId=as.character(plans$AgentId[driverRows]),
    stringsAsFactors=FALSE
  ))
  driverPeopleByHousehold<-table(driverPairs$HouseholdId)
  passengerHouseholds<-as.character(plans$HouseholdId[passengerRows])
  passengerAgents<-as.character(plans$AgentId[passengerRows])
  driverPeople<-as.integer(driverPeopleByHousehold[passengerHouseholds])
  driverPeople[is.na(driverPeople)]<-0L
  passengerIsAlsoDriver<-paste(passengerHouseholds,passengerAgents,sep="\034")%in%
    paste(driverPairs$HouseholdId,driverPairs$AgentId,sep="\034")
  status[passengerRows]<-
    driverPeople-as.integer(passengerIsAlsoDriver)>0
  return(status)
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
                         "ArrivingMode","HouseholdId")
  requiredGroupColumns<-c("PlanId","GroupId")
  requiredSourceColumns<-c("GroupId","VistaTripId","VistaPersonId",
                            "VistaHouseholdId","StartTime","ArrivalTime",
                            "OriginPurpose","DestinationPurpose","VistaCarRole",
                            "VistaHouseholdHasDriverTrip",
                            "VistaHouseholdHasOtherDriverTrip","Weight")
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
  passengerSourceRows<-sourceTrips$VistaCarRole=="passenger"
  if(any(is.na(sourceTrips$VistaHouseholdHasOtherDriverTrip[passengerSourceRows]))) {
    stop("VISTA passenger trips must record household driver context")
  }

  if(!is.null(rseed)) set.seed(rseed)

  plans$VistaCarRole<-NA_character_
  plans$VistaRoleSourceTripId<-NA_character_
  plans$VistaRoleSourcePersonId<-NA_character_
  plans$VistaRoleSourceHouseholdId<-NA_character_
  plans$VistaRoleSourceStartTime<-NA_real_
  plans$VistaRoleSourceArrivalTime<-NA_real_
  plans$VistaRoleMatchLevel<-NA_character_
  plans$VistaCarRoleInitial<-NA_character_
  plans$VistaInitialHouseholdDriverExpected<-NA
  plans$HouseholdCarRoleAction<-NA_character_
  plans$VistaRoleSourceHouseholdHasDriverTrip<-NA
  plans$VistaRoleSourceHouseholdHasOtherDriverTrip<-NA
  plans$VistaRoleSourceTravelDay<-NA_character_
  plans$VistaRoleSourceHouseholdCars<-NA_integer_
  plans$VistaRoleSourceHouseholdFourWheelDrives<-NA_integer_

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

  getIndexedRows<-function(index,key) {
    if(any(is.na(key))) return(integer())
    rows<-index[[paste(key,collapse="\034")]]
    if(is.null(rows)) integer() else rows
  }

  buildSourceIndexes<-function(sourceRows) {
    list(
      group=split(sourceRows,as.character(sourceTrips$GroupId[sourceRows])),
      destination=split(
        sourceRows,
        paste(sourceTrips$GroupId[sourceRows],
              sourceTrips$DestinationPurposeGroup[sourceRows],sep="\034")
      ),
      purposePair=split(
        sourceRows,
        paste(sourceTrips$GroupId[sourceRows],
              sourceTrips$OriginPurposeGroup[sourceRows],
              sourceTrips$DestinationPurposeGroup[sourceRows],sep="\034")
      )
    )
  }

  selectSourceTrip<-function(planRow,indexes) {
    groupKey<-as.character(planGroupId[planRow])
    groupCandidates<-indexes$group[[groupKey]]
    if(is.null(groupCandidates)) groupCandidates<-integer()
    if(length(groupCandidates)==0) {
      return(list(sourceRow=NA_integer_,matchLevel="unmatched"))
    }

    purposePairCandidates<-getIndexedRows(
      indexes$purposePair,
      c(groupKey,originPurpose[planRow],destinationPurpose[planRow])
    )
    timeCompatible<-abs(sourceTrips$DepartureBin[purposePairCandidates]-
                          departureBin[planRow])<=timeToleranceBins
    timeCompatible[is.na(timeCompatible)]<-FALSE
    candidateRows<-purposePairCandidates[timeCompatible]
    matchLevel<-"purpose_pair_time"

    destinationCandidates<-getIndexedRows(
      indexes$destination,c(groupKey,destinationPurpose[planRow])
    )
    if(length(candidateRows)==0) {
      timeCompatible<-abs(sourceTrips$DepartureBin[destinationCandidates]-
                            departureBin[planRow])<=timeToleranceBins
      timeCompatible[is.na(timeCompatible)]<-FALSE
      candidateRows<-destinationCandidates[timeCompatible]
      matchLevel<-"destination_time"
    }
    if(length(candidateRows)==0) {
      candidateRows<-destinationCandidates
      matchLevel<-"destination"
    }
    if(length(candidateRows)==0) {
      candidateRows<-groupCandidates
      matchLevel<-"group"
    }
    list(
      sourceRow=chooseSourceTrip(candidateRows),
      matchLevel=matchLevel
    )
  }

  allSourceIndexes<-buildSourceIndexes(seq_len(nrow(sourceTrips)))
  driverSourceIndexes<-buildSourceIndexes(
    which(sourceTrips$VistaCarRole=="driver")
  )
  externalPassengerSourceIndexes<-buildSourceIndexes(
    which(sourceTrips$VistaCarRole=="passenger" &
            !as.logical(sourceTrips$VistaHouseholdHasOtherDriverTrip))
  )

  carRows<-which(!is.na(plans$ArrivingMode) & plans$ArrivingMode=="car")
  selectedSourceRows<-rep(NA_integer_,length(carRows))
  selectedMatchLevels<-rep("unmatched",length(carRows))
  for(carIndex in seq_along(carRows)) {
    selection<-selectSourceTrip(carRows[carIndex],allSourceIndexes)
    if(!is.na(selection$sourceRow)) {
      selectedSourceRows[carIndex]<-selection$sourceRow
      selectedMatchLevels[carIndex]<-selection$matchLevel
    }
  }

  selectedRoles<-rep(NA_character_,length(carRows))
  matchedCarIndices<-which(!is.na(selectedSourceRows))
  selectedRoles[matchedCarIndices]<-
    sourceTrips$VistaCarRole[selectedSourceRows[matchedCarIndices]]
  initialRoles<-selectedRoles
  initialHouseholdDriverExpected<-rep(NA,length(carRows))
  initialPassengerIndices<-which(selectedRoles=="passenger")
  initialHouseholdDriverExpected[initialPassengerIndices]<-as.logical(
    sourceTrips$VistaHouseholdHasOtherDriverTrip[
      selectedSourceRows[initialPassengerIndices]
    ]
  )
  householdActions<-ifelse(is.na(selectedSourceRows),"unmatched","unchanged")

  matchedHouseholdIndices<-split(
    matchedCarIndices,
    as.character(plans$HouseholdId[carRows[matchedCarIndices]])
  )
  for(householdIndices in matchedHouseholdIndices) {
    repeat {
      householdRoles<-selectedRoles[householdIndices]
      passengerIndices<-householdIndices[householdRoles=="passenger"]
      if(length(passengerIndices)==0) break
      householdDriverExpected<-as.logical(
        sourceTrips$VistaHouseholdHasOtherDriverTrip[
          selectedSourceRows[passengerIndices]
        ]
      )
      passengerIndices<-passengerIndices[householdDriverExpected]
      if(length(passengerIndices)==0) break

      householdDriverIndices<-householdIndices[householdRoles=="driver"]
      driverAgents<-unique(as.character(
        plans$AgentId[carRows[householdDriverIndices]]
      ))
      passengerAgents<-as.character(plans$AgentId[carRows[passengerIndices]])
      hasOtherDriver<-vapply(
        passengerAgents,
        function(agent) any(driverAgents!=agent),
        logical(1)
      )
      unsupportedPassengerIndices<-passengerIndices[!hasOtherDriver]
      if(length(unsupportedPassengerIndices)==0) break

      passengerIndex<-unsupportedPassengerIndices[1]
      passengerAgent<-as.character(plans$AgentId[carRows[passengerIndex]])
      otherMemberIndices<-householdIndices[
        as.character(plans$AgentId[carRows[householdIndices]])!=passengerAgent
      ]
      driverAdded<-FALSE
      for(otherMemberIndex in otherMemberIndices) {
        selection<-selectSourceTrip(
          carRows[otherMemberIndex],driverSourceIndexes
        )
        if(!is.na(selection$sourceRow)) {
          selectedSourceRows[otherMemberIndex]<-selection$sourceRow
          selectedMatchLevels[otherMemberIndex]<-selection$matchLevel
          selectedRoles[otherMemberIndex]<-"driver"
          householdActions[otherMemberIndex]<-"household_driver_added"
          driverAdded<-TRUE
          break
        }
      }
      if(driverAdded) next

      selection<-selectSourceTrip(
        carRows[passengerIndex],externalPassengerSourceIndexes
      )
      if(!is.na(selection$sourceRow)) {
        selectedSourceRows[passengerIndex]<-selection$sourceRow
        selectedMatchLevels[passengerIndex]<-selection$matchLevel
        selectedRoles[passengerIndex]<-"passenger"
        householdActions[passengerIndex]<-"external_passenger_substituted"
        next
      }

      selection<-selectSourceTrip(carRows[passengerIndex],driverSourceIndexes)
      if(!is.na(selection$sourceRow)) {
        selectedSourceRows[passengerIndex]<-selection$sourceRow
        selectedMatchLevels[passengerIndex]<-selection$matchLevel
        selectedRoles[passengerIndex]<-"driver"
        householdActions[passengerIndex]<-"passenger_reassigned_driver"
        next
      }
      stop(paste0(
        "Could not satisfy VISTA household driver context for generated plan ",
        plans$PlanId[carRows[passengerIndex]]
      ))
    }
  }

  matchedCarIndices<-which(!is.na(selectedSourceRows))
  matchedPlanRows<-carRows[matchedCarIndices]
  matchedSourceRows<-selectedSourceRows[matchedCarIndices]
  plans$VistaCarRole[matchedPlanRows]<-sourceTrips$VistaCarRole[matchedSourceRows]
  plans$VistaRoleSourceTripId[matchedPlanRows]<-sourceTrips$VistaTripId[matchedSourceRows]
  plans$VistaRoleSourcePersonId[matchedPlanRows]<-sourceTrips$VistaPersonId[matchedSourceRows]
  plans$VistaRoleSourceHouseholdId[matchedPlanRows]<-
    sourceTrips$VistaHouseholdId[matchedSourceRows]
  plans$VistaRoleSourceStartTime[matchedPlanRows]<-sourceTrips$StartTime[matchedSourceRows]
  plans$VistaRoleSourceArrivalTime[matchedPlanRows]<-
    sourceTrips$ArrivalTime[matchedSourceRows]
  plans$VistaRoleMatchLevel[carRows]<-selectedMatchLevels
  plans$VistaCarRoleInitial[carRows]<-initialRoles
  plans$VistaInitialHouseholdDriverExpected[carRows]<-
    initialHouseholdDriverExpected
  plans$HouseholdCarRoleAction[carRows]<-householdActions
  plans$VistaRoleSourceHouseholdHasDriverTrip[matchedPlanRows]<-as.logical(
    sourceTrips$VistaHouseholdHasDriverTrip[matchedSourceRows]
  )
  plans$VistaRoleSourceHouseholdHasOtherDriverTrip[matchedPlanRows]<-as.logical(
    sourceTrips$VistaHouseholdHasOtherDriverTrip[matchedSourceRows]
  )
  optionalSourceColumns<-c(
    VistaRoleSourceTravelDay="VistaTravelDay",
    VistaRoleSourceHouseholdCars="VistaHouseholdCars",
    VistaRoleSourceHouseholdFourWheelDrives="VistaHouseholdFourWheelDrives"
  )
  for(planColumn in names(optionalSourceColumns)) {
    sourceColumn<-optionalSourceColumns[[planColumn]]
    if(sourceColumn%in%colnames(sourceTrips)) {
      plans[[planColumn]][matchedPlanRows]<-sourceTrips[[sourceColumn]][matchedSourceRows]
    }
  }

  passengerDriverStatus<-getPassengerHouseholdDriverStatus(plans)
  internalPassengerRows<-which(
    plans$VistaCarRole=="passenger" &
      plans$VistaRoleSourceHouseholdHasOtherDriverTrip%in%TRUE
  )
  if(any(!passengerDriverStatus[internalPassengerRows])) {
    stop("Household driver constraint was not satisfied for all passenger legs")
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
  seconds<-rep(NA_real_,length(time))
  populated<-which(!is.na(time) & nzchar(time))
  if(length(populated)==0) return(seconds)
  parts<-strsplit(time[populated],":",fixed=TRUE)
  valid<-lengths(parts)==3
  if(!any(valid)) return(seconds)
  validParts<-matrix(
    suppressWarnings(as.numeric(unlist(parts[valid],use.names=FALSE))),
    ncol=3,byrow=TRUE
  )
  complete<-complete.cases(validParts)
  converted<-rep(NA_real_,nrow(validParts))
  converted[complete]<-validParts[complete,1]*60*60+
    validParts[complete,2]*60+validParts[complete,3]
  seconds[populated[valid]]<-converted
  return(seconds)
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
    DriverLegPassengerOptions=integer(),
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
  driverRowsByHousehold<-split(seq_len(nrow(drivers)),drivers$HouseholdId)
  matches<-list()
  matchCount<-0
  for(passengerRow in seq_len(nrow(passengers))) {
    passengerHousehold<-passengers$HouseholdId[passengerRow]
    passengerAgent<-passengers$AgentId[passengerRow]
    possibleDrivers<-driverRowsByHousehold[[passengerHousehold]]
    if(is.null(possibleDrivers)) possibleDrivers<-integer()
    possibleDrivers<-possibleDrivers[
      drivers$AgentId[possibleDrivers]!=passengerAgent
    ]
    if(length(possibleDrivers)==0) next

    routeX<-drivers$DestinationX[possibleDrivers]-drivers$OriginX[possibleDrivers]
    routeY<-drivers$DestinationY[possibleDrivers]-drivers$OriginY[possibleDrivers]
    routeLengthSquared<-routeX^2+routeY^2
    usableRoute<-routeLengthSquared>0
    if(!any(usableRoute)) next
    possibleDrivers<-possibleDrivers[usableRoute]
    routeX<-routeX[usableRoute]
    routeY<-routeY[usableRoute]
    routeLengthSquared<-routeLengthSquared[usableRoute]

    pickupFraction<-((passengers$OriginX[passengerRow]-
                        drivers$OriginX[possibleDrivers])*routeX+
                       (passengers$OriginY[passengerRow]-
                          drivers$OriginY[possibleDrivers])*routeY)/
      routeLengthSquared
    dropoffFraction<-((passengers$DestinationX[passengerRow]-
                         drivers$OriginX[possibleDrivers])*routeX+
                        (passengers$DestinationY[passengerRow]-
                           drivers$OriginY[possibleDrivers])*routeY)/
      routeLengthSquared
    correctDirection<-pickupFraction>=0 & dropoffFraction<=1 &
      pickupFraction<=dropoffFraction
    if(!any(correctDirection)) next
    possibleDrivers<-possibleDrivers[correctDirection]
    routeX<-routeX[correctDirection]
    routeY<-routeY[correctDirection]
    routeLengthSquared<-routeLengthSquared[correctDirection]
    pickupFraction<-pickupFraction[correctDirection]
    dropoffFraction<-dropoffFraction[correctDirection]

    sharedStartX<-drivers$OriginX[possibleDrivers]+pickupFraction*routeX
    sharedStartY<-drivers$OriginY[possibleDrivers]+pickupFraction*routeY
    sharedEndX<-drivers$OriginX[possibleDrivers]+dropoffFraction*routeX
    sharedEndY<-drivers$OriginY[possibleDrivers]+dropoffFraction*routeY
    pickupDistance<-sqrt((passengers$OriginX[passengerRow]-sharedStartX)^2+
                           (passengers$OriginY[passengerRow]-sharedStartY)^2)
    dropoffDistance<-sqrt((passengers$DestinationX[passengerRow]-sharedEndX)^2+
                            (passengers$DestinationY[passengerRow]-sharedEndY)^2)
    sharedDistance<-(dropoffFraction-pickupFraction)*sqrt(routeLengthSquared)
    spatiallyCompatible<-pickupDistance<=routeToleranceInMeters &
      dropoffDistance<=routeToleranceInMeters &
      sharedDistance>=minSharedDistanceInMeters
    if(!any(spatiallyCompatible)) next
    possibleDrivers<-possibleDrivers[spatiallyCompatible]
    pickupFraction<-pickupFraction[spatiallyCompatible]
    dropoffFraction<-dropoffFraction[spatiallyCompatible]
    pickupDistance<-pickupDistance[spatiallyCompatible]
    dropoffDistance<-dropoffDistance[spatiallyCompatible]
    sharedDistance<-sharedDistance[spatiallyCompatible]
    sharedStartX<-sharedStartX[spatiallyCompatible]
    sharedStartY<-sharedStartY[spatiallyCompatible]
    sharedEndX<-sharedEndX[spatiallyCompatible]
    sharedEndY<-sharedEndY[spatiallyCompatible]

    driverDuration<-drivers$ArrivalTimeSeconds[possibleDrivers]-
      drivers$DepartureTimeSeconds[possibleDrivers]
    estimatedPickup<-drivers$DepartureTimeSeconds[possibleDrivers]+
      pickupFraction*driverDuration
    estimatedDropoff<-drivers$DepartureTimeSeconds[possibleDrivers]+
      dropoffFraction*driverDuration
    temporallyCompatible<-
      abs(estimatedPickup-passengers$DepartureTimeSeconds[passengerRow])<=
        timeTolerance &
      abs(estimatedDropoff-passengers$ArrivalTimeSeconds[passengerRow])<=
        timeTolerance
    if(!any(temporallyCompatible)) next
    possibleDrivers<-possibleDrivers[temporallyCompatible]
    estimatedPickup<-estimatedPickup[temporallyCompatible]
    estimatedDropoff<-estimatedDropoff[temporallyCompatible]
    pickupDistance<-pickupDistance[temporallyCompatible]
    dropoffDistance<-dropoffDistance[temporallyCompatible]
    sharedDistance<-sharedDistance[temporallyCompatible]
    sharedStartX<-sharedStartX[temporallyCompatible]
    sharedStartY<-sharedStartY[temporallyCompatible]
    sharedEndX<-sharedEndX[temporallyCompatible]
    sharedEndY<-sharedEndY[temporallyCompatible]

    newMatchCount<-length(possibleDrivers)
    matchIds<-matchCount+seq_len(newMatchCount)
    matches[[length(matches)+1]]<-data.frame(
      CandidateId=paste0("joint_travel_",matchIds),
      HouseholdId=passengerHousehold,
      DriverAgentId=drivers$AgentId[possibleDrivers],
      DriverLegId=drivers$LegId[possibleDrivers],
      PassengerAgentId=passengerAgent,
      PassengerLegId=passengers$LegId[passengerRow],
      DriverDepartureTimeSeconds=
        drivers$DepartureTimeSeconds[possibleDrivers],
      DriverArrivalTimeSeconds=drivers$ArrivalTimeSeconds[possibleDrivers],
      PassengerDepartureTimeSeconds=
        passengers$DepartureTimeSeconds[passengerRow],
      PassengerArrivalTimeSeconds=passengers$ArrivalTimeSeconds[passengerRow],
      EstimatedPickupTimeSeconds=estimatedPickup,
      EstimatedDropoffTimeSeconds=estimatedDropoff,
      PickupWindowStartSeconds=pmax(
        estimatedPickup-timeTolerance,
        passengers$DepartureTimeSeconds[passengerRow]-timeTolerance
      ),
      PickupWindowEndSeconds=pmin(
        estimatedPickup+timeTolerance,
        passengers$DepartureTimeSeconds[passengerRow]+timeTolerance
      ),
      DropoffWindowStartSeconds=pmax(
        estimatedDropoff-timeTolerance,
        passengers$ArrivalTimeSeconds[passengerRow]-timeTolerance
      ),
      DropoffWindowEndSeconds=pmin(
        estimatedDropoff+timeTolerance,
        passengers$ArrivalTimeSeconds[passengerRow]+timeTolerance
      ),
      PickupDistanceInMeters=pickupDistance,
      DropoffDistanceInMeters=dropoffDistance,
      SharedRouteStartX=sharedStartX,
      SharedRouteStartY=sharedStartY,
      SharedRouteEndX=sharedEndX,
      SharedRouteEndY=sharedEndY,
      SharedRouteDistanceInMeters=sharedDistance,
      DriverLegPassengerOptions=NA_integer_,
      PassengerSeatsRequired=1L,
      VehicleCapacityRequired=NA_integer_,
      stringsAsFactors=FALSE
    )
    matchCount<-matchCount+newMatchCount
  }
  if(matchCount==0) return(emptyHouseholdJointTravelCandidates())
  candidates<-do.call(rbind,matches)
  # A driver leg can appear against several passenger legs, so the capacity it
  # would need is counted once per driver leg rather than once per pairing.
  passengerOptions<-tapply(
    candidates$PassengerLegId,candidates$DriverLegId,
    function(passengerLegIds) length(unique(passengerLegIds))
  )
  candidates$DriverLegPassengerOptions<-
    as.integer(passengerOptions[candidates$DriverLegId])
  candidates$VehicleCapacityRequired<-
    candidates$DriverLegPassengerOptions+1L
  return(candidates)
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
