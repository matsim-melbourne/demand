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
