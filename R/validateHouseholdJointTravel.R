getHouseholdJointTravelSummary <- function(plans,candidates) {
  requiredPlanColumns<-c("AgentId","HouseholdId","LegId","VistaCarRole",
                         "VistaCarRoleInitial",
                         "VistaRoleSourceHouseholdHasOtherDriverTrip",
                         "HouseholdCarRoleAction")
  missingPlanColumns<-setdiff(requiredPlanColumns,colnames(plans))
  if(length(missingPlanColumns)>0) {
    stop(paste0("Role-labelled plans are missing required columns: ",
                paste(missingPlanColumns,collapse=", ")))
  }
  requiredCandidateColumns<-c("CandidateId","HouseholdId","DriverLegId",
                              "PassengerLegId")
  missingCandidateColumns<-setdiff(requiredCandidateColumns,colnames(candidates))
  if(length(missingCandidateColumns)>0) {
    stop(paste0("Joint-travel candidates are missing required columns: ",
                paste(missingCandidateColumns,collapse=", ")))
  }

  driverLegs<-unique(plans$LegId[plans$VistaCarRole=="driver" &
                                  !is.na(plans$VistaCarRole)])
  passengerLegs<-unique(plans$LegId[plans$VistaCarRole=="passenger" &
                                     !is.na(plans$VistaCarRole)])
  matchedDriverLegs<-unique(candidates$DriverLegId)
  matchedPassengerLegs<-unique(candidates$PassengerLegId)
  householdsWithCarRoles<-unique(plans$HouseholdId[
    !is.na(plans$VistaCarRole) & plans$VistaCarRole%in%c("driver","passenger")
  ])
  householdsWithCandidates<-unique(candidates$HouseholdId)
  passengerCoverage<-if(length(passengerLegs)==0) NA_real_ else
    100*length(matchedPassengerLegs)/length(passengerLegs)
  driverCoverage<-if(length(driverLegs)==0) NA_real_ else
    100*length(matchedDriverLegs)/length(driverLegs)
  passengerRows<-which(!is.na(plans$VistaCarRole) &
                         plans$VistaCarRole=="passenger")
  passengerHouseholdDriverStatus<-getPassengerHouseholdDriverStatus(plans)
  passengerLegsWithHouseholdDriver<-
    sum(passengerHouseholdDriverStatus[passengerRows],na.rm=TRUE)
  passengerHouseholdDriverCoverage<-if(length(passengerRows)==0) NA_real_ else
    100*passengerLegsWithHouseholdDriver/length(passengerRows)
  expectedPassengerRows<-passengerRows[
    plans$VistaRoleSourceHouseholdHasOtherDriverTrip[passengerRows]%in%TRUE
  ]
  supportedExpectedPassengerLegs<-
    sum(passengerHouseholdDriverStatus[expectedPassengerRows],na.rm=TRUE)
  expectedPassengerCoverage<-if(length(expectedPassengerRows)==0) NA_real_ else
    100*supportedExpectedPassengerLegs/length(expectedPassengerRows)
  externalPassengerLegs<-sum(
    plans$VistaRoleSourceHouseholdHasOtherDriverTrip[passengerRows]%in%FALSE
  )

  initialPlans<-plans
  initialPlans$VistaCarRole<-initialPlans$VistaCarRoleInitial
  initialPassengerRows<-which(!is.na(initialPlans$VistaCarRole) &
                                initialPlans$VistaCarRole=="passenger")
  initialPassengerHouseholdDriverStatus<-
    getPassengerHouseholdDriverStatus(initialPlans)
  initialPassengerHouseholdDriverCoverage<-
    if(length(initialPassengerRows)==0) NA_real_ else
      100*sum(initialPassengerHouseholdDriverStatus[initialPassengerRows],
              na.rm=TRUE)/length(initialPassengerRows)

  householdDriverAddedLegs<-sum(
    plans$HouseholdCarRoleAction=="household_driver_added",na.rm=TRUE
  )
  externalPassengerSubstitutions<-sum(
    plans$HouseholdCarRoleAction=="external_passenger_substituted",na.rm=TRUE
  )
  passengerReassignedDriverLegs<-sum(
    plans$HouseholdCarRoleAction=="passenger_reassigned_driver",na.rm=TRUE
  )

  data.frame(
    Metric=c(
      "households_with_car_roles",
      "households_with_candidates",
      "driver_legs",
      "driver_legs_with_passenger_options",
      "driver_leg_coverage_percent",
      "passenger_legs",
      "initial_passenger_household_driver_percent",
      "passenger_legs_with_other_household_driver",
      "passenger_household_driver_percent",
      "household_driver_expected_passenger_legs",
      "household_driver_expected_passenger_legs_supported",
      "household_driver_expected_support_percent",
      "external_driver_passenger_legs",
      "household_driver_added_legs",
      "external_passenger_substituted_legs",
      "passenger_reassigned_driver_legs",
      "passenger_legs_with_driver_options",
      "passenger_leg_coverage_percent",
      "candidate_pairs"
    ),
    Value=c(
      length(householdsWithCarRoles),
      length(householdsWithCandidates),
      length(driverLegs),
      length(matchedDriverLegs),
      driverCoverage,
      length(passengerLegs),
      initialPassengerHouseholdDriverCoverage,
      passengerLegsWithHouseholdDriver,
      passengerHouseholdDriverCoverage,
      length(expectedPassengerRows),
      supportedExpectedPassengerLegs,
      expectedPassengerCoverage,
      externalPassengerLegs,
      householdDriverAddedLegs,
      externalPassengerSubstitutions,
      passengerReassignedDriverLegs,
      length(matchedPassengerLegs),
      passengerCoverage,
      nrow(candidates)
    ),
    stringsAsFactors=FALSE
  )
}

getVistaPassengerHouseholdDriverSummary <- function(sourceTrips) {
  requiredColumns<-c("GroupId","VistaTripId","VistaCarRole",
                     "VistaHouseholdHasOtherDriverTrip","Weight")
  missingColumns<-setdiff(requiredColumns,colnames(sourceTrips))
  if(length(missingColumns)>0) {
    stop(paste0("VISTA source trips are missing required columns: ",
                paste(missingColumns,collapse=", ")))
  }
  passengerTrips<-sourceTrips[
    sourceTrips$VistaCarRole=="passenger",requiredColumns,drop=FALSE
  ]
  if(anyDuplicated(passengerTrips$VistaTripId)>0) {
    stop("Each VISTA passenger trip must appear in exactly one plan group")
  }
  passengerTrips$Weight<-suppressWarnings(as.numeric(passengerTrips$Weight))
  passengerTrips$Weight[
    is.na(passengerTrips$Weight) | passengerTrips$Weight<0
  ]<-0

  summariseRows<-function(rows,groupId) {
    passengerWeight<-sum(passengerTrips$Weight[rows])
    householdDriverWeight<-sum(
      passengerTrips$Weight[rows]*
        as.integer(passengerTrips$VistaHouseholdHasOtherDriverTrip[rows]%in%TRUE)
    )
    data.frame(
      GroupId=as.character(groupId),
      PassengerTrips=length(rows),
      PassengerTripWeight=passengerWeight,
      PassengerTripWeightWithOtherHouseholdDriver=householdDriverWeight,
      PassengerHouseholdDriverPercent=if(passengerWeight==0) NA_real_ else
        100*householdDriverWeight/passengerWeight,
      stringsAsFactors=FALSE
    )
  }
  groupRows<-split(seq_len(nrow(passengerTrips)),passengerTrips$GroupId)
  groupSummary<-do.call(
    rbind,
    lapply(names(groupRows),function(groupId) {
      summariseRows(groupRows[[groupId]],groupId)
    })
  )
  rbind(summariseRows(seq_len(nrow(passengerTrips)),"Overall"),groupSummary)
}

saveHouseholdJointTravelPlots <- function(plans,candidates,outdir,
                                          vistaSummary=NULL) {
  if(!requireNamespace("ggplot2",quietly=TRUE)) {
    stop("The ggplot2 package is required to create validation plots")
  }
  if(!requireNamespace("scales",quietly=TRUE)) {
    stop("The scales package is required to create validation plots")
  }
  dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
  plotTheme<-ggplot2::theme_minimal(base_size=13)+
    ggplot2::theme(
      panel.grid.minor=ggplot2::element_blank(),
      plot.title.position="plot"
    )

  roles<-plans$VistaCarRole[!is.na(plans$VistaCarRole)]
  roleCounts<-as.data.frame(table(roles),stringsAsFactors=FALSE)
  colnames(roleCounts)<-c("Role","Legs")
  roleCounts$Role<-factor(roleCounts$Role,levels=c("driver","passenger"),
                          labels=c("Driver","Passenger"))
  rolePlot<-ggplot2::ggplot(roleCounts,ggplot2::aes(x=Role,y=Legs,fill=Role))+
    ggplot2::geom_col(width=0.65,show.legend=FALSE)+
    ggplot2::geom_text(ggplot2::aes(label=scales::comma(Legs)),vjust=-0.4)+
    ggplot2::scale_fill_manual(values=c("Driver"="#2C7FB8","Passenger"="#F28E2B"))+
    ggplot2::scale_y_continuous(labels=scales::comma,expand=ggplot2::expansion(mult=c(0,0.1)))+
    ggplot2::labs(title="Generated car roles",x=NULL,y="Car legs")+
    plotTheme
  ggplot2::ggsave(file.path(outdir,"car-role-legs.png"),rolePlot,
                  width=7,height=5,dpi=160)

  structuralSummary<-getHouseholdJointTravelSummary(plans,candidates)
  structuralValues<-setNames(
    structuralSummary$Value,structuralSummary$Metric
  )
  contextData<-data.frame(
    Stage=c("Before household constraint","After household constraint"),
    Percent=c(
      structuralValues[["initial_passenger_household_driver_percent"]],
      structuralValues[["passenger_household_driver_percent"]]
    ),
    stringsAsFactors=FALSE
  )
  if(!is.null(vistaSummary)) {
    vistaOverall<-vistaSummary[vistaSummary$GroupId=="Overall",,drop=FALSE]
    contextData<-rbind(
      contextData[1,,drop=FALSE],
      data.frame(
        Stage="VISTA weighted target",
        Percent=vistaOverall$PassengerHouseholdDriverPercent,
        stringsAsFactors=FALSE
      ),
      contextData[2,,drop=FALSE]
    )
  }
  contextData$Stage<-factor(contextData$Stage,levels=contextData$Stage)
  contextPlot<-ggplot2::ggplot(
    contextData,ggplot2::aes(x=Stage,y=Percent,fill=Stage)
  )+
    ggplot2::geom_col(width=0.65,show.legend=FALSE)+
    ggplot2::geom_text(
      ggplot2::aes(label=paste0(round(Percent,1),"%")),vjust=-0.4
    )+
    ggplot2::scale_fill_manual(values=c("#7F7F7F","#59A14F","#2C7FB8"))+
    ggplot2::scale_y_continuous(
      limits=c(0,100),breaks=seq(0,100,20),
      labels=function(x) paste0(x,"%"),
      expand=ggplot2::expansion(mult=c(0,0.04))
    )+
    ggplot2::labs(
      title="Passenger legs with another household member driving",
      x=NULL,y=NULL
    )+
    plotTheme+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=15,hjust=1))
  ggplot2::ggsave(
    file.path(outdir,"passenger-household-driver-context.png"),
    contextPlot,width=8,height=5,dpi=160
  )

  actionLevels<-c("household_driver_added","external_passenger_substituted",
                  "passenger_reassigned_driver")
  actionLabels<-c("Household driver added","External passenger retained",
                  "Passenger reassigned as driver")
  actionCounts<-data.frame(
    Action=factor(actionLabels,levels=actionLabels),
    Legs=vapply(
      actionLevels,
      function(action) sum(plans$HouseholdCarRoleAction==action,na.rm=TRUE),
      integer(1)
    )
  )
  actionPlot<-ggplot2::ggplot(
    actionCounts,ggplot2::aes(x=Action,y=Legs,fill=Action)
  )+
    ggplot2::geom_col(width=0.65,show.legend=FALSE)+
    ggplot2::geom_text(
      ggplot2::aes(label=scales::comma(Legs)),vjust=-0.4
    )+
    ggplot2::scale_fill_manual(values=c("#2C7FB8","#F28E2B","#D73027"))+
    ggplot2::scale_y_continuous(
      labels=scales::comma,expand=ggplot2::expansion(mult=c(0,0.1))
    )+
    ggplot2::labs(title="Household role adjustments",x=NULL,y="Car legs")+
    plotTheme+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=15,hjust=1))
  ggplot2::ggsave(
    file.path(outdir,"household-role-adjustments.png"),
    actionPlot,width=8,height=5,dpi=160
  )

  passengerLegs<-unique(plans$LegId[!is.na(plans$VistaCarRole) &
                                      plans$VistaCarRole=="passenger"])
  passengerOptions<-data.frame(PassengerLegId=passengerLegs,
                               PossibleDrivers=0L,stringsAsFactors=FALSE)
  if(nrow(candidates)>0) {
    optionCounts<-table(candidates$PassengerLegId)
    passengerOptions$PossibleDrivers<-as.integer(optionCounts[
      match(passengerOptions$PassengerLegId,names(optionCounts))
    ])
    passengerOptions$PossibleDrivers[is.na(passengerOptions$PossibleDrivers)]<-0L
  }
  passengerOptions$Options<-cut(
    passengerOptions$PossibleDrivers,
    breaks=c(-Inf,0,1,2,Inf),
    labels=c("No feasible driver","One driver","Two drivers","Three or more")
  )
  coverageCounts<-as.data.frame(table(passengerOptions$Options),stringsAsFactors=FALSE)
  colnames(coverageCounts)<-c("Options","PassengerLegs")
  coverageCounts$Options<-factor(
    coverageCounts$Options,levels=levels(passengerOptions$Options)
  )
  coveragePlot<-ggplot2::ggplot(
    coverageCounts,ggplot2::aes(x=Options,y=PassengerLegs,fill=Options)
  )+
    ggplot2::geom_col(width=0.7,show.legend=FALSE)+
    ggplot2::geom_text(ggplot2::aes(label=scales::comma(PassengerLegs)),vjust=-0.4)+
    ggplot2::scale_fill_manual(values=c("#D73027","#91CF60","#1A9850","#006837"))+
    ggplot2::scale_y_continuous(labels=scales::comma,expand=ggplot2::expansion(mult=c(0,0.1)))+
    ggplot2::labs(title="Driver options for passenger legs",x=NULL,y="Passenger legs")+
    plotTheme+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=20,hjust=1))
  ggplot2::ggsave(file.path(outdir,"passenger-driver-options.png"),coveragePlot,
                  width=8,height=5,dpi=160)

  driverLegs<-unique(plans$LegId[!is.na(plans$VistaCarRole) &
                                   plans$VistaCarRole=="driver"])
  driverOptions<-data.frame(DriverLegId=driverLegs,
                            PossiblePassengers=0L,stringsAsFactors=FALSE)
  if(nrow(candidates)>0) {
    passengerCounts<-tapply(candidates$PassengerLegId,candidates$DriverLegId,
                            function(x) length(unique(x)))
    driverOptions$PossiblePassengers<-as.integer(passengerCounts[
      match(driverOptions$DriverLegId,names(passengerCounts))
    ])
    driverOptions$PossiblePassengers[is.na(driverOptions$PossiblePassengers)]<-0L
  }
  driverOptions$Options<-cut(
    driverOptions$PossiblePassengers,
    breaks=c(-Inf,0,1,2,Inf),
    labels=c("No feasible passenger","One passenger","Two passengers",
             "Three or more")
  )
  driverCoverageCounts<-as.data.frame(
    table(driverOptions$Options),stringsAsFactors=FALSE
  )
  colnames(driverCoverageCounts)<-c("Options","DriverLegs")
  driverCoverageCounts$Options<-factor(
    driverCoverageCounts$Options,levels=levels(driverOptions$Options)
  )
  driverPlot<-ggplot2::ggplot(
    driverCoverageCounts,ggplot2::aes(x=Options,y=DriverLegs,fill=Options)
  )+
    ggplot2::geom_col(width=0.7,show.legend=FALSE)+
    ggplot2::geom_text(ggplot2::aes(label=scales::comma(DriverLegs)),vjust=-0.4)+
    ggplot2::scale_fill_manual(values=c("#D73027","#91CF60","#1A9850",
                                        "#006837"))+
    ggplot2::scale_y_continuous(
      labels=scales::comma,expand=ggplot2::expansion(mult=c(0,0.1))
    )+
    ggplot2::labs(title="Passenger options for driver legs",
                  x=NULL,y="Driver legs")+
    plotTheme+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=20,hjust=1))
  ggplot2::ggsave(file.path(outdir,"driver-passenger-options.png"),driverPlot,
                  width=8,height=5,dpi=160)

  if(nrow(candidates)>0) {
    candidatePlotRows<-seq_len(nrow(candidates))
    if(length(candidatePlotRows)>100000) {
      candidatePlotRows<-unique(round(seq(1,nrow(candidates),length.out=100000)))
    }
    candidatePlotData<-candidates[candidatePlotRows,,drop=FALSE]
    plotSubtitle<-if(nrow(candidatePlotData)<nrow(candidates)) {
      paste0("Showing ",scales::comma(nrow(candidatePlotData))," of ",
             scales::comma(nrow(candidates))," candidate pairs")
    } else NULL
    compatibilityPlot<-ggplot2::ggplot(
      candidatePlotData,
      ggplot2::aes(x=PickupDistanceInMeters,y=DropoffDistanceInMeters,
                   colour=SharedRouteDistanceInMeters/1000)
    )+
      ggplot2::geom_point(alpha=0.35,size=1.5)+
      ggplot2::scale_colour_viridis_c(name="Shared route\n(km)")+
      ggplot2::scale_x_continuous(labels=scales::comma)+
      ggplot2::scale_y_continuous(labels=scales::comma)+
      ggplot2::coord_equal()+
      ggplot2::labs(title="Spatial compatibility of candidate pairs",
                    subtitle=plotSubtitle,
                    x="Pickup distance from driver route (m)",
                    y="Drop-off distance from driver route (m)")+
      plotTheme
    ggplot2::ggsave(file.path(outdir,"candidate-spatial-compatibility.png"),
                    compatibilityPlot,width=7,height=6,dpi=160)

    timing<-rbind(
      data.frame(Stage="Pickup",
                 DifferenceInMins=abs(candidatePlotData$EstimatedPickupTimeSeconds-
                                        candidatePlotData$PassengerDepartureTimeSeconds)/60),
      data.frame(Stage="Drop-off",
                 DifferenceInMins=abs(candidatePlotData$EstimatedDropoffTimeSeconds-
                                        candidatePlotData$PassengerArrivalTimeSeconds)/60)
    )
    timingPlot<-ggplot2::ggplot(
      timing,ggplot2::aes(x=DifferenceInMins,fill=Stage)
    )+
      ggplot2::geom_histogram(position="identity",alpha=0.65,bins=30)+
      ggplot2::scale_fill_manual(values=c("Pickup"="#2C7FB8","Drop-off"="#F28E2B"))+
      ggplot2::scale_y_continuous(labels=scales::comma)+
      ggplot2::labs(title="Timing differences for candidate pairs",
                    subtitle=plotSubtitle,
                    x="Absolute difference (minutes)",y="Candidate observations")+
      plotTheme
    ggplot2::ggsave(file.path(outdir,"candidate-timing-compatibility.png"),
                    timingPlot,width=8,height=5,dpi=160)
  }
  return(invisible(NULL))
}

runHouseholdJointTravelValidation <- function(
    plancsv,planGroupCsv,setupDir,outdir,rseed=12345,
    maxTimeDifferenceInMins=30,routeToleranceInMeters=1000,
    minSharedDistanceInMeters=0) {
  requiredFunctions<-c("readVistaCarRoleTrips","assignVistaCarRoles",
                       "findHouseholdJointTravelCandidates",
                       "getPassengerHouseholdDriverStatus")
  missingFunctions<-requiredFunctions[!vapply(requiredFunctions,exists,logical(1),
                                               mode="function")]
  if(length(missingFunctions)>0) {
    stop(paste0("Source R/householdJointTravel.R before running validation; ",
                "missing functions: ",paste(missingFunctions,collapse=", ")))
  }
  dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
  plans<-read.csv(plancsv,stringsAsFactors=FALSE,strip.white=TRUE)
  planGroups<-read.csv(planGroupCsv,stringsAsFactors=FALSE,strip.white=TRUE)
  sourceTrips<-readVistaCarRoleTrips(setupDir)
  vistaSummary<-getVistaPassengerHouseholdDriverSummary(sourceTrips)
  rolePlans<-assignVistaCarRoles(plans,planGroups,sourceTrips,rseed=rseed)
  if(!identical(rolePlans[,colnames(plans),drop=FALSE],plans)) {
    stop("VISTA role assignment changed existing generated plan values")
  }
  rolePlanCsv<-file.path(outdir,"plan.roles.csv")
  write.table(rolePlans,file=rolePlanCsv,row.names=FALSE,sep=',')

  candidates<-findHouseholdJointTravelCandidates(
    rolePlans,maxTimeDifferenceInMins,routeToleranceInMeters,
    minSharedDistanceInMeters
  )
  candidateCsv<-file.path(outdir,"household-joint-travel-candidates.csv")
  write.table(candidates,file=candidateCsv,row.names=FALSE,sep=',')
  summary<-getHouseholdJointTravelSummary(rolePlans,candidates)
  vistaOverall<-vistaSummary[vistaSummary$GroupId=="Overall",,drop=FALSE]
  summary<-rbind(
    data.frame(
      Metric=c("existing_plan_values_changed",
               "role_assignment_seed",
               "max_time_difference_in_mins",
               "route_tolerance_in_metres",
               "min_shared_distance_in_metres",
               "vista_weighted_passenger_legs",
               "vista_weighted_passenger_legs_with_other_household_driver",
               "vista_passenger_household_driver_percent"),
      Value=c(0,rseed,maxTimeDifferenceInMins,routeToleranceInMeters,
              minSharedDistanceInMeters,
              vistaOverall$PassengerTripWeight,
              vistaOverall$PassengerTripWeightWithOtherHouseholdDriver,
              vistaOverall$PassengerHouseholdDriverPercent),
      stringsAsFactors=FALSE
    ),
    summary
  )
  write.table(summary,file=file.path(outdir,"validation-summary.csv"),
              row.names=FALSE,sep=',')
  write.table(
    vistaSummary,
    file=file.path(outdir,"vista-passenger-household-driver-summary.csv"),
    row.names=FALSE,sep=','
  )
  saveHouseholdJointTravelPlots(rolePlans,candidates,outdir,vistaSummary)
  return(invisible(list(plans=rolePlans,candidates=candidates,summary=summary,
                        vistaSummary=vistaSummary)))
}
