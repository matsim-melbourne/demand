source("../../R/householdJointTravel.R")

test_that("VISTA roles are assigned to compatible generated car legs", {
  plans<-data.frame(
    PlanId=c(1,1,1,2,2),
    AgentId=c('person_1','person_1','person_1','person_2','person_2'),
    Activity=c('Home','Work','Home','Home','Study'),
    StartBin=c(1,17,35,1,18),
    EndBin=c(17,35,48,18,30),
    ArrivingMode=c(NA,'car','car',NA,'walk'),
    HouseholdId='household_1',
    Distance=c(NA,10000,10000,NA,2000),
    stringsAsFactors=FALSE
  )
  planGroups<-data.frame(PlanId=c(1,2),GroupId=c(3,3))
  sourceTrips<-data.frame(
    GroupId=c(3,3),
    VistaTripId=c('outward_driver','return_passenger'),
    VistaPersonId=c('vista_person_1','vista_person_1'),
    VistaHouseholdId=c('vista_household_1','vista_household_1'),
    StartTime=c(490,1030),
    ArrivalTime=c(510,1050),
    OriginPurpose=c('At Home','Work Related'),
    DestinationPurpose=c('Work Related','Go Home'),
    VistaCarRole=c('driver','passenger'),
    VistaHouseholdHasDriverTrip=c(TRUE,TRUE),
    VistaHouseholdHasOtherDriverTrip=c(FALSE,FALSE),
    Weight=c(1,1),
    stringsAsFactors=FALSE
  )

  assigned<-assignVistaCarRoles(plans,planGroups,sourceTrips,rseed=12345)
  carRows<-which(plans$ArrivingMode=='car')

  expect_equal(assigned$VistaCarRole[carRows],c('driver','passenger'))
  expect_equal(
    assigned$VistaRoleSourceTripId[carRows],
    c('outward_driver','return_passenger')
  )
  expect_equal(
    assigned$VistaRoleMatchLevel[carRows],
    c('purpose_pair_time','purpose_pair_time')
  )
  expect_true(all(is.na(assigned$VistaCarRole[-carRows])))
  expect_equal(assigned$ArrivingMode,plans$ArrivingMode)
  expect_equal(assigned$Distance,plans$Distance)
  expect_equal(assigned[,colnames(plans)],plans)
})

test_that("VISTA role assignment is reproducible and reports fallbacks", {
  plans<-data.frame(
    PlanId=c(1,1),
    AgentId=c('person_1','person_1'),
    Activity=c('Home','Other'),
    StartBin=c(1,20),
    EndBin=c(20,30),
    ArrivingMode=c(NA,'car'),
    HouseholdId='household_1',
    stringsAsFactors=FALSE
  )
  planGroups<-data.frame(PlanId=1,GroupId=2)
  sourceTrips<-data.frame(
    GroupId=c(2,2),
    VistaTripId=c('source_trip_1','source_trip_2'),
    VistaPersonId=c('vista_1','vista_2'),
    VistaHouseholdId=c('vista_household_1','vista_household_2'),
    StartTime=c(400,500),
    ArrivalTime=c(420,520),
    OriginPurpose=c('At Home','At Home'),
    DestinationPurpose=c('Education','Education'),
    VistaCarRole=c('driver','passenger'),
    VistaHouseholdHasDriverTrip=c(TRUE,FALSE),
    VistaHouseholdHasOtherDriverTrip=c(FALSE,FALSE),
    Weight=c(1,1),
    stringsAsFactors=FALSE
  )

  first<-assignVistaCarRoles(plans,planGroups,sourceTrips,rseed=99)
  second<-assignVistaCarRoles(plans,planGroups,sourceTrips,rseed=99)

  expect_equal(first,second)
  expect_equal(first$VistaRoleMatchLevel[2],'group')
  expect_true(first$VistaCarRole[2]%in%c('driver','passenger'))
})

test_that("household driver context is satisfied by another household member", {
  plans<-data.frame(
    PlanId=rep(1:2,each=2),
    AgentId=rep(c('person_1','person_2'),each=2),
    HouseholdId='household_1',
    Activity=rep(c('Home','Work'),2),
    StartBin=rep(c(1,17),2),
    EndBin=rep(c(17,35),2),
    ArrivingMode=rep(c(NA,'car'),2),
    stringsAsFactors=FALSE
  )
  planGroups<-data.frame(PlanId=1:2,GroupId=3)
  sourceTrips<-data.frame(
    GroupId=c(3,3),
    VistaTripId=c('internal_passenger','available_driver'),
    VistaPersonId=c('vista_passenger','vista_driver'),
    VistaHouseholdId=c('vista_household_1','vista_household_1'),
    StartTime=c(490,490),
    ArrivalTime=c(510,510),
    OriginPurpose=c('At Home','At Home'),
    DestinationPurpose=c('Work Related','Buy Something'),
    VistaCarRole=c('passenger','driver'),
    VistaHouseholdHasDriverTrip=c(TRUE,TRUE),
    VistaHouseholdHasOtherDriverTrip=c(TRUE,FALSE),
    Weight=c(1,1),
    stringsAsFactors=FALSE
  )

  assigned<-assignVistaCarRoles(plans,planGroups,sourceTrips,rseed=12345)
  passengerRows<-which(assigned$VistaCarRole=='passenger')

  expect_equal(assigned$VistaCarRoleInitial[c(2,4)],c('passenger','passenger'))
  expect_equal(assigned$VistaCarRole[c(2,4)],c('passenger','driver'))
  expect_equal(
    assigned$HouseholdCarRoleAction[c(2,4)],
    c('unchanged','household_driver_added')
  )
  expect_true(all(getPassengerHouseholdDriverStatus(assigned)[passengerRows]))
  expect_equal(assigned[,colnames(plans)],plans)
})

test_that("passenger roles can retain travel with a non-household driver", {
  plans<-data.frame(
    PlanId=rep(1,2),
    AgentId=rep('person_1',2),
    HouseholdId='household_1',
    Activity=c('Home','Work'),
    StartBin=c(1,17),
    EndBin=c(17,35),
    ArrivingMode=c(NA,'car'),
    stringsAsFactors=FALSE
  )
  planGroups<-data.frame(PlanId=1,GroupId=3)
  sourceTrips<-data.frame(
    GroupId=c(3,3),
    VistaTripId=c('internal_passenger','external_passenger'),
    VistaPersonId=c('vista_person_1','vista_person_2'),
    VistaHouseholdId=c('vista_household_1','vista_household_2'),
    StartTime=c(490,490),
    ArrivalTime=c(510,510),
    OriginPurpose=c('At Home','At Home'),
    DestinationPurpose=c('Work Related','Buy Something'),
    VistaCarRole=c('passenger','passenger'),
    VistaHouseholdHasDriverTrip=c(TRUE,FALSE),
    VistaHouseholdHasOtherDriverTrip=c(TRUE,FALSE),
    Weight=c(1,1),
    stringsAsFactors=FALSE
  )

  assigned<-assignVistaCarRoles(plans,planGroups,sourceTrips,rseed=12345)

  expect_equal(assigned$VistaCarRoleInitial[2],'passenger')
  expect_true(assigned$VistaInitialHouseholdDriverExpected[2])
  expect_equal(assigned$VistaCarRole[2],'passenger')
  expect_false(assigned$VistaRoleSourceHouseholdHasOtherDriverTrip[2])
  expect_equal(
    assigned$HouseholdCarRoleAction[2],
    'external_passenger_substituted'
  )
  expect_false(getPassengerHouseholdDriverStatus(assigned)[2])
})

test_that("household joint-travel candidates retain all feasible options", {
  agents<-c('driver_1','driver_2','passenger_1','passenger_2')
  plans<-data.frame(
    PlanId=rep(seq_along(agents),each=2),
    AgentId=rep(agents,each=2),
    HouseholdId='household_1',
    LegId=rep(c(NA,'leg'),length(agents)),
    ArrivingMode=rep(c(NA,'car'),length(agents)),
    VistaCarRole=rep(c(NA,NA),length(agents)),
    x=c(0,10000,0,10000,1000,5000,2000,8000),
    y=c(0,0,500,500,100,100,200,200),
    act_start_hhmmss=c('00:00:00','08:30:00','00:00:00','08:30:00',
                       '00:00:00','08:15:00','00:00:00','08:24:00'),
    act_end_hhmmss=c('08:00:00','17:00:00','08:00:00','17:00:00',
                     '08:03:00','17:00:00','08:06:00','17:00:00'),
    stringsAsFactors=FALSE
  )
  legRows<-seq(2,nrow(plans),by=2)
  plans$LegId[legRows]<-paste0(agents,'_leg_1')
  plans$VistaCarRole[legRows]<-c('driver','driver','passenger','passenger')
  originalPlans<-plans

  candidates<-findHouseholdJointTravelCandidates(
    plans,maxTimeDifferenceInMins=10,routeToleranceInMeters=500
  )

  expect_equal(nrow(candidates),4)
  expect_equal(as.integer(table(candidates$DriverLegId)),c(2L,2L))
  expect_equal(as.integer(table(candidates$PassengerLegId)),c(2L,2L))
  expect_true(all(candidates$PassengerSeatsRequired==1))
  expect_true(all(candidates$VehicleCapacityRequired==2))
  expect_true(all(candidates$SharedRouteDistanceInMeters>0))
  expect_true(all(candidates$PickupWindowStartSeconds<=
                    candidates$PickupWindowEndSeconds))
  expect_true(all(candidates$DropoffWindowStartSeconds<=
                    candidates$DropoffWindowEndSeconds))
  expect_identical(plans,originalPlans)
})

test_that("joint-travel candidates stay within household, time and direction", {
  plans<-data.frame(
    PlanId=rep(1:4,each=2),
    AgentId=rep(c('driver','late_passenger','other_household','reverse'),each=2),
    HouseholdId=rep(c('household_1','household_1','household_2','household_1'),each=2),
    LegId=rep(c(NA,'leg'),4),
    ArrivingMode=rep(c(NA,'car'),4),
    VistaCarRole=rep(c(NA,'passenger'),4),
    x=c(0,10000,1000,5000,1000,5000,8000,2000),
    y=0,
    act_start_hhmmss=c('00:00:00','08:30:00','00:00:00','10:30:00',
                       '00:00:00','08:30:00','00:00:00','08:30:00'),
    act_end_hhmmss=c('08:00:00','17:00:00','10:00:00','17:00:00',
                     '08:03:00','17:00:00','08:03:00','17:00:00'),
    stringsAsFactors=FALSE
  )
  legRows<-seq(2,nrow(plans),by=2)
  plans$LegId[legRows]<-paste0(plans$AgentId[legRows],'_leg_1')
  plans$VistaCarRole[2]<-'driver'

  candidates<-findHouseholdJointTravelCandidates(
    plans,maxTimeDifferenceInMins=10,routeToleranceInMeters=100
  )

  expect_equal(nrow(candidates),0)
  expect_equal(colnames(candidates),colnames(emptyHouseholdJointTravelCandidates()))
})
