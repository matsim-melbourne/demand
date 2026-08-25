source("../../R/householdJointTravel.R")

test_that("VISTA roles are assigned to compatible generated car legs", {
  plans<-data.frame(
    PlanId=c(1,1,1,2,2),
    AgentId=c('person_1','person_1','person_1','person_2','person_2'),
    Activity=c('Home','Work','Home','Home','Study'),
    StartBin=c(1,17,35,1,18),
    EndBin=c(17,35,48,18,30),
    ArrivingMode=c(NA,'car','car',NA,'walk'),
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
    Weight=c(1,1),
    stringsAsFactors=FALSE
  )

  first<-assignVistaCarRoles(plans,planGroups,sourceTrips,rseed=99)
  second<-assignVistaCarRoles(plans,planGroups,sourceTrips,rseed=99)

  expect_equal(first,second)
  expect_equal(first$VistaRoleMatchLevel[2],'group')
  expect_true(first$VistaCarRole[2]%in%c('driver','passenger'))
})
