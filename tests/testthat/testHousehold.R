source("../../R/locateParallel.R")
source("../../R/placeParallel.R")
source("../../R/plan.R")

test_that("Stable leg identifiers are assigned within each person", {
  plans<-data.frame(
    AgentId=c(rep('person_1',3),rep('person_2',2)),
    Activity=c('Home','Work','Home','Home','Home')
  )

  plans<-addStableLegIds(plans)

  expect_equal(
    plans$LegId,
    c(NA,'person_1_leg_1','person_1_leg_2',NA,'person_2_leg_1')
  )
})

test_that("One home coordinate is selected for each household", {
  calls<-0
  coordinateFunction<-function(sa1,locationType) {
    calls<<-calls+1
    c(as.numeric(sa1),as.numeric(sa1)+100)
  }
  plans<-data.frame(
    HouseholdId=c('household_1','household_1','household_1','household_2'),
    SA1_MAINCODE_2016=c(1001,1001,1001,1002),
    LocationType=c('home','work','home','home')
  )

  homes<-getHouseholdHomeLocations(plans,coordinateFunction)

  expect_equal(calls,2)
  expect_equal(homes$HouseholdId,c('household_1','household_2'))
  expect_equal(homes$x,c(1001,1002))
  expect_equal(homes$y,c(1101,1102))
})

test_that("Plan mappings retain household identifiers", {
  outdir<-'../actual/4.plan/households'
  dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
  matchedPrefix<-paste0(outdir,'/match_')
  write.csv(
    data.frame(
      AgentId=c('person_1','person_2'),
      HouseholdId=c('household_1','household_1'),
      HouseholdSize=c(2,2)
    ),
    paste0(matchedPrefix,'1.csv'),
    row.names=FALSE
  )
  write.csv(
    data.frame(
      PlanId=c(1,1,2,2),
      Activity=c('Home','Home','Home','Home'),
      GroupId=1
    ),
    paste0(outdir,'/plans.csv'),
    row.names=FALSE
  )

  capture_output(writePlan2Agent2GroupMap(
    1,
    matchedPrefix,
    paste0(outdir,'/plans.csv'),
    paste0(outdir,'/map.csv')
  ))
  planMap<-read.csv(paste0(outdir,'/map.csv'))

  expect_equal(planMap$AgentId,c('person_1','person_2'))
  expect_equal(planMap$HouseholdId,c('household_1','household_1'))
  expect_equal(planMap$HouseholdSize,c(2,2))
})
