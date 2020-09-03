library(tools) # for md5sum

source("../../R/place.R")

test_that("Assigning coordinates to activities works", {
  set.seed(12345)
  plancsv<-'../expected/5.locate/plan.csv'
  outcsv<-'../actual/6.place/plan.csv'
  outdir<-'../actual/6.place'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  writeInterval <- 100 # write to file every so many plans
  
  capture_output(
    assignLocationsToActivities(plancsv, outcsv, writeInterval)
  )
  capture_output(
    placeToSpatial(read.csv("../actual/6.place/plan.csv"),'../actual/6.place/plan.sqlite')
  )
  
  expect_true(file.exists('../actual/6.place/plan.sqlite'))
  expect_true(file.exists('../actual/6.place/plan.csv'))
  expect_true(md5sum('../actual/6.place/plan.csv') == md5sum('../expected/6.place/plan.csv'))
})
  