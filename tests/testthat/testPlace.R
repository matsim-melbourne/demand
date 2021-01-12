library(tools) # for md5sum
# setwd('~/Projects/matsim-melbourne/demand/R')
# wd<-getwd()
# setwd("../tests/testthat")
# source("helper_tests.R")

source("../../R/placeParallel.R")

test_that("Assigning coordinates to activities works", {
  rseed<-12345
  set.seed(12345)
  plancsv<-'./tests/expected/5.locate/plan.csv'
  outdir<-'./tests/actual/6.place'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  # writeInterval <- 100 # write to file every so many plans
  
  # locateParallel uses doParallel which must be run from the project root 
  # to ensure packrat libraries are source correctly by the workers.
  # See https://stackoverflow.com/a/36901524.
  wd<-getwd()
  setwd("../..")
  capture_output(wd)
  capture_output(
    assignLocationsToActivities(plancsv, outdir, rseed)
  )
  setwd(wd) 
  capture_output(
    placeToSpatial(read.csv("../actual/6.place/plan.csv"),'../actual/6.place/plan.sqlite')
  )
  expect_true(file.exists('../actual/6.place/plan.sqlite'))
  
  expect_true(file.exists('../actual/6.place/plan.csv'))
  expect_true(md5sum('../actual/6.place/plan.csv') == md5sum('../expected/6.place/plan.csv'))
})
