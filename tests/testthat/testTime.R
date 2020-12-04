library(tools) # for md5sum

source("../../R/time.R")

test_that("Assigning times to activities works", {
  rseed<-12345
  set.seed(rseed)
  
  binSizeInMins<-30
  plancsv<-'./tests/expected/6.place/plan.csv'
  outcsv<-'./tests/actual/7.time/plan.csv'
  outdir<-'./tests/actual/7.time'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  writeInterval <- 100 # write to file every so many plans
  # uses doParallel which must be run from the project root 
  # to ensure packrat libraries are source correctly by the workers.
  # See https://stackoverflow.com/a/36901524.
  wd<-getwd()
  setwd("../..")
  capture_output(wd)
  capture_output(
    assignTimesToActivities(plancsv, binSizeInMins, outdir, outcsv, writeInterval, rseed)
  )
  setwd(wd) 
  
  expect_true(file.exists('../actual/7.time/plan.csv'))
  expect_true(md5sum('../actual/7.time/plan.csv') == md5sum('../expected/7.time/plan.csv'))
})
  