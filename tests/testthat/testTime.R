library(tools) # for md5sum

source("../../R/time.R")

test_that("Assigning times to activities works", {
  set.seed(12345)
  
  binSizeInMins<-30
  plancsv<-'../expected/6.place/plan.csv'
  outcsv<-'../actual/7.time/plan.csv'
  outdir<-'../actual/7.time'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  writeInterval <- 100 # write to file every so many plans
  capture_output(
    assignTimesToActivities(plancsv, binSizeInMins, outcsv, writeInterval)
  )
  expect_true(file.exists('../actual/7.time/plan.csv'))
  expect_true(md5sum('../actual/7.time/plan.csv') == md5sum('../expected/7.time/plan.csv'))
})
  