library(tools) # for md5sum

source("../../R/locate.R")

test_that("Assigning SA1s to activities works", {
  set.seed(12345)
  censuscsv<-'../expected/2.sample/sample.csv.gz'
  vistacsv<-'../expected/3.plan/plan.csv'
  matchcsv<-'../expected/4.match/match.csv.gz'
  outcsv<-'../actual/5.locate/plan.csv'
  outdir<-'../actual/5.locate'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  writeInterval <- 100 # write to file every so many plans
  capture_output(
    assignActivityAreasAndTravelModes(censuscsv, vistacsv, matchcsv, outdir, outcsv, writeInterval)
  )
  capture_output(
    planToSpatial(read.csv("../actual/5.locate/plan.csv"),'../actual/5.locate/plan.sqlite')
  )
  
  files<-c(
    'plan.csv',
    'persons.discarded.csv'
  )
  for (file in files) {
    expect_true(file.exists(paste0('../actual/5.locate/', file)))
    expect_true(md5sum(paste0('../actual/5.locate/', file)) == md5sum(paste0('../expected/5.locate/', file)))
  }
  expect_true(file.exists('../actual/5.locate/plan.sqlite'))
})
  