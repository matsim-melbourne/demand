library(tools) # for md5sum

source("../../R/locateParallel.R")

test_that("Assigning SA1s to activities works", {
  rseed<-12345
  set.seed(rseed)
  censuscsv<-'./tests/expected/2.sample/sample.csv.gz'
  vistacsv<-'./tests/expected/3.plan/plan.csv'
  matchcsv<-'./tests/expected/4.match/match.csv.gz'
  outdir<-'./tests/actual/5.locate'
  outcsv<-'./tests/actual/5.locate/plan.csv'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  # locateParallel uses doParallel which must be run from the project root 
  # to ensure packrat libraries are source correctly by the workers.
  # See https://stackoverflow.com/a/36901524.
  wd<-getwd()
  setwd("../..")
  capture_output(wd)
  capture_output(
    locatePlans(censuscsv, vistacsv, matchcsv, outdir, outcsv, rseed)
  )
  setwd(wd) 
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
  