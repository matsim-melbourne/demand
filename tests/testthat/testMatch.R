library(tools) # for md5sum

source("../../R/match.R")

test_that("Matching census persons to VISTA plans generation works", {
  set.seed(12345)
  wd<-getwd()
  setwd('../../R')
  
  censuscsv<- '../tests/expected/2.sample/sample.csv.gz'
  vistacsv <- '../tests/expected/3.plan/plan.csv'
  outcsv <-  '../tests/actual/4.match/match.csv.gz'

  outdir<-'../tests/actual/4.match'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  
  #capture_output(
    match(censuscsv, vistacsv, outcsv)
  #)
  setwd(wd)

  file<-'match.csv.gz'
  expect_true(file.exists(paste0('../actual/4.match/', file)))
  expect_true(md5sum(paste0('../actual/4.match/', file)) == md5sum(paste0('../expected/4.match/', file)))
})
  