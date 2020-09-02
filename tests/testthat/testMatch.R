library(tools) # for md5sum

source("../../R/match.R")

test_that("Matching census persons to VISTA plans generation works", {
  set.seed(12345)
  censuscsv<- '../expected/2.sample/sample.csv.gz'
  vistacsv <- '../expected/3.plan/plan.csv'
  outcsv <-  '../actual/4.match/match.csv.gz'
  outdir<-'../actual/4.match'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  
  capture_output(
    matchPersons(censuscsv, vistacsv, outcsv)
  )

  file<-'match.csv.gz'
  expect_true(file.exists(paste0('../actual/4.match/', file)))
  expect_true(md5sum(paste0('../actual/4.match/', file)) == md5sum(paste0('../expected/4.match/', file)))
})
  