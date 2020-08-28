library(tools) # for md5sum

source("../../R/sample.R")

test_that("Sampling of the Melbourne 2016 synthetic population works", {
  set.seed(12345)
  wd<-getwd()
  setwd('../../R')
  outdir<-'../tests/actual/2.sample'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  capture_output(
    sampleMelbourne2016Population('../tests/data/', 1, '../tests/actual/2.sample/sample.csv.gz')
  )
  setwd(wd)

  file<-'sample.csv.gz'
  expect_true(file.exists(paste0('../actual/2.sample/', file)))
  expect_true(md5sum(paste0('../actual/2.sample/', file)) == md5sum(paste0('../expected/2.sample/', file)))
})
  