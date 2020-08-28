source("../../R/sample.R")
library(tools)
test_that("Sampling of the Melbourne 2016 synthetic population works", {
  set.seed(12345)
  wd<-getwd()
  setwd('../../R')
  sampleMelbourne2016Population('../tests/data/', 1, '../tests/actual/sample.csv.gz')
  setwd(wd)

  file<-'sample.csv.gz'
  expect_true(file.exists(paste0('../actual/', file)))
  expect_true(md5sum(paste0('../actual/', file)) == md5sum(paste0('../expected/', file)))
})
  