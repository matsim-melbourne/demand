source("../../R/setup.R")
library(tools)
test_that("VISTA 2012-18 trips pre-processing works", {
  set.seed(12345)
  wd<-getwd()
  setwd('../../R')
  demand_setup('../tests/actual', '../tests/data/T_VISTA1218_V1.sample.csv')
  setwd(wd)
  
  files<-c(
    'vista_2012_18_extracted_activities_weekday.csv.gz',
    'vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins.csv.gz',
    'vista_2012_18_extracted_activities_weekday_time_bins.csv.gz',
    'vista_2012_18_extracted_activities_weekend.csv.gz',
    'vista_2012_18_extracted_activities_weekend_time_bins.csv.gz'
  )
  for (file in files) {
    expect_true(file.exists(paste0('../actual/', file)))
    expect_true(md5sum(paste0('../actual/', file)) == md5sum(paste0('../expected/', file)))
  }
})
  