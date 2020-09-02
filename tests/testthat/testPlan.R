library(tools) # for md5sum

source("../../R/plan.R")

test_that("VISTA-like plans generation works", {
  set.seed(12345)
  csv<-paste0('../expected/1.setup/vista_2012_18_extracted_activities_weekday_time_bins.csv.gz')
  endcsv<-paste0('../expected/1.setup/vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins.csv.gz')
  binCols<-3:50 # specifies that columns 3-50 correspond to 48 time bins, i.e., 30-mins each
  outdir<-'../actual/3.plan'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  writeInterval <- 20 # write to file every 1000 plans
  capture_output(
    generatePlans(50, csv, endcsv, binCols, outdir, writeInterval)
  )

  files<-c(
    'analysis-start-times-by-activity-qq.pdf',
    'analysis-end-times-by-activity-qq.pdf',
    'analysis-start-times-by-bin-qq.pdf',
    'analysis-end-times-by-bin-qq.pdf',
    'analysis-start-times-by-activity.pdf',
    'analysis-end-times-by-activity.pdf',
    'analysis-activity-times-by-bin.pdf',
    'plan.csv'
  )
  for (file in files) {
    expect_true(file.exists(paste0('../actual/3.plan/', file)))
  }
  expect_true(md5sum('../actual/3.plan/plan.csv') == md5sum('../expected/3.plan/plan.csv'))
})
  