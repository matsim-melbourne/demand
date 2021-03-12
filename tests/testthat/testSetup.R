library(tools) # for md5sum

source("../../R/setup.R")

test_that("VISTA 2012-18 trips pre-processing works", {
  set.seed(12345)
  outdir<-'../actual/1.setup'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  capture_output(
    demand_setup_groups(
      getGroupIds('../data/vistaCohorts.csv.gz'),
      outdir, 
      '../data/T_VISTA1218_V1.sample.csv',
      'vista_2012_18_extracted_activities_weekday_',
      'vista_2012_18_extracted_activities_weekend_',
      'vista_2012_18_extracted_activities_weekday_time_bins_',
      'vista_2012_18_extracted_activities_weekend_time_bins_',
      'vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins_'
    )
  )

  files_prefix<-c(
    'vista_2012_18_extracted_activities_weekday_',
    'vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins_',
    'vista_2012_18_extracted_activities_weekday_time_bins_',
    'vista_2012_18_extracted_activities_weekend_',
    'vista_2012_18_extracted_activities_weekend_time_bins_'
  )
  
  groups<-getGroupIds('../data/vistaCohorts.csv.gz')
  for (gid in groups) {
    for (prefix in files_prefix) {
      file<-paste0(prefix,gid,".csv.gz")
      expect_true(file.exists(paste0('../actual/1.setup/',file)))
      expect_true(md5sum(paste0('../actual/1.setup/', file)) == md5sum(paste0('../expected/1.setup/', file)))
    }
  }
})
  