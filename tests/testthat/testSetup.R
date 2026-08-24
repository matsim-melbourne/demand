source("../../R/setup.R")

test_that("VISTA 2012-18 trips pre-processing works", {
  set.seed(12345)
  outdir<-'../actual/1.setup'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  capture_output(
    demand_setup_groups(
      getGroupIds('../data/vistaCohorts.csv.gz'),
      outdir, 
      '../../expected/1.setup/vista_2012_18_extracted_trips_weekday_',
      'vista_2012_18_extracted_activities_weekday_',
      'vista_2012_18_extracted_activities_weekday_time_bins_',
      'vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins_',
      NULL, NULL # ignoring weekends
    )
  )

  files_prefix<-c(
    'vista_2012_18_extracted_activities_weekday_',
    'vista_2012_18_extracted_activities_weekday_time_bins_',
    'vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins_'
  )
  
  groups<-getGroupIds('../data/vistaCohorts.csv.gz')
  for (gid in groups) {
    for (prefix in files_prefix) {
      file<-paste0(prefix,gid,".csv.gz")
      actual_file<-paste0('../actual/1.setup/',file)
      expected_file<-paste0('../expected/1.setup/',file)
      expect_true(file.exists(actual_file))
      expect_equal(
        read.csv(actual_file, check.names=FALSE),
        read.csv(expected_file, check.names=FALSE),
        tolerance=1e-12
      )
    }
  }
})
