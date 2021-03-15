library(tools) # for md5sum

source("../../R/plan.R")

test_that("VISTA-like plans generation works", {
  set.seed(12345)
  outdir<-'../actual/4.plan'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)

  capture_output(
    generatePlansByGroup(
      getGroupIds('../data/vistaCohorts.csv.gz'),
      '../expected/3.match/match_',
      '../expected/1.setup/vista_2012_18_extracted_activities_weekday_time_bins_',
      '../expected/1.setup/vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins_',
      3:50, # specifies that columns 3-50 correspond to 48 time bins, i.e., 30-mins each
      '../actual/4.plan/',
      20 # write to file every so often
    )
  )
  capture_output(
    combinePlans(
      getGroupIds('../data/vistaCohorts.csv.gz'),
      '../actual/4.plan/',
      '../actual/4.plan/plan.csv'
    )
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
  
  groups<-getGroupIds('../data/vistaCohorts.csv.gz')
  for (gid in groups) {
    for (file in files) {
      expect_true(file.exists(paste0('../actual/4.plan/',gid,'/',file)))
    }
  }
  
  expect_true(file.exists('../actual/4.plan/plan.csv'))
  expect_true(md5sum('../actual/4.plan/plan.csv') == md5sum('../expected/4.plan/plan.csv'))
})
  