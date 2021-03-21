library(tools) # for md5sum

source("../../R/group.R")

test_that("VISTA 2012-18 groupings work", {
  set.seed(12345)
  outdir<-'../actual/1.setup'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  capture_output(
    make_groups(
      '../data/P_VISTA1218_V1.sample.csv',
      '../data/T_VISTA1218_V1.sample.csv',
      '../data/vistaCohorts.csv.gz',
      '../actual/1.setup',
      '../actual/1.setup/vista_2012_18_extracted_persons_weekday.csv.gz',
      'vista_2012_18_extracted_group_weekday_',
      'vista_2012_18_extracted_trips_weekday_',
      NULL, NULL, NULL # ignoring weekends
    )
  )
  files<-c(
    'vista_2012_18_extracted_persons_weekday.csv.gz',
    'vista_2012_18_extracted_group_weekday_1.csv',
    'vista_2012_18_extracted_group_weekday_2.csv',
    'vista_2012_18_extracted_group_weekday_3.csv',
    'vista_2012_18_extracted_group_weekday_4.csv',
    'vista_2012_18_extracted_group_weekday_5.csv',
    'vista_2012_18_extracted_trips_weekday_1.csv',
    'vista_2012_18_extracted_trips_weekday_2.csv',
    'vista_2012_18_extracted_trips_weekday_3.csv',
    'vista_2012_18_extracted_trips_weekday_4.csv',
    'vista_2012_18_extracted_trips_weekday_5.csv'
  )
  for (file in files) {
    expect_true(file.exists(paste0('../actual/1.setup/', file)))
    expect_true(md5sum(paste0('../actual/1.setup/', file)) == md5sum(paste0('../expected/1.setup/', file)))
  }
})
  