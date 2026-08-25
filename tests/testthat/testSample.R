source("../../R/sample.R")

test_that("Sampling keeps Melbourne 2016 synthetic households complete", {
  set.seed(12345)
  outdir<-'../actual/2.sample'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  capture_output(
    sampleMelbourne2016Population(
      '../data/melbourne-2016-household-sample',
      50,
      '../actual/2.sample/sample.csv.gz'
    )
  )

  expect_true(file.exists('../actual/2.sample/sample.csv.gz'))
  sampled_persons<-read.csv('../actual/2.sample/sample.csv.gz')
  households<-read.csv('../data/melbourne-2016-household-sample/population/households.csv')
  sampled_households<-households[households$HouseholdId%in%sampled_persons$HouseholdId,]

  expect_equal(nrow(sampled_households),2)
  expect_equal(
    as.integer(table(sampled_persons$HouseholdId)[sampled_households$HouseholdId]),
    sampled_households$HouseholdSize
  )
  expect_true(all(c('HouseholdId','HouseholdSize')%in%colnames(sampled_persons)))
  expect_false(any(is.na(sampled_persons$SA1_MAINCODE_2016)))
  expect_true(all(vapply(split(sampled_persons,sampled_persons$HouseholdId),function(household) {
    length(unique(household$SA1_7DIGCODE))==1
  },logical(1))))
})
