library(tools) # for md5sum

source("../../R/xml.R")

test_that("Assigning times to activities works", {
  set.seed(12345)
  plancsv<-'../expected/7.time/plan.csv'
  outxml<-'../actual/8.xml/plan.xml'
  outdir<-'../actual/8.xml'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  writeInterval <- 2 # write to file every so many plans
  capture_output(
    writePlanAsMATSimXML(plancsv, outxml, writeInterval)
  )
  expect_true(file.exists('../actual/8.xml/plan.xml'))
  expect_true(md5sum('../actual/8.xml/plan.xml') == md5sum('../expected/8.xml/plan.xml'))
})
  