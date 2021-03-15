library(tools) # for md5sum

source("../../R/match.R")

test_that("Matching census persons to VISTA groups works", {
  set.seed(12345)
  outdir<-'../actual/3.match'
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  
  capture_output(
    matchPersons(
      getGroups('../data/vistaCohorts.csv.gz'),
      '../expected/2.sample/sample.csv.gz',
      '../actual/3.match/match_'
    )
    
  )

  files_prefix<-c(
    'match_'
  )
  
  groups<-getGroupIds('../data/vistaCohorts.csv.gz')
  for (gid in groups) {
    for (prefix in files_prefix) {
      file<-paste0(prefix,gid,".csv")
      expect_true(file.exists(paste0('../actual/3.match/',file)))
      expect_true(md5sum(paste0('../actual/3.match/', file)) == md5sum(paste0('../expected/3.match/', file)))
    }
  }
})
  