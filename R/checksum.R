library(tools) # for md5sum

checksum <- function() {
  expected<- list(
    '../data/VISTA_12_18_CSV.zip.dir/T_VISTA1218_V1.csv' = 'f16040c1a12c96df8ba1b18d9e6cabf0',
    '../data/absRegionsReprojected.sqlite' = '43a51bb3dbd5cfdabad23645d7a0dc87',
    '../data/distanceMatrix.rds' = 'cafb401785b656f3be660f387a2c3be2',
    '../data/distanceMatrixIndex.csv' = 'ae0bc43e02033bc2df150e665dd43134',
    '../data/SA1attributed.csv.gz' = '64d2031ff63d5496def5ff06784b8697',
    '../data/SA1centroids.csv.gz' = 'b7f27ff1e176b70cee71723f528f800a',
    '../data/addresses.csv.gz' = '43bca5fa2d814ce3337c8a01da6841e3',
    '../data/vistaCohorts.csv.gz' = 'd9f0c9ad76fc31513afbc9348b747e32',
    '../data/expectedDistances.rds' = '0d725dcfc2603a94399b78523fecc078',
    '../data/vistaSummaries/destinationProbabilitiesSA3.rds' = '938309b079c9bdeae46254d9c6dae321',
    '../data/vistaSummaries/distanceDistributionsSA3.rds' = '94bfe1ebb96a0703bfc712d4e0ab5639',
    '../data/vistaSummaries/distanceHistograms.rds' = '947647e3d342628983c0bcf7506cff9c',
    '../data/vistaSummaries/modeChoiceProbabilitiesSA3.rds' = 'c25809d0f3fd0653320a06c50eff4a82'
  )
  actual<-md5sum(names(expected))

  result <- TRUE
  for (file in names(expected)) {
    e <- expected[file]
    a <- actual[file]
    cat(paste0("Checking for ", file, ": "))
    if (!file.exists(file)) {
      cat("NOT FOUND\n")
      result <- FALSE
    } else if (a != e) {
      cat(paste0("MISMATCH\n!!! expected ", e, ", actual ", a, "\n"))
      result <- FALSE
    } else {
      cat("OK\n")
    }
  }
  return(result)
}
