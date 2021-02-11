library(tools) # for md5sum

checksum <- function() {
  expected<- list(
    '../data/VISTA_12_18_CSV.zip.dir/T_VISTA1218_V1.csv' = 'f16040c1a12c96df8ba1b18d9e6cabf0',
    '../data/distanceMatrix.rds' = 'cafb401785b656f3be660f387a2c3be2', 
    '../data/distanceMatrixIndex.csv' = 'ae0bc43e02033bc2df150e665dd43134', 
    '../data/SA1attributed.csv.gz' = '980faffc7c90996510123ce4d69f140e', 
    '../data/SA1centroids.csv.gz' = 'b7f27ff1e176b70cee71723f528f800a', 
    '../data/addresses.csv.gz' = 'ed761018dfdf4e00fdf2054aa25fe3ba',
    '../data/expectedDistances.rds' = '91e137c02459e21a2fabf541e8cc36de',
    '../data/vistaSummaries/destinationProbabilitiesSA3.rds' = '7dbacc829b312d608b6f24df7235b028',
    '../data/vistaSummaries/distanceDistributionsSA3.rds' = '9ded1a901e720c3f670b5ecda6c105f8',
    '../data/vistaSummaries/distanceHistograms.rds' = '7b4a6f836c56e53be5a9409a4886d42a',
    '../data/vistaSummaries/modeChoiceProbabilitiesSA3.rds' = 'ef6c190af7e1ee7f5c474811a5bf0dee',
    '../data/vistaSummaries/vistaDistanceHistograms.rds' = '2243e27a7ac46e64f2c0f2a68751606b'
  )
  actual<-md5sum(names(expected))
  
  result <- TRUE
  for (file in names(expected)) {
    e <- expected[file]
    a <- actual[file]
    if (a != e) {
      cat("MD5 hash of input data file did not match expectation. File may be outdated.\n")
      cat(paste0("File:          ", file, "\n"))
      cat(paste0("Expected hash: ", e, "\n"))
      cat(paste0("Actual hash:   ", a, "\n"))
      result <- FALSE
    }
  }
  return(result)
}