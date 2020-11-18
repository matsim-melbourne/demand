library(tools) # for md5sum

checksum <- function() {
  expected<- list(
    '../data/VISTA_12_18_CSV.zip.dir/T_VISTA1218_V1.csv' = 'f16040c1a12c96df8ba1b18d9e6cabf0',
    '../data/distanceMatrix.rds' = 'cafb401785b656f3be660f387a2c3be2', 
    '../data/distanceMatrixIndex.csv' = 'ae0bc43e02033bc2df150e665dd43134', 
    '../data/SA1attributed.csv.gz' = '980faffc7c90996510123ce4d69f140e', 
    '../data/SA1centroids.csv.gz' = 'b7f27ff1e176b70cee71723f528f800a', 
    '../data/addresses.csv.gz' = 'ed761018dfdf4e00fdf2054aa25fe3ba'
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