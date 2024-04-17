# Matches ABS census-like persons to VISTA-like plans
matchPersons<-function(filters, censuscsv, outcsv_prefix) {
  # example inputs
  # filters <- getGroups('../data/vistaCohorts.csv.gz')
  # censuscsv <- '../output/2.sample/sample.csv.gz'
  # outcsv_prefix <- '../output/3.match/match_'
  
  
  # Read in the persons
  gz1<-gzfile(censuscsv, 'rt')
  echo(paste0('Loading ABS census-like persons from ', censuscsv, '\n'))
  orig<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)
  
  echo(paste0('Matching census-like persons to VISTA-like groups\n'))
  
  groupIds <- unique(filters$cluster_id_5)

  # write csv header in new file first
  for (gid in groupIds) {
    outfile <- paste0(outcsv_prefix,gid,".csv")
    echo(paste0('Creating ',outfile,'\n'))
    write.table(orig[0,], outfile, row.names=FALSE, quote=TRUE, sep=",")
  }
  
  for (row in 1:nrow(filters)) {
    gender <- if(filters[row,]$sex=="M") "Male" else "Female"
    cohort <- orig %>%
      filter(Age >= filters[row,]$age_start & Age <= filters[row,]$age_end & Gender == gender)
    # Get unique AgentIds for the group to avoid duplicates   
    cohort <- cohort %>%
      dplyr::distinct(AgentId, .keep_all = TRUE)
    outfile <- paste0(outcsv_prefix,filters[row,]$cluster_id_5,".csv")
    echo(paste0('Appending to ',outfile,'\n'))
    write.table(cohort, outfile, row.names=FALSE, col.names=FALSE, quote=TRUE, sep=",", append=TRUE)
  }
}
