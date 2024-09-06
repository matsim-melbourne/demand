sampleBendigoPopulation <- function(dataDir, samplePercentage, outcsvgz, plansFile=NA) {
  
  
  samplePersons <- function(persons_csv_gz, samplePercent = NULL) {
    #sampleSize<-10 #for testing purposes
    #infile<-'persons/melbourne-2016-population.persons.csv.gz'
    infile<-persons_csv_gz
    
    # read in the population
    gz1<-gzfile(infile, 'rt')
    all<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T )
    close(gz1)
    
    # get the number of persons in this population file  
    rows<-as.numeric(nrow(all))
    
    if (is.null(samplePercent)) {
      # default sample size is the full set
      percent = rows
    } else {
      # clip to within 0-100 %
      percent<-max(min(samplePercent,100),0) 
    }
    sampleSize<-round(rows*(samplePercent/100.0))
    
    # sample the required number of persons from the population
    if (sampleSize == rows) {
      sampleSet = all
    } else {
      sampleSet<-all[sample(nrow(all), sampleSize),]
    }
    
    sampleSet<-sampleSet[order(as.numeric(rownames(sampleSet))),]
    return(sampleSet)
  }
  
  # Put values for testing the code
  
  outputDir="output"
  dataDir <- "../data/"
  samplePercent <- 1
  dataDir <- '../data'
  samplePercentage <- samplePercent
  outcsvgz <- paste0('../',outputDir,'/2.sample/sample.csv.gz')
  

  df<-data.frame(SA2=list.files(path=dataDir, pattern = "final_persons.csv", recursive = TRUE, full.names = TRUE), stringsAsFactors=FALSE)
  df$samplePercent<-samplePercentage
  persons<- NULL
paste0("Selecting a ", samplePercentage, "% population sample from Melbourne's ", nrow(df), " SA2 areas (can take a while)\n")

source("util.R")
  for(row in 1:nrow(df)) {
    printProgress(row,".")
    persons<-rbind(persons,samplePersons(df$SA2[row], df$samplePercent[row]))
  }
  cat('\n')
  echo(paste0("Wrote ", nrow(persons), " sampled persons to ", outcsvgz, '\n'))
  

  write.csv(persons, file=gzfile(outcsvgz), quote=TRUE, row.names = FALSE)
  

}


