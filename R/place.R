assignLocationsToActivities <- function(plancsv, outcsv, writeInterval) {

  options(scipen=999) # disable scientific notation for more readible filenames with small sample sizes
  
  # Read in the plans
  gz1<-gzfile(plancsv, 'rt')
  echo(paste0('Loading VISTA-like plans from ', plancsv, '\n'))
  plans<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)
  
  echo('Assigning coordinates to activities in SA1s (can take a while)\n')
  pp<-plans
  pp$x<-0; pp$y<-0
  wplans<-pp[FALSE,]
  write.table(wplans, file=outcsv, append=FALSE, row.names=FALSE, sep = ',')
  processed<-0
  homexy<-NULL
  i=0
  while(i<nrow(pp)) {
    i<-i+1
    newPerson<-i==1 || pp[i,]$AgentId != pp[i-1,]$AgentId
    if(newPerson) {
      homexy<-NULL 
    }
    if(is.null(homexy) && pp[i,]$LocationType=="home") {
      homexy <- getAddressCoordinates(pp[i,]$SA1_MAINCODE_2016, pp[i,]$LocationType)
    }
    if(pp[i,]$LocationType=="home") {
      pp[i,]$x <- homexy[1]
      pp[i,]$y <- homexy[2]
    } else {
      xy<-getAddressCoordinates(pp[i,]$SA1_MAINCODE_2016, pp[i,]$LocationType)
      pp[i,]$x<-xy[1]
      pp[i,]$y<-xy[2]
    }
    wplans<-rbind(wplans, pp[i,])
    # record progress for each person
    if(i==nrow(pp) || pp[i,]$AgentId != pp[i+1,]$AgentId) {
      processed<-processed+1
      printProgress(processed, '.')
    }
    # write it out at regular intervals
    if (processed%%writeInterval==0 || i==nrow(pp)) {
      write.table(wplans, file=outcsv, append=TRUE, row.names=FALSE, col.names=FALSE, sep = ',')
      wplans<-wplans[FALSE,] # remove all rows
    }
  }
  cat('\n')
  echo(paste0('Wrote ',processed,' plans to ', outcsv , '\n'))
}
