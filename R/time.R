assignTimesToActivities <- function(plancsv, binSizeInMins, outcsv, writeInterval) {
  # example inputs:
  #  plancsv <- '../output/6.place/plan.csv'
  #  binSizeInMins <- 30
  #  outcsv <- '../output/7.time/plan.csv'
  #  writeInterval <- 100
  
  options(scipen=999) # disable scientific notation for more readible filenames with small sample sizes
  
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(tidyr))
  
  # Converts vector of secs to HH:MM:SS format
  toHHMMSS <- function(v) {
    if(is.null(v) || is.na(v) || !is.numeric(v)) return("??:??:??")
    h <- v %/% (60*60)
    m <- (v - (h*60*60)) %/% 60
    s <- v %% 60
    x <- c(rbind(h,m,s))
    hh <- paste0(str_pad(h,2,pad="0"),":")
    mm <- paste0(str_pad(m,2,pad="0"),":")
    ss <- str_pad(s,2,pad="0")
    hhmmss<-paste0(str_pad(h,2,pad="0"),":",
                   str_pad(m,2,pad="0"),":",
                   str_pad(s,2,pad="0"))
    z <- data.frame(t(rbind(hh,mm,ss)))
    zz <- as.vector(t(z %>% unite("zz", c("hh", "mm", "ss"), sep="")))
    return(zz)
  }
  
  
  # Read in the plans
  gz1<-gzfile(plancsv, 'rt')
  echo(paste0('Loading VISTA-like plans from ', plancsv, '\n'))
  plans<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)
  
  echo('Assigning start/end times to activities  (can take a while)\n')
  pp<-plans
  pp$act_start_hhmmss<-""; pp$act_end_hhmmss<-""
  wplans<-pp[FALSE,]
  write.table(wplans, file=outcsv, append=FALSE, row.names=FALSE, sep = ',')
  
  times<-data.frame(PlanId=c(rbind(pp$PlanId,pp$PlanId)), Secs=60*(binSizeInMins*c(rbind(pp$StartBin,pp$EndBin))-binSizeInMins))
  ids<-unique(pp$PlanId)
  
  processed<-0
  for (id in ids) {
    secs <- times[times$PlanId==id,]$Secs # get the start/end times aligned to bin starts
    offsets <- sample(1:60*binSizeInMins, length(secs)) # generate unique offsets of 30mins length max
    secs<-secs + offsets
    secs <- sort(secs) 
    secs <- toHHMMSS(secs)
    odd <-1:length(secs)%%2 != 0
    starts <- secs[odd]
    ends <- secs[!odd]
    pp[pp$PlanId==id,]$act_start_hhmmss <- starts
    pp[pp$PlanId==id,]$act_end_hhmmss <- ends
    # record progress for each person
    processed<-processed+1
    printProgress(processed, '.')
    # add it to out list
    wplans<-rbind(wplans, pp[pp$PlanId==id,])
    # write it out at regular intervals
    if (processed%%writeInterval==0 || id==length(ids)) {
      write.table(wplans, file=outcsv, append=TRUE, row.names=FALSE, col.names=FALSE, sep = ',')
      wplans<-wplans[FALSE,] # remove all rows
    }
  }
  cat('\n')
  echo(paste0('Wrote ',processed,' plans to ', outcsv , '\n'))
}
