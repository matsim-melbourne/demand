assignTimesToActivities <- function(plancsv, binSizeInMins, outcsv, writeInterval) {

  options(scipen=999) # disable scientific notation for more readible filenames with small sample sizes
  
  suppressPackageStartupMessages(library(stringr))

  # Converts mins to HH:MM:SS format
  toHHMMSS <- function(mins) {
    if(is.null(mins) || is.na(mins) || !is.numeric(mins)) return("??:??:??")
    h<-mins %/% 60
    m<-mins - (h*60)
    s<-0
    hhmmss<-paste0(str_pad(h,2,pad="0"),":",
                   str_pad(m,2,pad="0"),":",
                   str_pad(s,2,pad="0"))
    return(hhmmss)
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
  processed<-0
  i=0
  while(i<nrow(pp)) {
    i<-i+1
    assignedStart<-FALSE
    assignedEnd<-FALSE
    if(i<nrow(pp) && (pp[i,]$EndBin == pp[i+1,]$StartBin)) {
      # if the end bin of this activity is the same as the start bin of the next activity, 
      # then put this end time in the third quarter of the bin
      pp[i,]$act_end_hhmmss<-toHHMMSS((pp[i,]$EndBin*(binSizeInMins-1))+(binSizeInMins*0.5)+sample(binSizeInMins*0.25, 1))
      assignedEnd<-TRUE
    } 
    if (i>1 && (pp[i,]$StartBin == pp[i-1,]$EndBin)) {
      # if the start bin of this activity is the same as the end bin of the previous activity, 
      # then put this start time in the start of the fourth quarter of the bin
      pp[i,]$act_start_hhmmss<-toHHMMSS((pp[i,]$StartBin*(binSizeInMins-1))+(binSizeInMins*0.75)+sample(binSizeInMins*0.125, 1))
      assignedStart<-TRUE
    } 
    if (pp[i,]$StartBin == pp[i,]$EndBin) {
      if (assignedStart && assignedEnd) {
        # getting too tight; need to squeeze start/end in the start of the fourth quarter of the bin
        tx<-(pp[i,]$StartBin*(binSizeInMins-1))+(binSizeInMins*0.75)+sample(binSizeInMins*0.125, 1)
        ty<-(pp[i,]$StartBin*(binSizeInMins-1))+(binSizeInMins*0.75)+sample(binSizeInMins*0.125, 1)
        if (tx < ty) {
          pp[i,]$act_start_hhmmss<-toHHMMSS(tx)
          pp[i,]$act_end_hhmmss<-toHHMMSS(ty)
        } else {
          pp[i,]$act_start_hhmmss<-toHHMMSS(ty)
          pp[i,]$act_end_hhmmss<-toHHMMSS(tx)
        }
      } else if (assignedEnd) {
        # if end is already assigned (in tird quarter) then put the start in the first half
        pp[i,]$act_start_hhmmss<-toHHMMSS((pp[i,]$StartBin*(binSizeInMins-1))+sample(binSizeInMins*0.5, 1))
        assignedStart<-TRUE
      } else if (assignedStart) {
        # if start is already assigned (in the fourth quarter) then put the end after it
        pp[i,]$act_end_hhmmss<-toHHMMSS((pp[i,]$EndBin*(binSizeInMins-1))+(binSizeInMins*(0.75+0.125))+sample(binSizeInMins*0.125, 1))
        assignedEnd<-TRUE
      } else {
        # else if the start/end bins of this activity is the same, then put start/end in each half of the bin
        pp[i,]$act_start_hhmmss<-toHHMMSS((pp[i,]$StartBin*(binSizeInMins-1))+sample(binSizeInMins*0.5, 1))
        assignedStart<-TRUE
        pp[i,]$act_end_hhmmss<-toHHMMSS((pp[i,]$EndBin*(binSizeInMins-1))+(binSizeInMins*0.5)+sample(binSizeInMins*0.5, 1))
        assignedEnd<-TRUE
      }
    } 
    if (!assignedStart) {
      # else put the start/end times anywhere in the bin
      pp[i,]$act_start_hhmmss<-toHHMMSS((pp[i,]$StartBin*(binSizeInMins-1))+sample(binSizeInMins, 1))
    }
    if (!assignedEnd) {
      # else put the start/end times anywhere in the bin
      pp[i,]$act_end_hhmmss<-toHHMMSS((pp[i,]$EndBin*(binSizeInMins-1))+sample(binSizeInMins, 1))
    }
    
    # add it to out list
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
