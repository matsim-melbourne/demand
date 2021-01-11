assignTimesToActivities <- function(plancsv, binSizeInMins, outdir, outcsv, writeInterval, rseed=NULL) {
  # example inputs:
  # plancsv <- './output/6.place/plan.csv'
  # binSizeInMins <- 30
  # outdir <- './output/7.time'
  # outcsv <- './output/7.time/plan.csv'
  # writeInterval <- 100
  # rseed <- 12345
  
  options(scipen=999) # disable scientific notation for more readible filenames with small sample sizes
  
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(doParallel))
  suppressPackageStartupMessages(library(doRNG))
  
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
  
  # Function courtesy https://privefl.github.io/blog/a-guide-to-parallelism-in-r/
  getParallelBlocks <- function (m, block.size, nb = ceiling(m/block.size)) {
    if (nb > m) {
      nb <- m
    }
    int <- m/nb
    upper <- round(1:nb * int)
    lower <- c(1, upper[-nb] + 1)
    size <- c(upper[1], diff(upper))
    cbind(lower, upper, size)
  }
    
  processBlocks <- function(plans, blocks, outcsv, writeInterval, i) {
    lower<-min(which(plans$PlanId==blocks[i,"lower"]))
    upper<-max(which(plans$PlanId==blocks[i,"upper"]))
    pp<-plans[lower:upper,]
    outfile<-paste0(outcsv, ".", i)
    pp$act_start_hhmmss<-""; pp$act_end_hhmmss<-""
    wplans<-pp[FALSE,]
    write.table(wplans, file=outfile, append=FALSE, row.names=FALSE, sep = ',')
    
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
      # add it to out list
      wplans<-rbind(wplans, pp[pp$PlanId==id,])
      # write it out at regular intervals
      if (processed%%writeInterval==0 || id==last(ids)) {
        write.table(wplans, file=outfile, append=TRUE, row.names=FALSE, col.names=FALSE, sep = ',')
        wplans<-wplans[FALSE,] # remove all rows
      }
    }
  }
  
  dir.create(outdir, showWarnings = FALSE, recursive=TRUE)
  
  # Read in the plans
  gz1<-gzfile(plancsv, 'rt')
  echo(paste0('Loading VISTA-like plans from ', plancsv, '\n'))
  plans<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)
  
  # use all but one cores
  ncores <- max(1,detectCores()-1)
  nrecords <- length(unique(plans$PlanId))
  blockSize <- ceiling(nrecords/ncores)
  blocks <- getParallelBlocks(nrecords, blockSize)
  
  echo(paste0('Assigning start/end times to activities using ', ncores, ' cores (can take a while)\n'))

  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  if (!is.null(rseed)) {
    registerDoRNG(seed = rseed)
  }
  stime <- Sys.time()
  result <- foreach(i=1:ncores,
                    .packages = c("stringr", "dplyr", "tidyr")
                    ) %dopar% {
    processBlocks(plans, blocks, outcsv, writeInterval, i)
  }

  etime <- Sys.time()
  echo(paste0('Finished parallel processing in ', round(etime - stime,1), 's\n'))
  stopCluster(cl)
  
  # files<-list.files(outdir,pattern="plan.csv.[[:digit:]]{1}",full.names=T)
  filesDF <- data.frame(
    location=list.files(outdir,pattern="plan.csv.[[:digit:]]{1}",full.names=T),
    order=list.files(outdir,pattern="plan.csv.[[:digit:]]{1}",full.names=F)%>%gsub("plan.csv.", "",.)%>%as.numeric(),
    stringsAsFactors=FALSE
  ) %>% arrange(order)
  
  combined<-lapply(filesDF$location,read.csv,header=T) %>%
    bind_rows()
  colnames(combined)<-c("PlanId","Activity","StartBin","EndBin","AgentId",
                             "SA1_MAINCODE_2016","LocationType","ArrivingMode",
                             "Distance","x","y","act_start_hhmmss","act_end_hhmmss")
  write.table(combined, file=outcsv, append=FALSE, row.names=FALSE, sep = ',')
  echo(paste0('Wrote ',length(unique(combined$PlanId)),' plans to ', outcsv , '\n'))
  
}
