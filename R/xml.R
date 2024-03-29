writePlanAsMATSimXML <- function(plancsv, outxml, writeInterval) {
  # example inputs
  # plancsv <- '../output/7.time/plan.csv'
  # outxml <- '../output/8.xml/plan.xml'
  # writeInterval <- 100 # write to file in blocks of this size
  
  options(scipen=999) # disable scientific notation for more readible filenames with small sample sizes
  

  # Read in the plans
  gz1<-gzfile(plancsv, 'rt')
  echo(paste0('Loading VISTA-like plans from ', plancsv, '\n'))
  plans<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
  close(gz1)
  
  # Change bike=>bicycle as required by baseline MATSim Melbourne scenario 
  # uncomment  the line below if "bicycle" instead of "bike" is desired
  # plans<-mutate(plans,ArrivingMode=replace(ArrivingMode,ArrivingMode=="bike","bicycle"))

    echo('Writing as MATSim XML (can take a while)\n')
  str=c(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<!DOCTYPE population SYSTEM "http://www.matsim.org/files/dtd/population_v6.dtd">',
    '<population>'
  )
  cat(str,file=outxml, sep="\n")
  
  pp<-plans
  popnWriteBuffer<-""
  processed<-0
  i=0
  while(i<nrow(pp)) {
    i<-i+1
    
    # if this row marks the start of a new person's plan
    if(i==1 || (pp[i,]$AgentId != pp[i-1,]$AgentId)) {
      # count the persons
      processed<-processed+1
      # create a new person
      str<-paste0('<person id="',processed-1,'">\n')
      # check if the person has work trips
      acts<-pp[which(pp$AgentId==pp[i,]$AgentId),"Activity"]
      # categorizing into worker and non-worker
      subPopulation <- "NonWorker"
      if("Work"%in%acts) subPopulation <- "Worker"
      # creating the sub-population attribute
      str<-paste0(str, '  <attributes>\n')
      str<-paste0(str, '    <attribute name="subpopulation" class="java.lang.String" >',subPopulation,'</attribute>\n')
      str<-paste0(str, '  </attributes>\n')
      # create a new plan
      str<-paste0(str, '  <plan selected="yes">\n')
    } else {
      # if not the first activity then also add a leg
      str<-paste0(str, '    <leg mode="',pp[i,]$ArrivingMode,'"/>\n') 
    }
    
    # add this row as an activity    
    str<-paste0(str, '    <activity type="',pp[i,]$Activity,'" x="',pp[i,]$x,'" y="',pp[i,]$y,'" end_time="',pp[i,]$act_end_hhmmss,'"/>\n') 
    
    # if this row marks the end of a person's plan 
    if(i==nrow(pp) || pp[i,]$AgentId != pp[i+1,]$AgentId) {
      # close off the tags
      str<-paste0(str, '  </plan>\n')
      str<-paste0(str, '</person>\n')
      # add person to write buffer
      popnWriteBuffer <- paste0(popnWriteBuffer, str)
      # write it out at regular intervals
      if (processed%%writeInterval==0 || i==nrow(pp)) {
        cat(popnWriteBuffer,file=outxml, sep="", append=TRUE)
        popnWriteBuffer<-"" # clear the buffer after writing it out
      }
      # report progress
      printProgress(processed,'.')
    }
    
  }
  cat('</population>',file=outxml, append=TRUE,sep="\n")
  cat('\n')
  echo(paste0('Wrote ',processed,' plans to ', outxml , '\n'))
  # close off the population XML
}
