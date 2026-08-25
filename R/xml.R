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
  agentIds<-as.character(pp$AgentId)
  activities<-as.character(pp$Activity)
  arrivingModes<-as.character(pp$ArrivingMode)
  activityX<-pp$x
  activityY<-pp$y
  activityEndTimes<-as.character(pp$act_end_hhmmss)
  workerRows<-agentIds%in%agentIds[activities=="Work"]
  firstPersonRows<-c(TRUE,agentIds[-1]!=agentIds[-length(agentIds)])
  lastPersonRows<-c(agentIds[-length(agentIds)]!=agentIds[-1],TRUE)
  householdIds<-if("HouseholdId"%in%colnames(pp)) {
    as.character(pp$HouseholdId)
  } else NULL
  householdSizes<-if("HouseholdSize"%in%colnames(pp)) pp$HouseholdSize else NULL
  legAttributeColumns<-c(
    legId="LegId",
    vistaCarRole="VistaCarRole",
    vistaCarRoleInitial="VistaCarRoleInitial",
    vistaInitialHouseholdDriverExpected="VistaInitialHouseholdDriverExpected",
    householdCarRoleAction="HouseholdCarRoleAction",
    vistaRoleSourceTripId="VistaRoleSourceTripId",
    vistaRoleMatchLevel="VistaRoleMatchLevel",
    vistaRoleSourceHouseholdHasOtherDriverTrip=
      "VistaRoleSourceHouseholdHasOtherDriverTrip"
  )
  legAttributeColumns<-legAttributeColumns[
    legAttributeColumns%in%colnames(pp)
  ]
  legAttributeValues<-lapply(
    legAttributeColumns,function(column) as.character(pp[[column]])
  )
  names(legAttributeValues)<-names(legAttributeColumns)
  popnWriteBuffer<-""
  processed<-0
  i=0
  while(i<nrow(pp)) {
    i<-i+1
    
    # if this row marks the start of a new person's plan
    if(firstPersonRows[i]) {
      # count the persons
      processed<-processed+1
      # create a new person
      str<-paste0('<person id="',agentIds[i],'">\n')
      # categorizing into worker and non-worker
      subPopulation <- "NonWorker"
      if(workerRows[i]) subPopulation <- "Worker"
      # creating the sub-population attribute
      str<-paste0(str, '  <attributes>\n')
      str<-paste0(str, '    <attribute name="subpopulation" class="java.lang.String" >',subPopulation,'</attribute>\n')
      if(!is.null(householdIds) && !is.na(householdIds[i])) {
        str<-paste0(str, '    <attribute name="householdId" class="java.lang.String" >',householdIds[i],'</attribute>\n')
      }
      if(!is.null(householdSizes) && !is.na(householdSizes[i])) {
        str<-paste0(str, '    <attribute name="householdSize" class="java.lang.Integer" >',householdSizes[i],'</attribute>\n')
      }
      str<-paste0(str, '  </attributes>\n')
      # create a new plan
      str<-paste0(str, '  <plan selected="yes">\n')
    } else {
      # if not the first activity then also add a leg
      legAttributes<-vapply(
        legAttributeValues,function(values) values[i],character(1)
      )
      legAttributes<-legAttributes[!is.na(legAttributes) & nzchar(legAttributes)]
      if(length(legAttributes)==0) {
        str<-paste0(str, '    <leg mode="',arrivingModes[i],'"/>\n')
      } else {
        str<-paste0(str, '    <leg mode="',arrivingModes[i],'">\n')
        str<-paste0(str, '      <attributes>\n')
        for(attributeName in names(legAttributes)) {
          str<-paste0(str, '        <attribute name="',attributeName,
                      '" class="java.lang.String" >',legAttributes[[attributeName]],
                      '</attribute>\n')
        }
        str<-paste0(str, '      </attributes>\n')
        str<-paste0(str, '    </leg>\n')
      }
    }
    
    # add this row as an activity    
    str<-paste0(str, '    <activity type="',activities[i],'" x="',activityX[i],
                '" y="',activityY[i],'" end_time="',activityEndTimes[i],'"/>\n')
    
    # if this row marks the end of a person's plan 
    if(lastPersonRows[i]) {
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
