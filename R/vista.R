extract_and_write_activities_end_time_dist_by_start_bins<-function(in_activities_csv_gz, out_csv, binsize) {
  # example inputs:
  # in_activities_csv_gz <- "output/1.setup/vista_2012_18_extracted_activities_weekday.csv.gz"
  # out_csv <- "output/1.setup/vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins.csv.gz"
  # binsize <- 48
  
  # Read the activities data
  gz1 <- gzfile(in_activities_csv_gz,'rt')
  activities<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
  close(gz1)
  
  binSizeInMins<-floor(60*24)/binsize # the size of each bin in minutes
  groups<-unique(activities$Activity.Group) # unique activity names

  df<-activities
  df$Act.Start.Bin<-1 + df$Act.Start.Time%/%binSizeInMins
  df$Act.End.Bin<-1 + df$Act.End.Time%/%binSizeInMins
  df<-df[df$Act.Start.Bin>=1 & df$Act.Start.Bin<=binsize,] # remove records with start bins out of range
  df<-df[df$Act.End.Bin>=1 & df$Act.End.Bin<=binsize,] # remove records with end bins out of range
  
  # create a new dataframe to store end time probabilities for activities for each start time bin
  pp<-data.frame(matrix(0, nrow = binsize*length(groups), ncol = 2+binsize))
  colnames(pp)<-c("Activity.Group", "Act.Start.Bin", seq(1,binsize))
  
  bin<-0
  while(bin<binsize) {
    bin<-bin+1
    dd<-df[df$Act.Start.Bin==bin,] # get all the records for this time bin
    
    i<-0
    while(i<length(groups)) {
      i<-i+1
      de<-dd[dd$Activity.Group==groups[i],]
      x<-as.vector(table(cut(rep(de[,"Act.End.Bin"],de$Count),breaks=seq(1,binsize+1)-0.5, include.lowest = TRUE)))
      if (sum(x)!= 0) x<-x/sum(x)
      offset<-(bin-1)*length(groups)
      pp[offset+i,]<-c(groups[i], bin, x)
    }
  }
  # Write the table out
  write.table(pp, file=out_csv, append=FALSE, row.names=FALSE, sep = ',')
}  

extract_and_write_activities_time_bins<-function(in_activities_csv_gz, out_csv_gz, binsize) {
  gz1 <- gzfile(in_activities_csv_gz,'rt')
  trips_data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
  close(gz1)
  
  # split home activitiy into morning/daytime/night
  activities<- trips_data # split_home_activity(trips_data)
  activities$Act.Duration<- activities$Act.End.Time - activities$Act.Start.Time
  
  groups<-unique(activities$Activity.Group) # unique activity names
  acts<-c("Act.Start.Time", "Act.End.Time", "Act.Duration")
  
  # create a dataframe with activity as rows and bin id as columns
  pp<-data.frame(matrix(0, nrow = length(groups), ncol = 2+binsize))
  colnames(pp)<-c("Activity.Group", "Activity.Stat", seq(1,binsize))
  
  # populate the dataframe with probabilities
  df<-activities
  rowid<-1
  binSizeInMins<-floor(60*24)/binsize
  binStartMins<-seq(0,binsize-1)*binSizeInMins
  binEndMins<-binStartMins+binSizeInMins-1
  for(i in 1:length(groups)) {
    dd<-df[df$Activity.Group==groups[i],]
    for(act in acts) {
      if(act=="Act.Start.Time" || act=="Act.End.Time") {
        #h<-hist(rep(as.numeric(dd[,act]),dd[,]$Count), breaks=binsize)
        h<-as.vector(table(cut(rep(as.numeric(dd[,act]),dd[,]$Count), breaks=c(binStartMins, binEndMins[length(binEndMins)]), include.lowest = TRUE)))
        v<-h/sum(h)
        #if(length(h$counts)>binsize) v<- h$counts[1:binsize]
        #if(length(h$counts)<binsize) v<- c(h$counts, rep(0,binsize-length(h$counts)))
        pp[rowid,]<-c(groups[i], paste0(act,".Prob"), v)
        rowid<-rowid+1
      } else if (act=="Act.Duration") {
        vm<-NULL; vu<-NULL
        for(j in 1:binsize) {
          d<-dd[dd$Act.Start.Time>=binStartMins[j],act]
          if (length(d)==0) {
            m<-0
            u<-0
          } else {
            m<-mean(d)
            u<-sd(d)
          }
          vm<-c(vm,round(m))
          vu<-c(vu,round(u))
        }
        pp[rowid,]<-c(groups[i], paste0(act,".Mins.Mean"), vm)
        pp[rowid+1,]<-c(groups[i], paste0(act,".Mins.Sigma"), vu)
        rowid<-rowid+2
      }
    }
  }
  # Write it out
  gz1 <- gzfile(out_csv_gz, "w")
  write.csv(pp, gz1, row.names=FALSE, quote=TRUE)
  close(gz1)
}
  
extract_and_write_activities_from<-function(in_vista_csv, out_weekday_activities_csv_gz, out_weekend_activities_csv_gz) {
  gz1 <- gzfile(in_vista_csv,'rt')
  vista_data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
  close(gz1)
  
  datacols<-c("PERSID",
              "ORIGPURP1",
              "DESTPURP1",
              "STARTIME","ARRTIME",
              "WDTRIPWGT",
              "WETRIPWGT")
              
  orig<-vista_data[,datacols]

  get_activities<-function (dataset) {
    # Get the activities and their start/end times
    df<-data.frame(row.names = 1:length(dataset$PERSID)) # create a data frame of the correct row size
    df$Person<-dataset$PERSID
    df$Index<-as.numeric(rownames(dataset)) # save the index of the activity
    df$Activity<-dataset$ORIGPURP1 # activity is ORIGPURP1
    df$Act.Start.Time<-c(0,dataset$ARRTIME[1:length(dataset$ARRTIME)-1]) # start time is arrive time of the previous row
    df$Act.End.Time<-dataset$STARTIME # end time is the start time of the trip
    df$Count<-dataset$Count
    
    # What is left is the final "Go Home" activity of each person
    lastact<-dataset[dataset$DESTPURP1=="Go Home",c("PERSID","DESTPURP1","ARRTIME", "Count")] # get all the "Go Home" activities
    colnames(lastact)<-c("Person","Activity", "Act.Start.Time", "Count") # rename the cols
    lastact$Index<-as.numeric(rownames(lastact)) # save the index of the activity
    lastact<-aggregate(lastact,by=list(lastact$Person),FUN=tail,n=1) # remove all but the last "Go Home" for each person
    lastact$Act.End.Time<-1439 # assign these final activities of the day the end time of 23:59
    
    # Now we want to insert these activities at the given index into the original list of activities
    dd<-rbind(df,lastact[,colnames(df)]) # first append them to the end of original set of activities
    id<- c(df$Index,(lastact$Index+0.5)) #give them half-rank indices ie where they should be slotted
    dy<-dd[order(id),] # now use order to pluck the set in the correct order
    
    # Assign the first activitiy of the person a start time of 0
    xx<-aggregate(dy,by=list(dy$Person),FUN=head,1)
    dy$Act.Start.Time<-apply(dy,1,function(x) {
      ifelse(as.numeric(x["Index"]) %in% xx$Index, 0, as.numeric(x["Act.Start.Time"])
      )
    })
    
    # Re-assign indices (since lastact created duplicate ids) and rownames
    dy$Index<-seq(1,length(dy$Index))
    rownames(dy)<-dy$Index
    dy
  }
  
  # Split into weekday/weekend and set the weights (ie counts here) correctly
  week<-orig[,datacols]
  isWeekday<-!is.na(week$WDTRIPWGT)
  weekdays<-week[isWeekday,]; weekdays$Count<- weekdays$WDTRIPWGT
  weekends<-week[!isWeekday,]; weekends$Count<-weekends$WETRIPWGT

  # Fix any rows where the weights are not defined
  if(any(is.na(weekends$Count))) {
    weekends[is.na(weekends$Count),]$Count<-0
  }
  if(any(is.na(weekdays$Count))) {
    weekdays[is.na(weekdays$Count),]$Count<-0
  }
  
  weekdays$Count<-as.numeric(gsub(",", "", weekdays$Count)) # remove commas from Count
  weekends$Count<-as.numeric(gsub(",", "", weekends$Count)) # remove commas from Count
  
  # Get the activities for each set
  weekday_activities<-get_activities(weekdays)
  weekend_activities<-get_activities(weekends)
  
  # Write them out
  gz1 <- gzfile(out_weekday_activities_csv_gz, "w")
  write.csv(weekday_activities, gz1, row.names=FALSE, quote=TRUE)
  close(gz1)
  gz1 <- gzfile(out_weekend_activities_csv_gz, "w")
  write.csv(weekend_activities, gz1, row.names=FALSE, quote=TRUE)
  close(gz1)
}

simplify_activities_and_create_groups<-function(gzfile) {
  
  gz1 <- gzfile(gzfile,'rt')
  activities<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
  close(gz1)
  
  # Simplify the activities as follows:
  # 1. remove "Change Mode" activities which are just in-transit mode-change activities
  # 2. remove "Accompany Someone" which is a secondary activitiy
  df<-activities
  #df<-df[df$Activity!="Change Mode",]
  #df<-df[df$Activity!="Accompany Someone",]
  
  # Assign activities into groups as follows:
  df$Activity.Group<-""
  df$Activity.Group<-ifelse(
    df$Activity=="At Home", 
    "Home", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Go Home", 
    "Home", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Personal Business", 
    "Personal", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Work Related", 
    "Work", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Change Mode", 
    "Mode Change", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Accompany Someone", 
    "With Someone", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Education", 
    "Study", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Buy Something", 
    "Shop", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Unknown Purpose (at start of day)" | df$Activity=="Other Purpose" | df$Activity=="Not Stated", 
    "Other", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Social" | df$Activity=="Recreational", 
    "Social/Recreational", df$Activity.Group)
  df$Activity.Group<-ifelse(
    df$Activity=="Pick-up or Drop-off Someone" | df$Activity=="Pick-up or Deliver Something",
    "Pickup/Dropoff/Deliver", df$Activity.Group)
  
  gz1 <- gzfile(gzfile, "w")
  write.csv(df, gz1, row.names=FALSE, quote=TRUE)
  close(gz1)
  
}
