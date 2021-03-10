# Function to pre-process some data; need only be run once
make_groups<-function(vista18PersonsCsv, 
                      vista18TripsCsv,
                      filterCsv, 
                      setupDir,
                      out_weekday_persons_csv_gz, 
                      out_weekend_persons_csv_gz,
                      out_weekday_groups_csv_prefix,
                      out_weekday_trips_csv_prefix) {
  
  suppressPackageStartupMessages(library(dplyr))
  
  # example parameter values
  #vista18PersonsCsv <- '../data/VISTA_12_18_CSV.zip.dir/P_VISTA1218_V1.csv'
  #filterCsv <- '../data/vistaCohorts.csv.gz'
  #setupDir <- '../output/1.setup'
  #out_weekday_persons_csv_gz <- '../output/1.setup/vista_2012_18_extracted_persons_weekday.csv.gz'
  #out_weekend_persons_csv_gz <- '../output/1.setup/vista_2012_18_extracted_persons_weekend.csv.gz'
  #out_weekday_groups_csv_prefix <- 'vista_2012_18_extracted_group_weekday_'
  #out_weekday_trips_csv_prefix <- 'vista_2012_18_extracted_trips_weekday_'
  
  extract_persons <- function(vista18PersonsCsv, out_weekday_persons_csv_gz, out_weekend_persons_csv_gz) {
    
    gz1 <- gzfile(vista18PersonsCsv,'rt')
    data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
    close(gz1)
    
    datacols<-c("PERSID",
                "AGE",
                "SEX",
                "WDPERSWGT",
                "WEPERSWGT")
    
    orig<-data[,datacols]
    
    # Split into weekday/weekend and set the weights (ie counts here) correctly
    week<-orig[,datacols]
    isWeekday<-!(is.na(week$WDPERSWGT) |  week$WDPERSWGT=='')
    weekdays<-week[isWeekday,]; weekdays$Count<- weekdays$WDPERSWGT
    weekends<-week[!isWeekday,]; weekends$Count<-weekends$WEPERSWGT
    
    # Fix any rows where the weights are not defined
    if(any(is.na(weekends$Count))) {
      weekends[is.na(weekends$Count),]$Count<-0
    }
    if(any(is.na(weekdays$Count))) {
      weekdays[is.na(weekdays$Count),]$Count<-0
    }
    
    weekdays$Count<-as.numeric(gsub(",", "", weekdays$Count)) # remove commas from Count
    weekends$Count<-as.numeric(gsub(",", "", weekends$Count)) # remove commas from Count
    
    # Write them out
    gz1 <- gzfile(out_weekday_persons_csv_gz, "w")
    write.csv(weekdays, gz1, row.names=FALSE, quote=TRUE)
    close(gz1)
    gz1 <- gzfile(out_weekend_persons_csv_gz, "w")
    write.csv(weekends, gz1, row.names=FALSE, quote=TRUE)
    close(gz1)
  }
  
  
  extract_groups <- function(weekday_persons_csv_gz, filterCsv, setupDir, out_weekday_groups_csv_prefix) {
    infile <- weekday_persons_csv_gz
    gz1 <- gzfile(infile,'rt')
    data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
    close(gz1)
    
    datacols<-c("PERSID",
                "AGE",
                "SEX",
                "Count")
    
    orig<-data[,datacols]
    
    infile <- filterCsv
    gz1 <- gzfile(infile,'rt')
    data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
    close(gz1)
    
    datacols<-c("sex",
                "min_age",
                "max_age",
                "cluster_id_5")
    
    filters <- data[,datacols] %>%
      group_by(cluster_id_5,sex) %>%
      summarise(age_start=min(min_age), age_end=max(max_age)) 
    
    groups <- unique(filters$cluster_id_5)
    # write csv header in new file first
    for (gid in groups) {
      outfile <- paste0(setupDir,"/",out_weekday_groups_csv_prefix,gid,".csv")
      echo(paste0('Creating empty file ',outfile,'\n'))
      write.table(orig[0,], outfile, row.names=FALSE, quote=TRUE, sep=",")
    }
    
    for (row in 1:nrow(filters)) {
      cohort <- orig %>%
        filter(AGE >= filters[row,]$age_start & AGE <= filters[row,]$age_end & SEX == filters[row,]$sex)
      outfile <- paste0(setupDir,"/",out_weekday_groups_csv_prefix,filters[row,]$cluster_id_5,".csv")
      echo(paste0('Appending to ',outfile,'\n'))
      write.table(cohort, outfile, row.names=FALSE, col.names=FALSE, quote=TRUE, sep=",", append=TRUE)
    }
  }
  
  extract_trips_groups<-function(vista18TripsCsv, 
                                 filterCsv, 
                                 setupDir, 
                                 weekday_groups_csv_prefix, 
                                 out_weekday_trips_csv_prefix
                                 ) {
    # example parameter values
    # setupDir <- '../output/1.setup'
    # vista18TripsCsv <- '../data/VISTA_12_18_CSV.zip.dir/T_VISTA1218_V1.csv'
    # filterCsv <- '../data/vistaCohorts.csv.gz'
    # weekday_groups_csv_prefix <- 'vista_2012_18_extracted_group_weekday_'
    # out_weekday_trips_csv_prefix <- 'vista_2012_18_extracted_trips_weekday_'
    
    groups<-getGroupIds(filterCsv)
    
    gz1 <- gzfile(vista18TripsCsv,'rt')
    vista_data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
    close(gz1)
    
    datacols<-c("PERSID",
                "ORIGPURP1",
                "DESTPURP1",
                "STARTIME","ARRTIME",
                "WDTRIPWGT",
                "WETRIPWGT")
    
    orig<-vista_data[,datacols]
    
    for (gid in groups) {
      infile<-paste0(setupDir,"/",weekday_groups_csv_prefix,gid,".csv")
      gz1 <- gzfile(infile,'rt')
      data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
      close(gz1)
      dd <- orig %>% filter(PERSID %in% data$PERSID)
      outfile <- paste0(setupDir,"/",out_weekday_trips_csv_prefix,gid,".csv")
      echo(paste0('Writing ',outfile,'\n'))
      write.table(dd, outfile, row.names=FALSE, col.names=TRUE, quote=TRUE, sep=",", append=FALSE)
    }
    
  }
  
  echo(paste0('Extracting VISTA persons into ',out_weekday_persons_csv_gz,' and ', out_weekend_persons_csv_gz,'\n'))
  extract_persons(vista18PersonsCsv, out_weekday_persons_csv_gz, out_weekend_persons_csv_gz)
  echo(paste0('Extracting VISTA groups based on ',filterCsv,'\n'))
  extract_groups(out_weekday_persons_csv_gz, filterCsv, setupDir, out_weekday_groups_csv_prefix)
  echo(paste0('Extracting VISTA trips groups based on ',filterCsv,'\n'))
  extract_trips_groups(vista18TripsCsv, filterCsv, setupDir, out_weekday_groups_csv_prefix, out_weekday_trips_csv_prefix) 
}

