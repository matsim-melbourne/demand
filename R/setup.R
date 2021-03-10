# Function to pre-process some data; need only be run once
demand_setup<-function(setupDir, 
                       vista18TripsCsv,
                       out_weekday_activities_csv_gz,
                       out_weekend_activities_csv_gz,
                       out_weekday_activities_time_bins_csv_gz,
                       out_weekend_activities_time_bins_csv_gz,
                       out_csv
                       ) {
  # example parameter values
  # setupDir <- '../output/1.setup'
  # vista18TripsCsv <- '../data/VISTA_12_18_CSV.zip.dir/T_VISTA1218_V1.csv'
  # out_weekday_activities_csv_gz <- paste0(setupDir,'/vista_2012_18_extracted_activities_weekday.csv.gz')
  # out_weekend_activities_csv_gz <- paste0(setupDir,'/vista_2012_18_extracted_activities_weekend.csv.gz')
  # out_weekday_activities_time_bins_csv_gz<-paste0(setupDir,'/vista_2012_18_extracted_activities_weekday_time_bins.csv.gz')
  # out_weekend_activities_time_bins_csv_gz<-paste0(setupDir,'/vista_2012_18_extracted_activities_weekend_time_bins.csv.gz')
  # out_csv <- paste0(setupDir,'/vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins.csv.gz')
  
  # Extract VISTA activities and save separately into weekday and weekend activities
  vista_csv <- vista18TripsCsv
  echo(paste0('Extracting VISTA weekday/end activities from ', vista_csv, ' (can take a while)\n'))
  extract_and_write_activities_from(vista_csv, out_weekday_activities_csv_gz, out_weekend_activities_csv_gz)
  echo(paste0('Wrote ', out_weekday_activities_csv_gz, ' and ', out_weekend_activities_csv_gz,'\n'))
  
  # Simplify some activity classes to activity groups
  echo(paste0('Grouping some VISTA activities\n'))
  simplify_activities_and_create_groups(out_weekday_activities_csv_gz)
  echo(paste0('Updated ', out_weekday_activities_csv_gz,'\n'))
  simplify_activities_and_create_groups(out_weekend_activities_csv_gz)
  echo(paste0('Updated ', out_weekend_activities_csv_gz,'\n'))
  
  # Write out the activity probabilities by time bins
  binsize<-48 # 30-min bins
  echo(paste0('Extracting VISTA weekday/end activities times into ',binsize,' bins (can take a while)\n'))
  in_activities_csv_gz<-out_weekday_activities_csv_gz
  out_csv_gz<-out_weekday_activities_time_bins_csv_gz
  extract_and_write_activities_time_bins(in_activities_csv_gz, out_csv_gz, binsize)
  in_activities_csv_gz<-out_weekend_activities_csv_gz
  out_csv_gz<-out_weekend_activities_time_bins_csv_gz
  extract_and_write_activities_time_bins(in_activities_csv_gz, out_csv_gz, binsize)
  echo(paste0('Wrote ', out_weekday_activities_time_bins_csv_gz, ' and ', out_weekend_activities_time_bins_csv_gz,'\n'))
  
  # Write out the activity end time probabilities for each start time bin
  in_activities_csv_gz<-out_weekday_activities_csv_gz
  echo(paste0('Extracting VISTA weekday activities end times distributions for each start time bin into ',out_csv,'\n'))
  extract_and_write_activities_end_time_dist_by_start_bins(in_activities_csv_gz, out_csv, binsize)
    
  echo('Setup complete\n')
  return(TRUE)
}

locations_setup<-function(setupDir, 
                          distanceMatrixFile, 
                          distanceMatrixIndexFile,
                          sa1AttributedFile,
                          sa1CentroidsFile,
                          addressesFile,
                          distancesFile,
                          plansFile=NULL) {
  
  dir.create(setupDir, showWarnings=FALSE, recursive=TRUE)
  
  # check if we want to keep only known SA1s from plans file (useful for testing)
  filterSa1s <- !is.null(plansFile)
  
  # read in the list of SA1s we want to keep
  sa1s <- vector()
  if(filterSa1s) {
    sa1s<-read.csv(plansFile)
    sa1s<-sa1s$SA1_MAINCODE_2016
  }
  
  # Extract the distance matrix index subset containing the required SA1s
  echo(paste0("Reading ", distanceMatrixIndexFile, "\n"))
  dmi <- read.csv(distanceMatrixIndexFile)
  if (filterSa1s) dmi <- dmi[dmi$sa1_maincode_2016 %in% sa1s, ]
  
  # Extract the distance matrix index subset containing the required SA1s
  echo(paste0("Reading ", distanceMatrixFile, "\n"))
  dm <- readRDS(distanceMatrixFile)
  if (filterSa1s) dm <- dm[dmi$index,dmi$index]
  outfile<-paste0(setupDir,"/locDistanceMatrix.rds")
  echo(paste0("Writing ", outfile, "\n"))
  saveRDS(dm, outfile)
  
  df<-dmi
  # Update the indices if needed and write them out
  if (filterSa1s) df$index<-seq(1,length(df$index))
  outfile<-paste0(setupDir,"/locDistanceMatrixIndex.rds")
  echo(paste0("Writing ", outfile, "\n"))
  saveRDS(df, outfile)
  
  # Reading in the attributed SA1 regions. I'm removing the geometry since it
  # won't be used here. Joining with the distance matrix index so the regions are
  # in the correct order.
  echo(paste0("Reading ", sa1AttributedFile, "\n"))
  sa1Aattributed <- inner_join(dmi,
                               read.csv(gzfile(sa1AttributedFile)),
                               by="sa1_maincode_2016")
  # Need to make sure that the probabilities sum to 1
  sa1Aattributed <- sa1Aattributed %>%
    mutate(home=home/sum(home,na.rm=T),
           work=work/sum(work,na.rm=T),
           park=park/sum(park,na.rm=T),
           education=education/sum(education,na.rm=T),
           commercial=commercial/sum(commercial,na.rm=T))
  
  outfile<-paste0(setupDir,"/locSa1Aattributed.rds")
  echo(paste0("Writing ", outfile, "\n"))
  saveRDS(sa1Aattributed, outfile)
  
  # Reading in the addresses.
  # These coordinates are in EPSG:28355, which is a projected coordinate system.
  echo(paste0("Reading ", addressesFile, "\n"))
  addresses <- read.csv(gzfile(addressesFile))
  if (filterSa1s) addresses <- addresses%>%filter(sa1_maincode_2016%in%sa1s)
  outfile<-paste0(setupDir,"/locAddresses.rds")
  echo(paste0("Writing ", outfile, "\n"))
  saveRDS(addresses, outfile)
  
  echo(paste0("Reading ", sa1CentroidsFile, "\n"))
  sa1Centroids <- read.csv(gzfile(sa1CentroidsFile))
  if (filterSa1s) sa1Centroids <- sa1Centroids%>%filter(sa1_maincode_2016%in%sa1s)
  outfile<-paste0(setupDir,"/locSa1Centroids.rds")
  echo(paste0("Writing ", outfile, "\n"))
  saveRDS(sa1Centroids, outfile)
  
  echo(paste0("Reading ", distancesFile, "\n"))
  expectedDistances <- readRDS(distancesFile)
  outfile<-paste0(setupDir,"/expectedDistances.rds")
  echo(paste0("Writing ", outfile, "\n"))
  saveRDS(expectedDistances, outfile)
  
}

