# Function to pre-process some data; need only be run once
demand_setup<-function(setupDir, vista18TripsCsv) {
  
  dir.create(setupDir, showWarnings=FALSE, recursive=TRUE)
  
  # Extract VISTA activities and save separately into weekday and weekend activities
  vista_csv <- vista18TripsCsv
  out_weekday_activities_csv_gz <- paste0(setupDir,'/vista_2012_18_extracted_activities_weekday.csv.gz')
  out_weekend_activities_csv_gz <- paste0(setupDir,'/vista_2012_18_extracted_activities_weekend.csv.gz')
  echo(paste0('Extracting VISTA weekday/end activities from ', vista_csv, ' (can take a while)\n'))
  extract_and_write_activities_from(vista_csv, out_weekday_activities_csv_gz, out_weekend_activities_csv_gz)
  echo(paste0('Wrote ', out_weekday_activities_csv_gz, ' and ', out_weekend_activities_csv_gz,'\n'))
  
  # Simplify some activity classes to activity groups
  echo(paste0('Grouping some VISTA activities\n'))
  simplify_activities_and_create_groups(out_weekday_activities_csv_gz)
  echo(paste0('Updated ', out_weekday_activities_csv_gz,'\n'))
  simplify_activities_and_create_groups(out_weekend_activities_csv_gz)
  echo(paste0('Updated ', out_weekend_activities_csv_gz,'\n'))
  
  # Write out the activity probabilitities by time bins
  binsize<-48 # 30-min bins
  echo(paste0('Extracting VISTA weekday/end activities times into ',binsize,' bins (can take a while)\n'))
  out_weekday_activities_time_bins_csv_gz<-paste0(setupDir,'/vista_2012_18_extracted_activities_weekday_time_bins.csv.gz')
  out_weekend_activities_time_bins_csv_gz<-paste0(setupDir,'/vista_2012_18_extracted_activities_weekend_time_bins.csv.gz')
  in_activities_csv_gz<-out_weekday_activities_csv_gz
  out_csv_gz<-out_weekday_activities_time_bins_csv_gz
  extract_and_write_activities_time_bins(in_activities_csv_gz, out_csv_gz, binsize)
  in_activities_csv_gz<-out_weekend_activities_csv_gz
  out_csv_gz<-out_weekend_activities_time_bins_csv_gz
  extract_and_write_activities_time_bins(in_activities_csv_gz, out_csv_gz, binsize)
  echo(paste0('Wrote ', out_weekday_activities_time_bins_csv_gz, ' and ', out_weekend_activities_time_bins_csv_gz,'\n'))
  
  # Write out the activity end time probabilities for each staret time bin
  in_activities_csv_gz<-out_weekday_activities_csv_gz
  out_csv <- paste0(setupDir,'/vista_2012_18_extracted_activities_weekday_end_dist_for_start_bins.csv.gz')
  echo(paste0('Extracting VISTA weekday activities end times distributions for each start time bin into ',out_csv,'\n'))
  extract_and_write_activities_end_time_dist_by_start_bins(in_activities_csv_gz, out_csv, binsize)
    
  echo('Setup complete\n')
  return(TRUE)
}
