sampleMelbourne2016Population <- function(dataDir, samplePercentage, outcsvgz, plansFile=NA) {
  
  assignSa1Maincode <- function(persons_csv_gz, out_persons_csv_gz, sa1_csv_gz) {
    # read in the SA1s file
    gz1<-gzfile(sa1_csv_gz, 'rt')
    sa1s<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
    close(gz1)
    sa1s$SA1_MAINCODE_2016<-as.numeric(sa1s$SA1_MAINCODE_2016)
    sa1s$SA1_7DIGITCODE_2016<-as.numeric(sa1s$SA1_7DIGITCODE_2016)
    # read in the persons
    gz1<-gzfile(persons_csv_gz, 'rt')
    persons<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
    close(gz1)
    persons$SA1_7DIGCODE<-as.numeric(persons$SA1_7DIGCODE)
    persons$SA2_MAINCODE<-as.numeric(persons$SA2_MAINCODE)
    
    person_keys<-paste(persons$SA1_7DIGCODE,persons$SA2_MAINCODE,sep=':')
    sa1_keys<-paste(sa1s$SA1_7DIGITCODE_2016,sa1s$SA2_MAINCODE_2016,sep=':')
    matches<-match(person_keys,sa1_keys)
    if(any(is.na(matches))) {
      stop('Could not assign SA1_MAINCODE_2016 to every sampled person')
    }
    persons$SA1_MAINCODE_2016<-sa1s$SA1_MAINCODE_2016[matches]
    write.csv(persons, file=gzfile(out_persons_csv_gz), quote=TRUE, row.names = FALSE)
  }
  
  sampleHouseholds <- function(persons_csv, samplePercent = NULL, sa1s = NULL) {
    household_files<-file.path(dirname(persons_csv),c('households.csv.gz','households.csv'))
    household_file<-household_files[file.exists(household_files)][1]
    if(is.na(household_file)) {
      stop(paste0('No households.csv or households.csv.gz found beside ',persons_csv))
    }

    all_persons<-read.csv(persons_csv,header=T,stringsAsFactors=F,strip.white=T)
    all_households<-read.csv(household_file,header=T,stringsAsFactors=F,strip.white=T)
    required_household_columns<-c('HouseholdId','HouseholdSize','Members','SA2_MAINCODE','SA1_7DIGCODE')
    missing_columns<-setdiff(required_household_columns,colnames(all_households))
    if(length(missing_columns)>0) {
      stop(paste0('Household file is missing columns: ',paste(missing_columns,collapse=', ')))
    }

    member_ids<-lapply(all_households$Members,function(members) {
      members<-gsub('^\\[|\\]$','',members)
      if(nchar(trimws(members))==0) return(character())
      trimws(strsplit(members,',',fixed=TRUE)[[1]])
    })
    membership<-data.frame(
      AgentId=unlist(member_ids,use.names=FALSE),
      HouseholdId=rep(all_households$HouseholdId,lengths(member_ids)),
      stringsAsFactors=FALSE
    )
    if(anyDuplicated(membership$AgentId)>0) {
      stop(paste0('A person belongs to more than one household in ',household_file))
    }
    household_index<-match(all_persons$AgentId,membership$AgentId)
    if(any(is.na(household_index))) {
      stop(paste0('Some persons are missing from household membership in ',household_file))
    }
    all_persons$HouseholdId<-membership$HouseholdId[household_index]

    person_household_index<-match(all_persons$HouseholdId,all_households$HouseholdId)
    all_persons$HouseholdSize<-all_households$HouseholdSize[person_household_index]
    all_persons$SA2_MAINCODE<-all_households$SA2_MAINCODE[person_household_index]
    all_persons$SA1_7DIGCODE<-all_households$SA1_7DIGCODE[person_household_index]

    eligible_households<-all_households
    if(!is.null(sa1s)) {
      eligible_households<-eligible_households[eligible_households$SA1_7DIGCODE%in%sa1s,]
    }
    percent<-if(is.null(samplePercent)) 100 else max(min(samplePercent,100),0)
    sample_size<-round(nrow(eligible_households)*(percent/100.0))
    if(sample_size==nrow(eligible_households)) {
      sampled_household_ids<-eligible_households$HouseholdId
    } else if(sample_size==0) {
      sampled_household_ids<-character()
    } else {
      sampled_household_ids<-sample(eligible_households$HouseholdId,sample_size)
    }

    sample_set<-all_persons[all_persons$HouseholdId%in%sampled_household_ids,]
    return(sample_set)
  }
  
  # read in the list of SA1s we want to keep
  sa1s <- NULL
  if(!is.na(plansFile)) {
    sa1s<-read.csv(plansFile)
    sa1s<-sa1s$SA1_7DIGCODE
  }
  
  # get all the Melbourne 2016 persons files by SA2
  df<-data.frame(SA2=list.files(path=dataDir, pattern = "\\persons\\.csv(\\.gz)?$", recursive = TRUE, full.names = TRUE), stringsAsFactors=FALSE)
  df$samplePercent<-samplePercentage
  persons<-NULL
  echo(paste0("Selecting complete households for a ", samplePercentage, "% population sample from Melbourne's ", nrow(df), " SA2 areas (can take a while)\n"))
  for(row in 1:nrow(df)) {
    printProgress(row,".")
    persons<-rbind(persons,sampleHouseholds(df$SA2[row],df$samplePercent[row],sa1s))
  }
  cat('\n')
  echo(paste0("Selected ",length(unique(persons$HouseholdId))," complete households containing ",nrow(persons)," persons\n"))
  echo(paste0("Wrote ",nrow(persons)," sampled persons to ",outcsvgz,'\n'))
  write.csv(persons, file=gzfile(outcsvgz), quote=TRUE, row.names = FALSE)
  
  # Fix their home location SA1 code (convert from SA1_7DIGCODE to SA1_MAINCODE_2016)
  echo(paste0('Assigning SA1_MAINCODE_2016 to persons in ', outcsvgz, ' (can take a while)\n'))
  sa1_files<-file.path(dataDir,c('sa1_2016_aust.csv.gz','sa1_2016_aust.csv'))
  sa1_file<-sa1_files[file.exists(sa1_files)][1]
  if(is.na(sa1_file)) stop(paste0('No SA1 lookup file found in ',dataDir))
  assignSa1Maincode(outcsvgz,outcsvgz,sa1_file) # overwriting outfile
  echo(paste0('Updated ', outcsvgz,'\n'))
  
}
