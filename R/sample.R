library(data.table)

sampleMelbourne2016Population <- function(dataDir, samplePercentage, outcsvgz, plansFile = NULL) {
  
  assignSa1Maincode <- function(persons_csv_gz, out_persons_csv_gz, sa1_csv_gz) {
    # read in the SA1s file
    gz1<-gzfile(sa1_csv_gz, 'rt')
    sa1s<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
    close(gz1)
    sa1s$SA1_MAINCODE_2016<-as.numeric(sa1s$SA1_MAINCODE_2016)
    sa1s$SA1_7DIGITCODE_2016<-as.numeric(sa1s$SA1_7DIGITCODE_2016)
    sa1s_dt<-data.table(sa1s)
    setkey(sa1s_dt, SA1_7DIGITCODE_2016, SA2_MAINCODE_2016)
    
    # read in the persons
    gz1<-gzfile(persons_csv_gz, 'rt')
    persons<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T)
    close(gz1)
    persons$SA1_7DIGCODE<-as.numeric(persons$SA1_7DIGCODE)
    persons$SA2_MAINCODE<-as.numeric(persons$SA2_MAINCODE)
    
    # create a new column for SA1_MAINCODE_2016
    #persons$SA1_MAINCODE_2016<-""
    
    # match and assign
    df<-apply(persons, 1, function(p) {
      #sa1<-sa1s[sa1s$SA1_7DIGITCODE_2016==p['SA1_7DIGCODE'] & sa1s$SA2_MAINCODE_2016==p['SA2_MAINCODE'],]
      sa1<-sa1s_dt[.(as.numeric(p['SA1_7DIGCODE']),as.numeric(p['SA2_MAINCODE']))]
      p['SA1_MAINCODE_2016']<-sa1$SA1_MAINCODE_2016
      p
    })
    df<-t(df)
    df<-as.data.frame(df)
    write.csv(df, file=gzfile(out_persons_csv_gz), quote=TRUE, row.names = FALSE)
  }
  
  samplePersons <- function(persons_csv_gz, samplePercent = NULL, sa1s = NULL) {
    #sampleSize<-10 #for testing purposes
    #infile<-'persons/melbourne-2016-population.persons.csv.gz'
    infile<-persons_csv_gz
    
    # read in the population
    gz1<-gzfile(infile, 'rt')
    all<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T )
    close(gz1)
    
    # if we're restricting to a subset of nodes
    if (!is.null(sa1s)) all<-all%>%filter(SA1_7DIGCODE%in%sa1s)
    
    # get the number of persons in this population file  
    rows<-as.numeric(nrow(all))
    
    if (is.null(samplePercent)) {
      # default sample size is the full set
      percent = rows
    } else {
      # clip to within 0-100 %
      percent<-max(min(percent,100),0) 
    }
    sampleSize<-round(rows*(percent/100.0))

    # sample the required number of persons from the population
    if (sampleSize == rows) {
      sampleSet = all
    } else {
      sampleSet<-all[sample(nrow(all), sampleSize),]
    }
    
    sampleSet<-sampleSet[order(as.numeric(rownames(sampleSet))),]
    return(sampleSet)
  }
  
  # read in the list of SA1s we want to keep
  sa1s <- NULL
  if(!is.null(plansFile)) {
    sa1s<-read.csv(plansFile)
    sa1s<-sa1s$SA1_7DIGCODE
  }
  
  # get all the Melbourne 2016 persons files by SA2
  df<-data.frame(SA2=list.files(path=dataDir, pattern = "\\persons.csv.gz$", recursive = TRUE, full.names = TRUE), stringsAsFactors=FALSE)
  df$samplePercent<-samplePercentage
  persons<-NULL
  echo(paste0("Selecting a ", samplePercentage, "% population sample from Melbourne's ", nrow(df), " SA2 areas (can take a while)\n"))
  for(row in 1:nrow(df)) {
    printProgress(row,".")
    persons<-rbind(persons,samplePersons(df$SA2[row], df$samplePercent[row],sa1s))
  }
  cat('\n')
  echo(paste0("Wrote ", nrow(persons), " sampled persons to ", outcsvgz, '\n'))
  write.csv(persons, file=gzfile(outcsvgz), quote=TRUE, row.names = FALSE)
  
  # Fix their home location SA1 code (convert from SA1_7DIGCODE to SA1_MAINCODE_2016)
  echo(paste0('Assigning SA1_MAINCODE_2016 to persons in ', outcsvgz, ' (can take a while)\n'))
  assignSa1Maincode(outcsvgz, outcsvgz, paste0(dataDir,'/sa1_2016_aust.csv.gz')) # overwriting outfile
  echo(paste0('Updated ', outcsvgz,'\n'))
  
}

countMelbourne2016Population <- function(dataDir, plansFile=NULL) {
  # read in the list of SA1s we want to keep
  sa1s <- NULL
  if(!is.null(plansFile)) {
    sa1s<-read.csv(plansFile)
    sa1s<-sa1s$SA1_7DIGCODE
  }
  
  # get all the Melbourne 2016 persons files by SA2
  df<-data.frame(SA2=list.files(path=dataDir, pattern = "\\persons.csv.gz$", recursive = TRUE, full.names = TRUE), stringsAsFactors=FALSE)
  persons<-0
  for(row in 1:nrow(df)) {
    # read in the population
    gz1<-gzfile(df$SA2[row], 'rt')
    all<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T )
    close(gz1)
    
    # if we're restricting to a subset of nodes
    if (!is.null(sa1s)) all<-all%>%filter(SA1_7DIGCODE%in%sa1s)
    
    persons<- persons + as.numeric(nrow(all))
  }
 return(persons) 
}