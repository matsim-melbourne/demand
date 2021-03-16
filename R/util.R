suppressPackageStartupMessages(library(dplyr))
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Probabilistically selects an index from the vector of probabilities
selectIndexFromProbabilities <-function(vv) {
  if(is.null(vv) || is.na(vv)) return(vv)
  if(length(vv)==0 || length(vv)==1) return(length(vv))
  vv<-vv/sum(vv) # normalise to 1
  v<-cumsum(vv) # cumulative sum to 1
  roll<-runif(1)
  select<-match(TRUE,v>roll) # pick the first col that is higher than the dice roll
  return(select)
}

# Timestamped console output
echo<- function(msg) {
  cat(paste0(as.character(Sys.time()), ' | ', msg))
}

# Progress bar
printProgress<-function(row, char, majorInterval=100, minorInterval=10) {
  if(is.null(row) || is.na(row) || !is.numeric(row)) return()
  if((row-1)%%majorInterval==0) echo('')
  cat(char)
  if(row%%minorInterval==0) cat('|')
  if(row%%majorInterval==0) cat(paste0(' ', row,'\n'))
}

getGroupIds<-function(filterCsv) {
  groups<- getGroups(filterCsv)
  groupIds <- unique(groups$cluster_id_5)
  return(groupIds)
}

getGroups<-function(filterCsv) {
  gz1 <- gzfile(filterCsv,'rt')
  data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
  close(gz1)
  
  datacols<-c("sex",
              "min_age",
              "max_age",
              "cluster_id_5")
  
  filters <- data[,datacols] %>%
    group_by(cluster_id_5,sex) %>%
    summarise(age_start=min(min_age), age_end=max(max_age)) 
  
  return(filters)
}
