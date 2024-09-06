suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(tidyr))
# suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(igraph))
outputDir="../dataWorkLocation/dataOutput"

collateMelbourne2016Population <- function(outputDir, plansFile=NA) {
  # df=persons_cleaned
  
  importPersons <- function(persons_csv_gz, sa1s = NULL) {
    #sampleSize<-10 #for testing purposes
    infile<-"../dataWorkLocation/Abbotsford/population/persons.csv.gz"
    # infile<-persons_csv_gz
    
    # read in the population
    gz1<-gzfile(infile, 'rt')
    all_persons<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T )
    close(gz1)
    
    # if we're restricting to a subset of nodes
    if (!is.null(sa1s)) all<-all%>%filter(SA1_7DIGCODE%in%sa1s)
    
    return(all_persons)
  }
  
  # read in the list of SA1s we want to keep
  sa1s <- NULL
  if(!is.na(plansFile)) {
    sa1s<-read.csv(plansFile)
    sa1s<-sa1s$SA1_7DIGCODE
  }
  
  # get all the Melbourne 2016 persons files by SA2
  df<-data.frame(SA2=list.files(path='data', pattern = "\\persons.csv.gz$", recursive = TRUE, full.names = TRUE), stringsAsFactors=FALSE)
  persons<-NULL
  echo(paste0("Collating the population from Melbourne's ", nrow(df), " SA2 areas (can take a while)\n"))
  for(row in 1:nrow(df)) {
    printProgress(row,".")
    persons<-rbind(persons,importPersons(df$SA2[row],sa1s))
  }
  cat('\n')
  
  # read in the SA1s file so we can attach the full code
  gz1<-gzfile('../dataWorkLocation/data/sa1_2016_aust.csv.gz', 'rt')
  sa1s<-read.csv(gz1, header=T, stringsAsFactors=F, strip.white=T) %>%
    dplyr::select(SA1_7DIGITCODE_2016,SA1_MAINCODE_2016) %>%
    mutate(SA1_7DIGITCODE_2016=as.integer(SA1_7DIGITCODE_2016),
           SA1_MAINCODE_2016=as.numeric(SA1_MAINCODE_2016))
  close(gz1)
  
  
  persons_cleaned <- persons %>%
    mutate(across(c(Gender,RelationshipStatus), ~ as.factor(.x))) %>%
    mutate(across(c(PartnerId,MotherId,FatherId,ChildrenIds,RelativeIds), ~ ifelse(.x=="",NA,.x))) %>%
    inner_join(sa1s, by=c("SA1_7DIGCODE"="SA1_7DIGITCODE_2016"))
  
  echo(paste0("Assigning households to ", nrow(persons_cleaned), " people (can take a while)\n"))
  persons_with_hh <- assignHHids(persons_cleaned)
  
  echo(paste0("Wrote ", nrow(persons), " sampled persons to ", outputDir, '\n'))
  saveRDS(persons_with_hh,paste0(outputDir,'/collatedPopulation.rds'))

}


reorderColumns <- function(df) {
  # df <- df_children %>% dplyr::select(x=AgentId,y=ChildrenIds)
  
  result <- df %>%
    mutate(from=ifelse(x<y,x,y)) %>%
    mutate(to  =ifelse(y>x,y,x)) %>%
    dplyr::select(from,to)
  return(result)
}

assignHHids <- function(df) {
  
  df2 <- df %>%
    rename(AgentIdOld=AgentId) %>%
    mutate(AgentId=row_number()) %>%
    mutate(ChildrenIds=str_replace_all(ChildrenIds, "\\[|\\]", "")) %>%
    mutate(RelativeIds=str_replace_all(ChildrenIds, "\\[|\\]", ""))
  
  df_ids <- df2 %>%
    dplyr::select(id=AgentId,AgentIdOld)
  
  df_children <- df2 %>%
    filter(!is.na(ChildrenIds)) %>%
    dplyr::select(AgentId,ChildrenIds) %>%
    mutate(ChildrenIds=str_split(ChildrenIds,pattern=", ")) %>%
    unnest(ChildrenIds) %>%
    left_join(df_ids, by=c("ChildrenIds"="AgentIdOld")) %>%
    dplyr::select(AgentId,ChildrenIds=id)
  
  df_relatives <- df2 %>%
    filter(!is.na(RelativeIds)) %>%
    dplyr::select(AgentId,RelativeIds) %>%
    mutate(RelativeIds=str_split(RelativeIds,pattern=", ")) %>%
    unnest(RelativeIds) %>%
    left_join(df_ids, by=c("RelativeIds"="AgentIdOld")) %>%
    dplyr::select(AgentId,RelativeIds=id)
  
  df_partner <- df2 %>%
    filter(!is.na(PartnerId)) %>%
    dplyr::select(AgentId,PartnerId) %>%
    left_join(df_ids, by=c("PartnerId"="AgentIdOld")) %>%
    dplyr::select(AgentId,PartnerId=id)
  
  df_mother <- df2 %>%
    filter(!is.na(MotherId)) %>%
    dplyr::select(AgentId,MotherId) %>%
    left_join(df_ids, by=c("MotherId"="AgentIdOld")) %>%
    dplyr::select(AgentId,MotherId=id)
  
  df_father <- df2 %>%
    filter(!is.na(FatherId)) %>%
    dplyr::select(AgentId,FatherId) %>%
    left_join(df_ids, by=c("FatherId"="AgentIdOld")) %>%
    dplyr::select(AgentId,FatherId=id)
  
  all_relationships <- bind_rows(
    df_children  %>% dplyr::select(x=AgentId,y=ChildrenIds) %>% reorderColumns(),
    df_relatives %>% dplyr::select(x=AgentId,y=RelativeIds) %>% reorderColumns(),
    df_partner   %>% dplyr::select(x=AgentId,y=PartnerId  ) %>% reorderColumns(),
    df_mother    %>% dplyr::select(x=AgentId,y=MotherId   ) %>% reorderColumns(),
    df_father    %>% dplyr::select(x=AgentId,y=FatherId   ) %>% reorderColumns()
  ) %>%
    distinct()
  
  # Ids of people with families
  all_relationships_ids <- base::unique(c(all_relationships$from,all_relationships$to))
  no_relationship_ids <- df2 %>%
    dplyr::select(AgentId) %>%
    filter(!AgentId%in%all_relationships_ids) %>%
    mutate(HouseholdId=row_number())

  # Making the graph for the relationships
  g <- graph_from_data_frame(all_relationships, vertices = all_relationships_ids, directed = FALSE) 
  #plot(g,  vertex.size=0.1, vertex.label=NA, vertex.color="red", edge.arrow.size=0, edge.curved = 0)
  
  # Getting components
  comp <- components(g)
  comp_df <- data.frame(AgentId=as.integer(names(comp$membership)),
                        HouseholdId=comp$membership+nrow(no_relationship_ids), row.names=NULL)
  
  df_households <- bind_rows(no_relationship_ids,comp_df)
  
  # df_children_compressed <- df_children %>%
  #   group_by(AgentId) %>%
  #   summarize(ChildrenIds = list(ChildrenIds))
  
  df_children_compressed <- df_children %>%
    group_by(AgentId) %>%
    summarise(ChildrenIds=paste(ChildrenIds, collapse=',')) %>%
    ungroup() %>%
    distinct()
  
  df_relatives_compressed <- df_relatives %>%
    group_by(AgentId) %>%
    summarize(RelativeIds = paste(RelativeIds, collapse=",")) %>%
    ungroup()
  
  df_final <- df2 %>%
    dplyr::select(AgentIdOld,AgentId,Age,Gender,RelationshipStatus,SA2_MAINCODE,SA1_7DIGCODE,SA1_MAINCODE_2016) %>%
    left_join(df_households,by="AgentId") %>%
    left_join(df_partner,by="AgentId") %>%
    left_join(df_mother,by="AgentId") %>%
    left_join(df_father,by="AgentId") %>%
    left_join(df_children_compressed,by="AgentId") %>%
    left_join(df_relatives_compressed,by="AgentId") %>%
    dplyr::select(AgentIdOld,AgentId,Age,Gender,RelationshipStatus,HouseholdId,
                  PartnerId, MotherId, FatherId, ChildrenIds,RelativeIds,
                  SA2_MAINCODE,SA1_7DIGCODE,SA1_MAINCODE_2016)
  
  return(df_final)
}

