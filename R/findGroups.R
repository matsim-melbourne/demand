# load libraries and data -------------------------------------------------
vista18TripsCsv <- '../data/VISTA_12_18_CSV.zip.dir/T_VISTA1218_V1.csv'
vista18PersonCsv <- '../data/VISTA_12_18_CSV.zip.dir/P_VISTA1218_V1.csv'

suppressPackageStartupMessages(library(cluster))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))

gz1 <- gzfile(vista18TripsCsv,'rt')
vista_data<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
close(gz1)

gz1 <- gzfile(vista18PersonCsv,'rt')
vista_data_persons<-read.csv(gz1,header = T,sep=',',stringsAsFactors = F,strip.white = T)
close(gz1)



# clean data --------------------------------------------------------------

# age groups we want to investigate
persons <- vista_data_persons %>%
  dplyr::select("PERSID",age="AGE",sex="SEX") %>%
  mutate(age_group=case_when(
    age>=0  & age<=14 ~ '0-14',
    age>=15 & age<=19 ~'15-19',
    age>=20 & age<=24 ~'20-24',
    age>=25 & age<=29 ~'25-29',
    age>=30 & age<=34 ~'30-34',
    age>=35 & age<=39 ~'35-39',
    age>=40 & age<=44 ~'40-44',
    age>=45 & age<=49 ~'45-49',
    age>=50 & age<=54 ~'50-54',
    age>=55 & age<=59 ~'55-59',
    age>=60 & age<=64 ~'60-64',
    age>=65           ~'65plus')) %>%
  dplyr::select(PERSID,age_group,sex)
  
vista_data_weekday <- vista_data %>%
  filter(!is.na(WDTRIPWGT) & WDTRIPWGT!='') %>%
  dplyr::select("PERSID","DESTPURP1","LINKMODE","WDTRIPWGT") %>%
  mutate(WDTRIPWGT=as.numeric(WDTRIPWGT)) %>%
  # using same logic as vista.R, although some of these will be deleted
  mutate(Activity=case_when(
    DESTPURP1=="At Home" ~ 'Home',
    DESTPURP1=="Go Home" ~ 'Home',
    DESTPURP1=="Personal Business" ~ 'Personal',
    DESTPURP1=="Work Related" ~ 'Work',
    DESTPURP1=="Change Mode" ~ 'Mode Change',
    DESTPURP1=="Accompany Someone" ~ 'With Someone',
    DESTPURP1=="Education" ~ 'Study',
    DESTPURP1=="Buy Something" ~ 'Shop',
    DESTPURP1=="Unknown Purpose (at start of day)" ~ 'Home',
    DESTPURP1=="Other Purpose" ~ 'Other',
    DESTPURP1=="Not Stated" ~ 'Other',
    DESTPURP1=="Social" ~ 'Social/Recreational',
    DESTPURP1=="Recreational" ~ 'Social/Recreational',
    DESTPURP1=="Pick-up or Drop-off Someone" ~ 'Shop',
    DESTPURP1=="Pick-up or Deliver Something" ~ 'Pickup/Dropoff/Deliver')) %>%
  mutate(transport_mode = case_when(
    LINKMODE %in% c("Vehicle Driver","Vehicle Passenger","Taxi","Motorcycle")  ~ 'car',
    LINKMODE == "Walking"             ~ 'walk',
    LINKMODE == "Bicycle"             ~ 'bike',
    LINKMODE %in% c("Train","Public Bus","School Bus","Tram") ~ 'pt'
  )) %>%
  filter(!is.na(transport_mode)) %>%
  dplyr::select(PERSID,Activity,transport_mode,WDTRIPWGT)


# joining trips to age/sex demographics
vista_data_cohorts <- vista_data_weekday %>%
  filter(Activity%in%c('Work','Study','Shop','Personal','Social/Recreational','Pickup/Dropoff/Deliver')) %>%
  inner_join(persons, by="PERSID") %>%
  mutate(id=paste0(sex,'_',age_group)) %>%
  dplyr::select(id,Activity,transport_mode,WDTRIPWGT)

percent_activity <- vista_data_cohorts %>%
  group_by(Activity,id) %>%
  summarise(count=sum(WDTRIPWGT,na.rm=T)) %>%
  group_by(id) %>%
  mutate(group_total=sum(count),
         percentage=count/group_total) %>%
  dplyr::select(id,Activity,percentage) %>%
  ungroup() %>%
  pivot_wider(names_from=Activity, values_from=percentage)

percent_mode <- vista_data_cohorts %>%
  group_by(transport_mode,id) %>%
  summarise(count=sum(WDTRIPWGT,na.rm=T)) %>%
  group_by(id) %>%
  mutate(group_total=sum(count),
         percentage=count/group_total) %>%
  dplyr::select(id,transport_mode,percentage) %>%
  ungroup() %>%
  pivot_wider(names_from=transport_mode, values_from=percentage)

vista_data_cohorts2 <- inner_join(percent_activity,percent_mode,by="id") 


# the clustering functions work on labeled matrices
cohorts <- vista_data_cohorts2 %>%
  dplyr::select('Work','Study','Shop','Personal','Social/Recreational') %>%
  as.matrix()
rownames(cohorts)<-vista_data_cohorts2$id

cohortsMode <- vista_data_cohorts2 %>%
  dplyr::select('Work','Study','Shop','Personal','Social/Recreational','bike','car','pt','walk') %>%
  as.matrix()
rownames(cohortsMode)<-vista_data_cohorts2$id



# k-means clustering ------------------------------------------------------

# # making 4  clusters
# clusters<-kmeans(cohorts, 4, iter.max = 100, nstart = 4)
# 
# # visualising the clusters
# fviz_cluster(clusters, data = cohorts)
# ggsave("clusterPlot.pdf",width=8,height=6)

# how many clusters should we use?
fviz_nbclust(cohorts, kmeans, method = "gap_stat")
ggsave("gapStatistic.pdf",width=8,height=6)
fviz_nbclust(cohortsMode, kmeans, method = "gap_stat")
ggsave("gapStatisticWithMode.pdf",width=8,height=6)



# Hierarchical clustering -------------------------------------------------



# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(cohorts, method = x)$ac
}

map_dbl(m, ac)
# average    single  complete      ward 
# 0.9055169 0.8861145 0.9207356 0.9605694 
# ward is the best

hc <- agnes(cohorts, method = "ward")
# Cut tree into groups
sub_grp4 <- cutree(hc, k = 4)
sub_grp5 <- cutree(hc, k = 5)
sub_grp6 <- cutree(hc, k = 6)
sub_grp7 <- cutree(hc, k = 7)

# Number of members in each cluster
table(sub_grp4)
table(sub_grp5)
table(sub_grp6)

pdf("dendrogram.pdf",width=8,height=6) # The height of the plot in inches
pltree(hc, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
rect.hclust(hc, k = 7, border = 2:5)
dev.off()

fviz_cluster(list(data = cohorts, cluster = sub_grp4))
ggsave("clusterPlotHierarchical4.pdf",width=8,height=6)
fviz_cluster(list(data = cohorts, cluster = sub_grp5))
ggsave("clusterPlotHierarchical5.pdf",width=8,height=6)
fviz_cluster(list(data = cohorts, cluster = sub_grp6))
ggsave("clusterPlotHierarchical6.pdf",width=8,height=6)
fviz_cluster(list(data = cohorts, cluster = sub_grp7))
ggsave("clusterPlotHierarchical7.pdf",width=8,height=6)


# Hierarchical clustering with mode ---------------------------------------

hcMode <- agnes(cohortsMode, method = "ward")

# Cut tree into groups
sub_grp4_mode <- cutree(hcMode, k = 4)
sub_grp5_mode <- cutree(hcMode, k = 5)
sub_grp6_mode <- cutree(hcMode, k = 6)
sub_grp7_mode <- cutree(hcMode, k = 7)



pdf("dendrogramMode.pdf",width=8,height=6) # The height of the plot in inches
pltree(hcMode, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
rect.hclust(hc, k = 5, border = 2:5)
dev.off()

fviz_cluster(list(data = cohortsMode, cluster = sub_grp4_mode))
ggsave("clusterPlotHierarchical4withMode.pdf",width=8,height=6)
fviz_cluster(list(data = cohortsMode, cluster = sub_grp5_mode))
ggsave("clusterPlotHierarchical5withMode.pdf",width=8,height=6)
fviz_cluster(list(data = cohortsMode, cluster = sub_grp6_mode))
ggsave("clusterPlotHierarchical6withMode.pdf",width=8,height=6)
fviz_cluster(list(data = cohortsMode, cluster = sub_grp7_mode))
ggsave("clusterPlotHierarchical7withMode.pdf",width=8,height=6)

# 
# 
# pca<-prcomp(cohorts)
# 
# fviz_pca_ind(pca, pointsize = "cos2", 
#              pointshape = 21, fill = "#E7B800",
#              repel = TRUE # Avoid text overlapping (slow if many points)
# )
# 
# ggplot(vista_data_cohorts, aes(x=Study, y=Work)) +
#   # geom_point(aes(color=Shop))+
#   geom_text(aes(label=id))
