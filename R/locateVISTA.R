# outdir<-'../output/5.locate'

# Libraries and functions -------------------------------------------------
suppressPackageStartupMessages(library(sf)) # for spatial things
suppressPackageStartupMessages(library(lwgeom)) # for advanced spatial things
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(haven)) # for spss reading
suppressPackageStartupMessages(library(ggplot2)) # for plotting data
suppressPackageStartupMessages(library(fitdistrplus)) # for log normal distributions
suppressPackageStartupMessages(library(purrr)) # for nested dataframes
# Expected: VISTA
# Actual: VP(virtual population)

analyseLocate <- function(outdir) {
  
  # Functions ---------------------------------------------------------------
  lognormal_fun <- function(df) {
    result<-data.frame(logmean=NA,logsd=NA,count=NA)
    if(df$centroid_distance%>%unique()%>%length()>5) {
      modelEstimate<-fitdist(df$centroid_distance,weights=as.integer(df$weight), distr="lnorm")$estimate
      result<-data.frame(logmean=as.numeric(modelEstimate[1]),
                         logsd=as.numeric(modelEstimate[2]),
                         count=nrow(df))
    }
    return(result)
  }
  
  aggregateData <- function(df,binwidth,max_dist=Inf) {
    # binwidth=400
    df<-df %>%
      mutate(centroid_distance=ifelse(centroid_distance<1,1,centroid_distance))
    maxDist <- max(df$centroid_distance)
    
    df_aggregated <- df %>%
      mutate(distance=findInterval(centroid_distance,seq(0,maxDist,binwidth))) %>%
      mutate(distance=distance*binwidth-(binwidth*0.5)) %>%
      group_by(distance) %>%
      summarise(count=sum(weight,na.rm=T)) %>%
      mutate(proportion=count/sum(count,na.rm=T)) %>%
      dplyr::select(distance,proportion) %>%
      filter(distance<max_dist)
  }
  
  makeLine <- function(binwidth,max_dist,logmean,logsd) {
    df<-data.frame(distance=seq(0,max_dist,binwidth*0.2)) %>%
      mutate(proportion=dlnorm(distance,meanlog=logmean,sdlog=logsd) * binwidth)
    return(df)
  }
  
  
  # Read data ---------------------------------------------------------------
  sa1 <- st_read("../data/absRegionsReprojected.sqlite",
                 layer="sa1_2016_aust") %>%
    st_drop_geometry() %>%
    dplyr::select(sa1_maincode_2016,sa3_code_2016)
  
  vpTrips <- read.csv(paste0(outdir,"/plan.csv")) %>%
    dplyr::select(planid=PlanId,origin_sa1=SA1_MAINCODE_2016,ArrivingMode,LocationType,
                  centroid_distance=Distance) %>%
    mutate(transport_mode=lead(ArrivingMode),
           destination_sa1=lead(origin_sa1),
           destination_type=lead(LocationType)) %>%
    filter(!is.na(transport_mode)) %>%
    dplyr::select(planid,transport_mode,origin_type=LocationType,destination_type,
                  centroid_distance,origin_sa1,destination_sa1)
  
  vpTripsSA1 <- vpTrips %>%
    inner_join(sa1, by=c('origin_sa1'='sa1_maincode_2016')) %>%
    mutate(centroid_distance=ifelse(centroid_distance<1,1,centroid_distance)) %>%
    mutate(weight=1) %>%
    dplyr::select(planid,transport_mode,origin_type,destination_type,
                  centroid_distance,weight,sa1_maincode_2016=origin_sa1,
                  sa3_code_2016)
  
  
  
  # Distance histograms -----------------------------------------------------
  histogramAttributes <- data.frame(
    transport_mode=c('walk', 'bike',  'pt', 'car'),
    binwidth      =c(   400,   2000,  5000,  5000),
    max_dist      =c(  5000,  20000, 60000, 60000)
  )
  vpDistanceHistograms <- vpTripsSA1 %>%
    dplyr::select(transport_mode,centroid_distance,weight) %>%
    group_by(transport_mode) %>%
    nest() %>%
    inner_join(histogramAttributes, by='transport_mode') %>%
    mutate(model = map(data, lognormal_fun)) %>%
    mutate(data_aggregated = map(data, aggregateData, binwidth, max_dist)) %>%
    dplyr::select(-data) %>%
    unnest(cols=c('data_aggregated','model')) %>%
    mutate(distance=distance+binwidth*0.2) %>%
    mutate(type='vp')
  vistaDistanceHistograms <- readRDS("../data/vistaSummaries/distanceHistograms.rds")
  
  combinedAll <- bind_rows(vistaDistanceHistograms,
                           vpDistanceHistograms) %>%
    arrange(transport_mode,distance,type) %>%
    mutate(transport_mode=factor(transport_mode,
                                 levels=c("walk","bike","pt","car"),
                                 labels=c("Walking","Cycling","Public transport","Driving"))) %>%
    mutate(type=factor(type,
                       levels=c("vista","vp"),
                       labels=c("Expected","Actual")))
  combinedAllLines <- combinedAll %>%
    dplyr::select(transport_mode,type,binwidth,max_dist,logmean,logsd) %>%
    distinct()%>%ungroup()%>%rowwise()%>% 
    dplyr::mutate(test_var = list(makeLine(binwidth,max_dist,logmean,logsd))) %>% 
    unnest()
  
  
  ggplot(combinedAll,
         aes(x=distance,y=proportion)) +
    geom_col(aes(fill=type,width=binwidth*0.4)) +
    geom_line(data=combinedAllLines, lineend='round', linetype='dashed', size=0.3, aes(color=type)) +
    scale_fill_manual(values=c('#009B95','#FF7100')) +
    scale_color_manual(values=c('blue','green')) +
    facet_wrap(facets=vars(transport_mode), nrow=2, ncol=2, scales='free') +
    scale_y_continuous(minor_breaks=NULL, n.breaks=4, labels=scales::percent_format(accuracy=1)) +
    theme(
      legend.title=element_blank(),
      legend.position="bottom",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,0,5,0),
      panel.spacing.x = unit(1, "lines"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      strip.text = element_text(size = 10),
      axis.ticks = element_line(size=0.2),
      panel.grid.major = element_line(size=0.2),
      panel.grid.minor = element_blank()) +
    labs(x = "Distance traveled (m)", y="Proportion")
  ggsave(paste0(outdir,"/analysis-distance-histograms.pdf"),width=6,height=4)
  
  
  
  # Distance distributions SA3 ----------------------------------------------
  vpDistanceDistributionsSA3 <- vpTripsSA1 %>%
    group_by(sa3_code_2016,transport_mode) %>%
    nest() %>%
    mutate(model = map(data, lognormal_fun)) %>%
    dplyr::select(-data) %>%
    unnest(cols='model') %>%
    filter(!is.na(logmean))
  vistaDistanceDistributionsSA3 <- readRDS("../data/vistaSummaries/distanceDistributionsSA3.rds")
  
  expectedVsActualSA3 <- inner_join(vpDistanceDistributionsSA3%>%
                                      rename(meanActual=logmean,sdActual=logsd,countActual=count),
                                    vistaDistanceDistributionsSA3%>%
                                      rename(meanExpected=logmean,sdExpected=logsd,countExpected=count),
                                    by=c("sa3_code_2016","transport_mode")) %>%
    mutate(transport_mode=factor(transport_mode,
                                 levels=c("walk","bike","pt","car"),
                                 labels=c("Walking","Cycling","Public transport","Driving")))
  expectedVsActualSA3Cor <- expectedVsActualSA3 %>%
    group_by(transport_mode) %>%
    summarise(corr_mean=cor(meanExpected,meanActual),
              corr_sd=cor(sdExpected,sdActual))
  expectedVsActualSA3Cor
  
  ggplot(expectedVsActualSA3, aes(x=meanExpected,y=meanActual)) +
    geom_point(aes(x=meanExpected,y=meanActual,color='#009B95'),size=0.5,alpha=0.25) +
    geom_point(aes(x=sdExpected,y=sdActual,color='#FF7100'),size=0.5,alpha=0.25) +
    scale_color_manual(values=c('#009B95', '#FF7100'),labels=c('log-mean','log-sd')) +
    geom_abline(aes(slope = 1, intercept=0),size=0.2) +
    facet_wrap(~transport_mode, scales="free", ncol=2) +
    labs(x="Expected distance", y="Actual distance") + 
    # ggtitle('Expected versus actual distances by SA3 region') +
    guides(colour = guide_legend(override.aes = list(alpha = 1,size=2))) +
    scale_x_continuous(expand=c(0,0),limits=c(0,10.5), breaks=seq(0,10,2))+
    scale_y_continuous(expand=c(0,0),limits=c(0,10.5), breaks=seq(0,10,2)) +
    theme(
      legend.title=element_blank(),
      legend.position="bottom",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,0,5,0))
  ggsave(paste0(outdir,"/analysis-expected-versus-actual-distances-SA3.pdf"), width=210, height=160, units = "mm")
  
  
  
  # Destination probabilities -----------------------------------------------
  vpStartLocations <- vpTripsSA1 %>%
    group_by(planid) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(location_type=origin_type,weight,sa3_code_2016)
  vpLocations <- bind_rows(
    vpStartLocations,
    vpTripsSA1 %>%
      dplyr::select(location_type=destination_type,weight,sa3_code_2016)) %>%
    filter(location_type!='home')
  vpDestinationProbabilitiesSA3 <- vpLocations %>%
    group_by(location_type,sa3_code_2016) %>%
    summarise(weight=sum(weight,na.rm=T)) %>%
    group_by(location_type) %>%
    mutate(total=sum(weight,na.rm=T)) %>%
    ungroup() %>%
    mutate(prob=weight/total) %>%
    dplyr::select(prob_actual=prob,location_type,sa3_code_2016)
  vistaDestinationProbabilitiesSA3 <- readRDS("../data/vistaSummaries/destinationProbabilitiesSA3.rds")
  
  expectedVsActualProbabilities <-
    full_join(vistaDestinationProbabilitiesSA3,
              vpDestinationProbabilitiesSA3,
              by=c('location_type','sa3_code_2016')) %>%
    dplyr::select(prob_expected,prob_actual,location_type,sa3_code_2016) %>%
    mutate(prob_expected=ifelse(is.na(prob_expected),0,prob_expected)) %>%
    mutate(prob_actual=ifelse(is.na(prob_actual),0,prob_actual))
  
  
  ggplot(expectedVsActualProbabilities, aes(x=prob_expected,y=prob_actual)) +
    geom_point(color='#009B95',size=0.5,alpha=0.5) +
    geom_abline(aes(slope = 1, intercept=0),size=0.2) +
    facet_wrap(~location_type, scales="free", ncol=2) +
    labs(x="Expected probability", y="Actual probability") + 
    # ggtitle('Expected versus actual destination likelihood by SA3 region') +
    guides(colour = guide_legend(override.aes = list(alpha = 1,size=2))) +
    scale_x_continuous(expand=c(0,0),limits=c(0,0.105), breaks=seq(0,0.1,0.02),
                       labels=scales::percent_format(accuracy=1)) +
    scale_y_continuous(expand=c(0,0),limits=c(0,0.105), breaks=seq(0,0.1,0.02),
                       labels=scales::percent_format(accuracy=1)) +
    theme(
      legend.title=element_blank(),
      legend.position="bottom",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,0,5,0))
  ggsave(paste0(outdir,"/analysis-destination-attraction-sa3.pdf"), width=210, height=160, units = "mm")
  
  
  
  # Mode choice probabilities -----------------------------------------------
  vpModeChoiceProbabilitiesSA3 <- vpTripsSA1 %>%
    group_by(sa3_code_2016) %>%
    mutate(total=sum(weight,na.rm=T)) %>%
    group_by(transport_mode,sa3_code_2016,total) %>%
    summarise(weight=sum(weight,na.rm=T)) %>%
    ungroup() %>%
    mutate(prob=weight/total) %>%
    dplyr::select(prob_actual=prob,transport_mode,sa3_code_2016)
  vistaModeChoiceProbabilitiesSA3 <- readRDS("../data/vistaSummaries/modeChoiceProbabilitiesSA3.rds")
  
  expectedVsActualModeSA3 <-
    full_join(vistaModeChoiceProbabilitiesSA3,
              vpModeChoiceProbabilitiesSA3,
              by=c('transport_mode','sa3_code_2016')) %>%
    dplyr::select(prob_expected,prob_actual,transport_mode,sa3_code_2016) %>%
    mutate(prob_expected=ifelse(is.na(prob_expected),0,prob_expected)) %>%
    mutate(prob_actual=ifelse(is.na(prob_actual),0,prob_actual)) %>%
    mutate(transport_mode=factor(transport_mode,
                                 levels=c("walk","bike","pt","car"),
                                 labels=c("Walking","Cycling","Public transport","Driving")))
  
  ggplot(expectedVsActualModeSA3, aes(x=prob_expected,y=prob_actual)) +
    geom_point(color='#009B95',size=0.5,alpha=0.5) +
    geom_abline(aes(slope = 1, intercept=0),size=0.2) +
    facet_wrap(~transport_mode, scales="free", ncol=2) +
    labs(x="Expected probability", y="Actual probability") + 
    # ggtitle('Expected versus actual destination likelihood by SA3 region') +
    guides(colour = guide_legend(override.aes = list(alpha = 1,size=2))) +
    scale_x_continuous(expand=c(0,0),limits=c(0,1), breaks=seq(0,1,0.2),
                       labels=scales::percent_format(accuracy=1)) +
    scale_y_continuous(expand=c(0,0),limits=c(0,1), breaks=seq(0,1,0.2),
                       labels=scales::percent_format(accuracy=1)) +
    theme(
      legend.title=element_blank(),
      legend.position="bottom",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,0,5,0))
  ggsave(paste0(outdir,"/analysis-mode-choice-sa3.pdf"), width=210, height=160, units = "mm")
}