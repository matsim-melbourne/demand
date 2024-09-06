suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(tidyr))
# suppressPackageStartupMessages(library(tibble))

determineEmployed <- function(outputDir) {
 
  echo(paste0("Injesting population data\n"))
  
  population <- readRDS(paste0(outputDir,'/collatedPopulation.rds'))
  

  population_employed <- population %>%
    dplyr::mutate(age_cat = case_when(Age <   5             ~  1,
                                      Age >=  5 & Age <=  9 ~  2,
                                      Age >= 10 & Age <= 14 ~  3,
                                      Age >= 15 & Age <= 19 ~  4, 
                                      Age >= 20 & Age <= 24 ~  5,
                                      Age >= 25 & Age <= 29 ~  6, 
                                      Age >= 30 & Age <= 34 ~  7, 
                                      Age >= 35 & Age <= 39 ~  8, 
                                      Age >= 40 & Age <= 44 ~  9,
                                      Age >= 45 & Age <= 49 ~ 10, 
                                      Age >= 50 & Age <= 54 ~ 11, 
                                      Age >= 55 & Age <= 59 ~ 12, 
                                      Age >= 60 & Age <= 64 ~ 13, 
                                      Age >= 65 & Age <= 69 ~ 14,
                                      Age >= 70 & Age <= 74 ~ 15, 
                                      Age >= 75 & Age <= 79 ~ 16,
                                      Age >= 80 & Age <= 84 ~ 17,
                                      Age >= 85 & Age <= 89 ~ 18,
                                      Age >= 90 & Age <= 94 ~ 19,
                                      Age >= 95 & Age <= 99 ~ 20,
                                      Age >= 100            ~ 21))
  
  population_employed_count <- population_employed %>%
    group_by(SA2_MAINCODE,Gender,age_cat) %>%
    summarise(pop_count=n())
  
  work_status <- read.csv("../dataWorkLocation/melb_sa2_employment_2016.csv") %>%
    mutate(tot=Employed+Not.Employed) %>%
    dplyr::select(SA2_MAINCODE=SA2,Age,Gender=Sex,Employed,Total=tot) %>%
    mutate(employment_percent=Employed/Total) %>%
    mutate(employment_percent=ifelse(is.nan(employment_percent), 0, employment_percent)) %>%
    fill(SA2_MAINCODE, .direction="down") %>%
    mutate(Age=ifelse(Age=="",NA,Age)) %>%
    fill(Age, .direction="down") %>%
    dplyr::mutate(age_cat = case_when(Age == "0-4 years"   ~  1,
                                      Age == "5-9 years"   ~  2,
                                      Age == "10-14 years" ~  3,
                                      Age == "15-19 years" ~  4,
                                      Age == "20-24 years" ~  5,
                                      Age == "25-29 years" ~  6,
                                      Age == "30-34 years" ~  7,
                                      Age == "35-39 years" ~  8,
                                      Age == "40-44 years" ~  9,
                                      Age == "45-49 years" ~ 10,
                                      Age == "50-54 years" ~ 11,
                                      Age == "55-59 years" ~ 12,
                                      Age == "60-64 years" ~ 13,
                                      Age == "65-69 years" ~ 14,
                                      Age == "70-74 years" ~ 15,
                                      Age == "75-79 years" ~ 16,
                                      Age == "80-84 years" ~ 17,
                                      Age == "85-89 years" ~ 18,
                                      Age == "90-94 years" ~ 19,
                                      Age == "95-99 years" ~ 20,
                                      Age == "100 years and over" ~ 21)) %>%
    dplyr::select(SA2_MAINCODE,Gender,age_cat,employment_percent)
  
  work_count <- population_employed_count %>%
    left_join(work_status, by=c("SA2_MAINCODE","Gender","age_cat")) %>%
    mutate(employment_count=round(pop_count*employment_percent)) %>%
    mutate(employment_count=ifelse(employment_count>pop_count,pop_count,employment_count)) %>%
    dplyr::select(SA2_MAINCODE,Gender,age_cat,pop_count,employment_count)
  
  
  set.seed(12)
  
  echo(paste0("Performing join on ", nrow(population_employed), " sampled persons, may take a while\n"))
  
  population_employed_joined <- population_employed %>%
    inner_join(work_count, by=c("SA2_MAINCODE","Gender","age_cat")) %>%
    arrange(SA2_MAINCODE,Gender,age_cat) %>%
    group_by(SA2_MAINCODE,Gender,age_cat) %>%
    rowwise() %>%
    mutate(random_sample=round(runif(1, min=1, max=pop_count))) %>%
    mutate(is_employed=ifelse(employment_count>=random_sample,T,F)) %>%
    arrange(SA2_MAINCODE,Gender,age_cat,random_sample) %>%
    dplyr::select(-pop_count,-employment_count,-random_sample)
  
  echo(paste0("Wrote ", nrow(population_employed_joined), " sampled persons to ", outputDir, '\n'))
  saveRDS(population_employed_joined,paste0(outputDir,'/populationEmployed.rds'))
  
}