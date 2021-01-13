## ----warning=FALSE, results='hide', echo='FALSE'------------------------------
library(fit)
library(ggplot2)
library(dplyr)
library(stringr)
library(foreach)
library(doParallel)
library(zoo)
library(splines)
library(openxlsx)
library(geosphere)
library(XML)
library(tidyr)


## -----------------------------------------------------------------------------
source('src/ctt_definition.R') # constant variables and files
source('src/f_getactivityinfo.R') # main functions to get info from activity
source('src/f_getpowspeedzones.R') # functions to get power/speed zones for new trimp scores
source('src/f_getgpxactivity.R') # functions for reading gpx files


## -----------------------------------------------------------------------------
# DECLARE NAME OF ATHLETES TO ANALYZE
# sel.athletes <- c("NAME")
sel.athletes <- read.table("data/list_athletes.txt",stringsAsFactors = F)$V1

# all folders in PRO_HEART, where sel.athletes folders are located
ph_folders1 <- list.dirs('../../2101_TRAININGPEAKS/PRO_HEART',recursive = F)
ph_folders2 <- list.dirs(list.dirs('../../2003_TRAININGPEAKS/PRO_HEART',recursive = F),recursive=F)
ph_folders3 <- list.dirs('../../2101_TRAININGPEAKS/MASTER_HEART',recursive = F)
all.dirs.PH <- c(ph_folders1, ph_folders2, ph_folders3)


## -----------------------------------------------------------------------------
if(file.exists('out/rdas/tpeaks_newzones.rda')){
  load('out/rdas/tpeaks_newzones.rda')
}


## -----------------------------------------------------------------------------
if (file.exists('data/athlete_maxHR.txt')){
  lab.maxHR <- read.table('data/athlete_maxHR.txt')
  colnames(lab.maxHR) <- c('name','maxHR')
}
if (file.exists('data/athlete_goldVT.csv')){
  lab.goldVT <- read.csv('data/athlete_goldVT.csv')
}


## -----------------------------------------------------------------------------
# rm(tp.newzones)
 writeLines(c("###############################",
               "##   TRAININGPEAKS PROJECT   ##",
               "###############################"))
for (athlete in sel.athletes){
  writeLines(c("","###############################",
               "Calculating maxHR and HR/Power/Speed correlations...",
               paste0("Working with athlete ",athlete),""))
  
  # check if we already have the info
  if(!exists("tp.newzones")){
    writeLines(c("Nothing loaded or previously computed...",
                 "Running everything for the first time."))
    tp.newzones <- data.frame(name=athlete, ath.id=paste0("BA_",str_pad(1,3,pad="0")),
                              maxHR= NA, stringsAsFactors=FALSE)
  } else {
    # if there is no row for the athlete, create one
    if(!athlete %in% tp.newzones$name){
      writeLines(c("Unknown athlete, creating a new entry."))
      tp.newzones[nrow(tp.newzones)+1,'name'] <- athlete
      tp.newzones$ath.id[tp.newzones$name == athlete] <- paste0("BA_",str_pad(dim(tp.newzones)[1],3,pad="0"))
    } else if (!is.na(tp.newzones$maxHR[tp.newzones$name == athlete])) {
      # if there already is a row with a not NA maxHR, the athlete has already been processed --> skip
      # if(dim(tp.newzones[tp.newzones$name == athlete,])[1] != 0
      #    && !is.na(tp.newzones$maxHR[tp.newzones$name == athlete])){
      writeLines(paste0("Zones already calculated, skipping athlete ",athlete))
      next
    }
  }
  ### SRC COMMENT NEXT SECTION AS TANGENT METHOD DOESNT NEED LAB MAXHR
  ### SET IT TO 190 TO HAVE A MINIMUM TO SMOOTH TO REMOVE SPIKES
  # you should be here if the athlete has not been processed, with a new row in the table
  # we need to check now if there is a lab-measured maxHR
  # if not, we will use 180 as default (to smooth HR over that later)
  # if (exists('lab.maxHR')){
  #   ath.maxHR <- if (athlete %in% lab.maxHR$name) {lab.maxHR$maxHR[lab.maxHR$name == athlete]} else {180}
  # } else {
  #   ath.maxHR <- 180
  # }
  ath.maxHR <- 190
  # Finally, run the function to get the zones and update the tp.newzones table
  tp.newzones <- update.ath_info_with_newzones(tp.newzones, athlete, ath.maxHR)
  # also, add the new "goldVT" zones, to include in the calculation later
  if(exists("lab.goldVT") & any(lab.goldVT$ath.name == athlete)){
    tp.newzones$gold.vt1[tp.newzones$name == athlete] <- lab.goldVT$gold.VT1[lab.goldVT$ath.name == athlete]
    tp.newzones$gold.vt2[tp.newzones$name == athlete] <- lab.goldVT$gold.VT2[lab.goldVT$ath.name == athlete]
    # if any is 0, it means that there was an error on the calculations, so set NA for those athletes
    if(any(tp.newzones[tp.newzones$name == athlete,c("gold.vt1","gold.vt2")] == 0) |
       tp.newzones[tp.newzones$name == athlete,]$gold.vt1 > tp.newzones[tp.newzones$name == athlete,]$gold.vt2) {
      tp.newzones$gold.vt1[tp.newzones$name == athlete] <- NA
      tp.newzones$gold.vt2[tp.newzones$name == athlete] <- NA
    }
  } else {
    tp.newzones$gold.vt1[tp.newzones$name == athlete] <- NA
    tp.newzones$gold.vt2[tp.newzones$name == athlete] <- NA
  }
}
# save everything in a file, so we don't need to calculate them every time
save(tp.newzones,file='out/rdas/tpeaks_newzones.rda')
tp.newzones


## -----------------------------------------------------------------------------
tpeaks.all <- NULL
###################
# initialize cluster
###################
# set up number of cores
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

for (athlete in sel.athletes){
  writeLines(c("","###############################",
               "Processing all fit files...",
               paste0("Working with athlete ",athlete),""))
  
  sel.dirs <- all.dirs.PH[str_detect(all.dirs.PH,athlete)][1]
  if (length(str_split(sel.dirs,"/")[[1]]) == 5){
    sel.dirs <- list.dirs(sel.dirs)
  } else {
    sel.dirs <- list.dirs(sel.dirs,recursive=F)
    sel.dirs <- sel.dirs[str_detect(sel.dirs,"YEAR")]
  }
  files <- list.files(sel.dirs,pattern=".fit|gpx",full.names = TRUE)
  ath.id <- tp.newzones$ath.id[tp.newzones$name == athlete]
  ath.maxHR <- tp.newzones$maxHR[tp.newzones$name == athlete]
  
  start <- Sys.time()

  tp.athlete <- NULL
  tp.athlete <- foreach (file=files, .combine=rbind,
                         # .export=c("get.act_info_from_fitdata","smooth.data","onerow.df","get.date_GARMIN"),
                         .packages=c("dplyr", "fit", "stringr", "zoo", "splines","geosphere","XML","tidyr")) %dopar% {
    temp.tp.athlete <- process.fitfile(file, ath.id)
    temp.tp.athlete
  }
  end <- Sys.time()
  duration <- end-start
  print(duration)
  tpeaks.all <- rbind(tpeaks.all,tp.athlete)
  rownames(tpeaks.all) <- NULL
}
# stop cluster
stopCluster(cl)

# save everything in a file, so we don't need to calculate them every time
# save(tpeaks.all,file=paste0('out/rdas/tpeaks_all_',format(as.Date(Sys.Date()),format="%y%m%d"),'.rda'))
# tpeaks.all


## -----------------------------------------------------------------------------
# levels(training.all$file) <- c(levels(training.all$file),"PRO_HEART/AUS_PH/BENTLEYOLDEN/FULL_2YEARS/kk.fit","PRO_HEART/AUS_PH/BENTLEYOLDEN/MONTH_2YEARS/2018-09-28-203213-ELEMNT 3AE9-91-0.fit")
# training.all[8,]$file <- "PRO_HEART/AUS_PH/BENTLEYOLDEN/FULL_2YEARS/kk.fit"
# training.all[7,]$file <- "PRO_HEART/AUS_PH/BENTLEYOLDEN/MONTH_2YEARS/2018-09-28-203213-ELEMNT 3AE9-91-0.fit"
error.sessions <- tpeaks.all %>% filter(str_detect(date,"error")) %>% select(ath.id,file,reason=date)
# check if file has already been processed in a different folder (some cases with full/monthly divisions)
dup.sessions <- tpeaks.all %>%
  filter(!str_detect(date,"error")) %>% 
  mutate(filename=sapply(str_split(file,"/"),function(x) last(x))) %>%
  group_by(ath.id,filename) %>%
  mutate(id=row_number(),source=first(file)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  filter(id >1) %>%
  mutate(reason="duplicated file") %>%
  select(ath.id,file,source,reason)
# update all.activities
all.activities <- tpeaks.all %>%
  filter(!str_detect(date,"error")) %>% 
  mutate(filename=sapply(str_split(file,"/"),function(x) last(x))) %>%
  group_by(ath.id,filename) %>%
  mutate(id=row_number(),source=first(file)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  filter(id == 1) %>% 
  select(-filename,-source,-id)
# get duplicated activities based on time, etc
dup.sessions <- rbind(dup.sessions,all.activities %>%
  group_by(ath.id,date,start_time,duration.min) %>%
  mutate(id=row_number(),source=first(file)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  filter(id > 1) %>%
  mutate(reason="repeated activity") %>%
  select(ath.id,file,source,reason))
# unique activities
all.activities <- all.activities %>%
  group_by(ath.id,date,start_time,duration.min) %>%
  mutate(id=row_number(),source=first(file)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  filter(id == 1) %>% 
  select(-source,-id)


## -----------------------------------------------------------------------------
# cohort_vs_athlete <- as.data.frame(t(do.call("cbind",str_split(str_remove(all.dirs.PH,".*TRAININGPEAKS/"),"/"))),
                                   # stringsAsFactors = F)
cohort_vs_athlete <- str_split(str_remove(all.dirs.PH,".*TRAININGPEAKS/"),"/")
cohort_vs_athlete <- as.data.frame(t(do.call("cbind",
                                             lapply(cohort_vs_athlete,function(x){x[(length(x)-1):length(x)]}))))
cohort_vs_athlete <- cohort_vs_athlete %>% 
  filter(V2 != "rawzip") %>% 
  # mutate(V1 = ifelse(V1==V2,'P@H',V1)) %>% 
  arrange(V1) %>% 
  distinct(V2, .keep_all = T)

ath.info <- merge(merge(tp.newzones, cohort_vs_athlete,by.x="name",by.y="V2") %>% select(name,ath.id,cohort=V1),
                  all.activities %>%
                    mutate(folder=tolower(sapply(str_split(file,"/"),function(x) x[4]))) %>%
                    group_by(ath.id) %>%
                    summarise(total_activities=n(), 
                              month_2_year_0=sum(folder=="month_2_year_0"),
                              month_12_year_0=sum(folder=="month_12_year_0"),
                              month_2_year_2=sum(folder=="month_2_year_2"),
                              month_12_year_2=sum(folder=="month_12_year_2"),
                              full_2years=sum(folder=="full_2years"),
                              n_cycling=sum(sport_type=="Cycling"),
                              n_running=sum(sport_type=="Running"),
                              n_other=sum(!grepl("Running|Cycling",sport_type)),
                              n_heart=sum(!is.na(hr.avg)),n_power=sum(!is.na(power.avg)),
                              n_heart_power=sum(!is.na(power.avg) & !is.na(hrmax.activity))), by='ath.id', all=T) %>%
  mutate(perc_heart=round(n_heart/total_activities*100,2),perc_power = round(n_power/total_activities*100,2), perc_heart_power = round(n_heart_power/total_activities*100,2)) %>% 
  arrange(ath.id)
# add error and duplicate count
for (i in ath.info$name) {
  ath.info[ath.info$name==i,'n_error'] <- sum(!is.na(str_extract(error.sessions$file,i)))
  ath.info[ath.info$name==i,'n_duplicates'] <- sum(!is.na(str_extract(dup.sessions$file,i)))
}
# add year count
year.info <- all.activities %>% group_by(ath.id,year) %>% tally() %>% mutate(n=as.numeric(n))
for (i in ath.info$ath.id) {
  for (year in unique(year.info$year)){
    year_n <- as.numeric(year.info[as.character(year.info$ath.id) == i & year.info$year == year,"n"])
    ath.info[ath.info$ath.id==i,paste0("n_",year)] <- if(is.na(year_n)){0}else{year_n}
  }
}

#merge with newzones and nice order
ath.full.info <- merge(ath.info,tp.newzones,by="ath.id") %>% 
  select(-name.y,-starts_with("month"),-full_2years) %>% 
  rename(name=name.x) %>% 
  relocate(any_of(c("n_error","n_duplicates")),.after=total_activities) %>% 
  relocate(perc_heart,.after=n_heart) %>% 
  relocate(perc_power,.after=n_power)

ath.full.info


## -----------------------------------------------------------------------------
save(ath.full.info, tp.newzones, tpeaks.all, all.activities, error.sessions, dup.sessions,file=
       paste0("out/rdas/trainingpeaks_results_",format(as.Date(Sys.Date()),format="%y%m%d"),".rda"))


## -----------------------------------------------------------------------------
# load('trainingpeaks_data_PARALLEL_200701.rda')
# 
# save CSV
# write.csv(tp.newzones,file=paste0("out/csv/tpeaks_summary_newzones_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
write.csv(ath.full.info, file=paste0("out/csv/tpeaks_summary_athletes_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
write.csv(all.activities, file=paste0("out/csv/tpeaks_summary_activities_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
write.csv(error.sessions, file=paste0("out/csv/tpeaks_summary_error_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
write.csv(dup.sessions, file=paste0("out/csv/tpeaks_summary_dups_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
# save all in an XLSX file
sheet_list <- list("athletes"= ath.full.info,
                   "activities"=all.activities,
                   "errors"=error.sessions,
                   "duplicates"=dup.sessions)
write.xlsx(sheet_list, keepNA=TRUE,
           file=paste0("out/trainingpeaks_results_",format(as.Date(Sys.Date()),format="%y%m%d"),".xlsx"))

