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
library(readr)


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
  lab.goldVT$gold.VT1 <- round(lab.goldVT$gold.VT1,2)
  lab.goldVT$gold.VT2 <- round(lab.goldVT$gold.VT2,2)
}
if (file.exists('data/athlete_testdates.csv')){
  ath.testdates <- readr::read_csv('data/athlete_testdates.csv',
                                   col_types = cols("c",col_date(format="%d/%m/%y"),
                                                    col_date(format="%d/%m/%y")))
}


## -----------------------------------------------------------------------------
# rm(tp.newzones)
###################
# initialize cluster
###################
# set up number of cores
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

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
  if (exists('lab.maxHR')){
    if (athlete %in% lab.maxHR$name) {
      ath.maxHR <- lab.maxHR$maxHR[lab.maxHR$name == athlete]
      tp.newzones[tp.newzones$name == athlete,"lab.maxHR"] <- ath.maxHR
    } else {
        ath.maxHR <- 999
        tp.newzones[tp.newzones$name == athlete,"lab.maxHR"] <- NA
    }
  } else {
    ath.maxHR <- 999
    tp.newzones[tp.newzones$name == athlete,"lab.maxHR"] <- NA
  }
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
  save(tp.newzones,file='out/rdas/tpeaks_newzones.rda')
}
# stop cluster
stopCluster(cl)
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

# clean columns, so they all have the correct type (numbers are not chars)
all.activities <- type.convert(all.activities, as.is=T)

# get duplicated activities based on time, etc
dup.sessions <- rbind(dup.sessions,all.activities %>%
  group_by(ath.id,date,start_time) %>% 
  arrange(-total_dist.km, .by_group=T) %>%
  mutate(id=row_number(),source=first(file)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  filter(id > 1) %>%
  mutate(reason="repeated activity") %>%
  select(ath.id,file,source,reason))

# unique activities
all.activities <- all.activities %>%
  group_by(ath.id,date,start_time) %>%
  arrange(-total_dist.km, .by_group=T) %>% 
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
ath.info.test <- merge(ath.full.info,ath.testdates,by="name",all = T) %>% 
  relocate(name,.after=ath.id) %>% 
  arrange(ath.id)

# easier to write, then name back to ath.info.test
res <- ath.info.test
# 
# res <- ath.info.test %>% 
#   select(ath.id,name,total_activities, test_date_1,test_date_2)
# res


## -----------------------------------------------------------------------------
for (i in ath.info.test$ath.id) {
# for (i in "BA_002") {
  ath.activities <- all.activities %>% filter(ath.id==i)
  tests <- c(ath.info.test$test_date_1[ath.info.test$ath.id == i],
             ath.info.test$test_date_2[ath.info.test$ath.id == i])
  for (test_i in seq_len(sum(!is.na(tests)))) {
    if (test_i == 1){
      t = "test1"
      t.date = tests[test_i]
    } else {
      t = "test2"
      t.date = tests[test_i]
    }
    for (period in c("prevweek","prevmonth","prevyear")){
      days=case_when(period=="prevweek" ~ 7, period=="prevmonth" ~ 35, period=="prevyear" ~ 364, TRUE ~ 0)
      # get all activities for the the period
      sport.period.activities <- ath.activities %>% 
        filter(as.Date(as.character(date),"%Y%m%d") < t.date,
               as.Date(as.character(date),"%Y%m%d") >= t.date - days)
      # total sessions
      res[res$ath.id == i,paste0(t,".sessions.total.",period)] <- sport.period.activities %>%
        count() %>% as.numeric()
      # total trimps
      res[res$ath.id == i,paste0(t,".etrimp.total.",period)] <- sport.period.activities %>%
        summarise(total_trimp = round(sum(etrimp,na.rm=T),1)) %>% as.numeric()
      res[res$ath.id == i,paste0(t,".lutrimp.total.",period)] <- sport.period.activities %>%
        summarise(total_trimp = round(sum(lutrimp,na.rm=T),1)) %>% as.numeric()
      res[res$ath.id == i,paste0(t,".lutrimp_goldvt.total.",period)] <- if (all(is.na(sport.period.activities$lutrimp.goldvt)))
        {NA} else {
        sport.period.activities %>% summarise(total_trimp = round(sum(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric() }
      # weekly average
      if(period != "prevweek") {
        res[res$ath.id == i,paste0(t,".sessions.wkavg.",period)] <- 
          round(res[res$ath.id == i,paste0(t,".sessions.total.",period)]/(days/7),1)
        res[res$ath.id == i,paste0(t,".etrimp.wkavg.",period)] <- 
          round(res[res$ath.id == i,paste0(t,".etrimp.total.",period)]/(days/7),1)
        res[res$ath.id == i,paste0(t,".lutrimp.wkavg.",period)] <- 
          round(res[res$ath.id == i,paste0(t,".lutrimp.total.",period)]/(days/7),1)
        res[res$ath.id == i,paste0(t,".lutrimp_goldvt.wkavg.",period)] <- 
          round(res[res$ath.id == i,paste0(t,".lutrimp_goldvt.total.",period)]/(days/7),1)
      }
      # monotony
      if (period == "prevweek"){
        res[res$ath.id == i,paste0(t,".etrimp.monotony.",period)] <- sport.period.activities %>%
          summarise(monotony = round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),1)) %>% as.numeric()
        res[res$ath.id == i,paste0(t,".lutrimp.monotony.",period)] <- sport.period.activities %>%
          summarise(monotony = round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),1)) %>% as.numeric()
        res[res$ath.id == i,paste0(t,".lutrimp_goldvt.monotony.",period)] <- if (all(is.na(sport.period.activities$lutrimp.goldvt)))
        {NA} else { sport.period.activities %>%
            summarise(monotony = round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric()}
      } else {
        wk_monot.etrimp <- NULL
        wk_monot.lutrimp <- NULL
        wk_monot.lutrimp_goldvt <- NULL
        for (wk in seq_len(days/7)){
          # loop through weeks
          wk_monot.etrimp <- c(wk_monot.etrimp, ath.activities %>% 
                                 filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
                                        as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>% 
                                 summarise(m=round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),2)) %>% as.numeric())
          wk_monot.lutrimp <- c(wk_monot.lutrimp, ath.activities %>% 
                                  filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
                                         as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>% 
                                  summarise(m=round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),2)) %>% as.numeric())
          wk_monot.lutrimp_goldvt <- c(wk_monot.lutrimp_goldvt, ath.activities %>% 
                                         filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
                                                as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>% 
                                         summarise(m=round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),2)) %>% as.numeric())
        } # week loop
        res[res$ath.id == i,paste0(t,".etrimp.monotony.",period)] <- round(sum(wk_monot.etrimp,na.rm=T)/(days/7)/
                                                                             sd(wk_monot.etrimp,na.rm=T),2)
        res[res$ath.id == i,paste0(t,".lutrimp.monotony.",period)] <- round(sum(wk_monot.lutrimp,na.rm=T)/(days/7)/
                                                                              sd(wk_monot.lutrimp,na.rm=T),2)
        res[res$ath.id == i,paste0(t,".lutrimp_goldvt.monotony.",period)] <- round(sum(wk_monot.lutrimp_goldvt,na.rm=T)/(days/7)/
                                                                                     sd(wk_monot.lutrimp_goldvt,na.rm=T),2)
      } # end monotony calculation
      # sessions per sport
      for (sport in unique(ath.activities$sport_type)) {
        # to name variable
        if (sport == "Running") {
          sp="run"
        } else if (sport == "Cycling") {
          sp="bike"
        } else {
          sp="other"
        }
        # get all activities for the sport and the period
        sport.period.activities <- ath.activities %>% 
          filter(sport_type == sport) %>% 
          filter(as.Date(as.character(date),"%Y%m%d") < t.date,
                 as.Date(as.character(date),"%Y%m%d") >= t.date - days)
        if (dim(sport.period.activities)[1] == 0) {
          # go to next sport
          res[res$ath.id == i,paste0(t,".",sp,".sessions.total.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".hours.total.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".km.total.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".hours.session.avg.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".km.session.avg.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".hours.session.max.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".km.session.max.",period)] <- 0
          next
        }
        # total sessions
        res[res$ath.id == i,paste0(t,".",sp,".sessions.total.",period)] <- sport.period.activities %>%
          count() %>% as.numeric()
        # weekly average
        if(period != "prevweek") {
          # sessions
          res[res$ath.id == i,paste0(t,".",sp,".sessions.wkavg.",period)] <- 
            round(res[res$ath.id == i,paste0(t,".",sp,".sessions.total.",period)]/(days/7),1)
        }
        # total hours
        res[res$ath.id == i,paste0(t,".",sp,".hours.total.",period)] <- sport.period.activities %>%
          summarise(total_duration = round(sum(duration.min)/60,1)) %>% as.numeric()
        # weekly average
        if(period != "prevweek") {
          # hours
          res[res$ath.id == i,paste0(t,".",sp,".hours.wkavg.",period)] <- 
            round(res[res$ath.id == i,paste0(t,".",sp,".hours.total.",period)]/(days/7),1)
        }
        # total distance in km
        res[res$ath.id == i,paste0(t,".",sp,".km.total.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(sum(total_dist.km),2)) %>% as.numeric()
        # weekly average
        if(period != "prevweek") {
          # distance
          res[res$ath.id == i,paste0(t,".",sp,".km.wkavg.",period)] <- 
            round(res[res$ath.id == i,paste0(t,".",sp,".km.total.",period)]/(days/7),2)
        }
        # single session average hours
        res[res$ath.id == i,paste0(t,".",sp,".hours.session.avg.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(mean(duration.min)/60,1)) %>% as.numeric()
        # single session average distance
        res[res$ath.id == i,paste0(t,".",sp,".km.session.avg.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(mean(total_dist.km),2)) %>% as.numeric()
        # single session max hours
        res[res$ath.id == i,paste0(t,".",sp,".hours.session.max.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(max(duration.min)/60,1)) %>% as.numeric()
        # single session max distance
        res[res$ath.id == i,paste0(t,".",sp,".km.session.max.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(max(total_dist.km),2)) %>% as.numeric()
      } # end sport loop
    } # end period loop
    
    # between tests
    if(test_i == 2) {
      # total sessions
      t1.date = tests[1]
      t2.date = tests[2]
      weeks.btw = round(as.numeric(t2.date - t1.date)/7,0)
      period.activities <- ath.activities %>% 
        filter(as.Date(as.character(date),"%Y%m%d") < t2.date,
               as.Date(as.character(date),"%Y%m%d") > t1.date)
      res[res$ath.id == i,"btwtests.weeks"] <- weeks.btw
      res[res$ath.id == i,"btwtests.sessions.total"] <- period.activities %>% count() %>% as.numeric()
      res[res$ath.id == i,"btwtests.sessions.wkavg"] <- round(res[res$ath.id == i,"btwtests.sessions.total"]/weeks.btw,1)
      
      #########
      # total trimps
      res[res$ath.id == i,paste0("btwtests.etrimp.total")] <- period.activities %>%
        summarise(total_trimp = round(sum(etrimp,na.rm=T),1)) %>% as.numeric()
      res[res$ath.id == i,paste0("btwtests.lutrimp.total")] <- period.activities %>%
        summarise(total_trimp = round(sum(lutrimp,na.rm=T),1)) %>% as.numeric()
      res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.total")] <- if (all(is.na(period.activities$lutrimp.goldvt)))
      {NA} else {period.activities %>% summarise(total_trimp = round(sum(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric() }
      # weekly average
      res[res$ath.id == i,paste0("btwtests.etrimp.wkavg")] <- 
        round(res[res$ath.id == i,paste0("btwtests.etrimp.total")]/weeks.btw,1)
      res[res$ath.id == i,paste0("btwtests.lutrimp.wkavg")] <- 
        round(res[res$ath.id == i,paste0("btwtests.lutrimp.total")]/weeks.btw,1)
      res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.wkavg")] <- 
        round(res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.total")]/weeks.btw,1)
      # monotony
      wk_monot.etrimp <- NULL
      wk_monot.lutrimp <- NULL
      wk_monot.lutrimp_goldvt <- NULL
      for (wk in seq_len(weeks.btw)){
        # loop through weeks
        wk_monot.etrimp <- c(wk_monot.etrimp, ath.activities %>% 
                               filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
                                      as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
                               summarise(m=round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),2)) %>% as.numeric())
        wk_monot.lutrimp <- c(wk_monot.lutrimp, ath.activities %>% 
                                filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
                                       as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
                                summarise(m=round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),2)) %>% as.numeric())
        wk_monot.lutrimp_goldvt <- c(wk_monot.lutrimp_goldvt,
                      ath.activities %>% 
                        filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
                               as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
                        summarise(m=round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),2)) %>% as.numeric())
      } # week loop
      res[res$ath.id == i,paste0("btwtests.etrimp.monotony")] <- round(sum(wk_monot.etrimp,na.rm=T)/weeks.btw/
                                                                         sd(wk_monot.etrimp,na.rm=T),2)
      res[res$ath.id == i,paste0("btwtests.lutrimp.monotony")] <- round(sum(wk_monot.lutrimp,na.rm=T)/weeks.btw/
                                                                          sd(wk_monot.lutrimp,na.rm=T),2)
      res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.monotony")] <-
        round(sum(wk_monot.lutrimp_goldvt,na.rm=T)/weeks.btw/sd(wk_monot.lutrimp_goldvt,na.rm=T),2)
      # monotony calculation
      #########
      
      # sport loop
      for (sport in unique(ath.activities$sport_type)) {
        # to name variable
        if (sport == "Running") {
          sp="run"
        } else if (sport == "Cycling") {
          sp="bike"
        } else {
          sp="other"
        }
        # get all activities for the sport and the period
        sport.period.activities <- ath.activities %>% 
          filter(sport_type == sport) %>% 
          filter(as.Date(as.character(date),"%Y%m%d") < t2.date,
                 as.Date(as.character(date),"%Y%m%d") > t1.date)
        
        if (dim(sport.period.activities)[1] == 0) {
          # go to next sport
          res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")] <- 0
          res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")] <- 0
          res[res$ath.id == i,paste0("btwtests.",sp,".km.total")] <- 0
          res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.avg")] <- 0
          res[res$ath.id == i,paste0("btwtests.",sp,".km.session.avg")] <- 0
          res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.max")] <- 0
          res[res$ath.id == i,paste0("btwtests.",sp,".km.session.max")] <- 0
          next
        }
        # total sessions
        res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")] <- sport.period.activities %>%
          count() %>% as.numeric()
        # weekly average sessions
        res[res$ath.id == i,paste0("btwtests.",sp,".sessions.wkavg")] <- 
          round(res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")]/weeks.btw,1)
        # total hours
        res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")] <- sport.period.activities %>%
          summarise(total_duration = round(sum(duration.min)/60,1)) %>% as.numeric()
        # weekly average hours
        res[res$ath.id == i,paste0("btwtests.",sp,".hours.wkavg")] <- 
          round(res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")]/weeks.btw,1)
        # total distance in km
        res[res$ath.id == i,paste0("btwtests.",sp,".km.total")] <- sport.period.activities %>%
          summarise(total_distance = round(sum(total_dist.km),2)) %>% as.numeric()
        # weekly average distance
        res[res$ath.id == i,paste0("btwtests.",sp,".km.wkavg")] <- 
          round(res[res$ath.id == i,paste0("btwtests.",sp,".km.total")]/weeks.btw,2)
        # single session average hours
        res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.avg")] <- sport.period.activities %>%
          summarise(total_distance = round(mean(duration.min)/60,1)) %>% as.numeric()
        # single session average distance
        res[res$ath.id == i,paste0("btwtests.",sp,".km.session.avg")] <- sport.period.activities %>%
          summarise(total_distance = round(mean(total_dist.km),2)) %>% as.numeric()
        # single session max hours
        res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.max")] <- sport.period.activities %>%
          summarise(total_distance = round(max(duration.min)/60,1)) %>% as.numeric()
        # single session max distance
        res[res$ath.id == i,paste0("btwtests.",sp,".km.session.max")] <- sport.period.activities %>%
          summarise(total_distance = round(max(total_dist.km),2)) %>% as.numeric()
      } # end sport loop
    } # end between test calculations
  } # end test loop
} # end athlete loop
# 
# res
ath.info.test <- res


## -----------------------------------------------------------------------------
# 
# # add error and duplicate count
# i <- "BA_003"
# # add year count
# test1 <- ath.info.test$test_date_1[ath.info.test$ath.id == i]
# test2 <- ath.info.test$test_date_2[ath.info.test$ath.id == i]
# test1
# test2
# format(test2,format="%Y%m%d")
# as.Date("20150531","%Y%m%d")
# 
# all.activities  %>% 
#   filter(ath.id==i) %>% 
#   arrange(-date) %>% 
#   filter(as.Date(as.character(date),"%Y%m%d") < test2,
#          as.Date(as.character(date),"%Y%m%d") >= test2-28) %>% 
#   count() %>%
#   as.numeric()
# 
# all.activities %>% 
#   filter(ath.id==i) %>% 
#   filter(as.Date(as.character(date),"%Y%m%d") < test1,
#          as.Date(as.character(date),"%Y%m%d") >= test1-7) %>% 
#   arrange(-date) %>% 
#   summarise(monotony=round(sum(lutrimp.goldvt,na.rm=T)/sd(lutrimp.goldvt,na.rm=T),2)) %>% as.numeric()
# # diff(fitdata$record$timestamp)
# 
# wk_mon <- NULL
# for (wk in seq_len(35/7)){
#   # loop through weeks
#   wk_mon <- c(wk_mon, all.activities %>% 
#     filter(ath.id==i) %>% 
#     filter(as.Date(as.character(date),"%Y%m%d") < (test1 - (7*(wk-1))),
#            as.Date(as.character(date),"%Y%m%d") >= (test1 - (7*wk))) %>% 
#     arrange(-date) %>% 
#     summarise(monotony=round(mean(etrimp,na.rm=T)/sd(etrimp,na.rm=T),2)) %>% as.numeric())
# }
# wk_mon
# mean(wk_mon)/sd(wk_mon)


## -----------------------------------------------------------------------------
save(ath.info.test, tp.newzones, tpeaks.all, all.activities, error.sessions, dup.sessions,file=
       paste0("out/rdas/trainingpeaks_results_",format(as.Date(Sys.Date()),format="%y%m%d"),".rda"))


## -----------------------------------------------------------------------------
# load('trainingpeaks_data_PARALLEL_200701.rda')
# 
# save CSV
# write.csv(tp.newzones,file=paste0("out/csv/tpeaks_summary_newzones_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
write.csv(ath.info.test, file=paste0("out/csv/tpeaks_summary_athletes_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
write.csv(all.activities, file=paste0("out/csv/tpeaks_summary_activities_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
write.csv(error.sessions, file=paste0("out/csv/tpeaks_summary_error_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
write.csv(dup.sessions, file=paste0("out/csv/tpeaks_summary_dups_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
# save all in an XLSX file
sheet_list <- list("athletes"= ath.info.test,
                   "activities"=all.activities,
                   "errors"=error.sessions,
                   "duplicates"=dup.sessions)
write.xlsx(sheet_list, keepNA=TRUE,
           file=paste0("out/trainingpeaks_results_",format(as.Date(Sys.Date()),format="%y%m%d"),".xlsx"))

