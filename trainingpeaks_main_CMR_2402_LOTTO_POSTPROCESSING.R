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
source('src/f_getmaxhrtangent.R') # functions to get maxHR with tangent method
source('src/f_gettcxactivity.R') # functions for reading tcx files


## -----------------------------------------------------------------------------
# DECLARE NAME OF ATHLETES TO ANALYZE
# sel.athletes <- c("NAME")

ath.testdates <- readr::read_csv('../../TP_LOTTO23/lotto_tpeaks_2402.csv',
                                   col_types = cols("c",
                                                    col_date(format="%d/%m/%Y")))
sel.athletes <- ath.testdates$ATHLETE
# sel.athletes <- c(sel.athletes, ath.testdates2$ATHLETE, ath.testdates3$ATHLETE)


# all folders in PRO_HEART, where sel.athletes folders are located
all.dirs.PH <- list.dirs('../../TP_LOTTO23/',recursive = F)


## -----------------------------------------------------------------------------
if(file.exists('out/rdas/tpeaks_newzones_cmr.rda')){
  load('out/rdas/tpeaks_newzones_cmr.rda')
}



rda_file <- tail(list.files('out/rdas/', pattern="tpeaks_all_*", recursive=T, full.names=T), 1)
writeLines(paste0("Using most recent rda file: ", rda_file))
load(file=rda_file)

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
dup.sessions <- rbind(dup.sessions, all.activities %>%
                        group_by(ath.id, date, start_time) %>% 
                        # mutate device_brand_id so 0s are 9999 and do not interfere with sorting in next step
                        mutate(device_brand_id = ifelse(device_brand_id == 0, 9999, device_brand_id)) %>% 
                        # arrange by device_brand_id (so garmin would always be higher)
                        arrange(device_brand_id, -total_dist.km, .by_group=T) %>%
                        # arrange(-total_dist.km, .by_group=T) %>%
                        mutate(id=row_number(),source=first(file)) %>%
                        ungroup() %>%
                        mutate_if(is.factor, as.character) %>%
                        filter(id > 1) %>%
                        mutate(reason="repeated activity") %>%
                        select(ath.id, file, source, reason))

# unique activities
all.activities <- all.activities %>%
  group_by(ath.id, date, start_time) %>%
  # mutate device_brand_id so 0s are 9999 and do not interfere with sorting in next step
  mutate(device_brand_id = ifelse(device_brand_id == 0, 9999, device_brand_id)) %>% 
  # arrange by device_brand_id (so garmin would always be higher)
  arrange(device_brand_id, -total_dist.km, .by_group=T) %>%
  # arrange(-total_dist.km, .by_group=T) %>%% 
  mutate(id=row_number(), source=first(file)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  filter(id == 1) %>% 
  select(-source, -id)

# split z01 into z0 and z1 and all the respective times
all.activities <- all.activities %>% 
  mutate(hr.z1 = round(hr.z1.time / duration.min * 100, 2),
         hr.z0 = round(hr.z01 - hr.z1, 2),
         hr.z0.time = round(hr.z0/100 * duration.min, 1),
         hr.z01.time = round(hr.z01/100 * duration.min, 1),
         hr.zones.total.time = hr.z0.time + hr.z1.time + hr.z2.time + hr.z3.time + hr.z4.time + hr.z5.time,
         hr.zones.total.perc = hr.z0 + hr.z1 + hr.z2 + hr.z3 + hr.z4 + hr.z5) %>% 
  relocate(any_of(c("hr.z0","hr.z1")), .after = hr.z01) %>% 
  relocate(any_of(c("hr.z01.time", "hr.z0.time")), .before = hr.z1.time) %>% 
  relocate(hr.zones.total.time, .after = hr.z5.time) %>%
  relocate(hr.zones.total.perc, .after = hr.z5)


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

cohort_vs_athlete$V1 <- "LOTTO_2312"

ath.info <- merge(merge(tp.newzones, cohort_vs_athlete,by.x="name",by.y="V2") %>% select(name,ath.id,cohort=V1),
                  all.activities %>%
                    mutate(folder=tolower(sapply(str_split(file,"/"),function(x) x[4]))) %>%
                    group_by(ath.id) %>%
                    summarise(total_activities=n(), 
                              n_cycling=sum(sport_type=="Cycling", na.rm = T),
                              n_running=sum(sport_type=="Running", na.rm = T),
                              n_other=sum(!grepl("Running|Cycling",sport_type)),
                              n_heart=sum(!is.na(hr.avg)),
                              n_heart_chest=sum(hr.sensor & !is.na(hr.avg)),
                              # n_power=sum(!is.na(power.avg)),
                              # n_heart_power=sum(!is.na(power.avg) & !is.na(hrmax.activity))
                              ), by='ath.id', all=T) %>%
  mutate(perc_heart=round(n_heart/total_activities*100,1),
         perc_heart_chest = round(n_heart_chest/n_heart * 100, 1),
         #        perc_power = round(n_power/total_activities*100,1),
         #        perc_heart_power = round(n_heart_power/total_activities*100,1)
  ) %>% 
  arrange(ath.id)

# add error and duplicate count
for (i in ath.info$name) {
  ath.info[ath.info$name==i,'n_error'] <- sum(!is.na(str_extract(error.sessions$file,i)))
  ath.info[ath.info$name==i,'n_duplicates'] <- sum(!is.na(str_extract(dup.sessions$file,i)))
}
# add year count
year.info <- all.activities %>% group_by(ath.id,year) %>% tally() %>% mutate(n=as.numeric(n)) %>% arrange(year)
for (i in ath.info$ath.id) {
  for (year in unique(year.info$year)){
    year_n <- as.numeric(year.info[as.character(year.info$ath.id) == i & year.info$year == year,"n"])
    ath.info[ath.info$ath.id==i,paste0("n_",year)] <- if(is.na(year_n)){0}else{year_n}
  }
}

#merge with newzones and nice order
ath.full.info <- merge(ath.info,tp.newzones,by="ath.id") %>% 
  select(-name.y) %>% 
  rename(name=name.x) %>% 
  relocate(any_of(c("n_error","n_duplicates")),.after=total_activities) %>% 
  # relocate(perc_power,.after=n_power) %>% 
  relocate(perc_heart,.after=n_heart) %>% 
  relocate(perc_heart_chest, .after=n_heart_chest)
  
ath.full.info


## -----------------------------------------------------------------------------
ath.info.test <- merge(ath.full.info, ath.testdates, by.x = "name", by.y = "ATHLETE" , all = T) %>% 
  relocate(name,.after=ath.id) %>% 
  arrange(ath.id) %>% 
  rename(test_date_1 = TEST_DATE)


## -----------------------------------------------------------------------------
# add total time and time per zones
hr_summary <- all.activities %>%
  # remove activities without HR
  filter(!is.na(hrmax.activity)) %>%
  group_by(ath.id) %>% 
  # filter(ath.id == "CMR_010") %>% 
  summarise(duration.min.total = sum(duration.min, na.rm = T),
            hr.z0.min.total = sum(hr.z0.time, na.rm = T),
            hr.z1.min.total = sum(hr.z1.time, na.rm = T),
            hr.z2.min.total = sum(hr.z2.time, na.rm = T),
            hr.z3.min.total = sum(hr.z3.time, na.rm = T),
            hr.z4.min.total = sum(hr.z4.time, na.rm = T),
            hr.z5.min.total = sum(hr.z5.time, na.rm = T),
            hr.zones.min.total = hr.z0.min.total + hr.z1.min.total + hr.z2.min.total + hr.z3.min.total + hr.z4.min.total + hr.z5.min.total,
            # hr.zones.min.total.difference = round(hr.zones.min.total / duration.min.total * 100,1)
            ) %>% 
  mutate(hr.z0.perc.total = round(hr.z0.min.total / duration.min.total * 100, 1),
         hr.z1.perc.total = round(hr.z1.min.total / duration.min.total * 100, 1),
         hr.z2.perc.total = round(hr.z2.min.total / duration.min.total * 100, 1),
         hr.z3.perc.total = round(hr.z3.min.total / duration.min.total * 100, 1),
         hr.z4.perc.total = round(hr.z4.min.total / duration.min.total * 100, 1),
         hr.z5.perc.total = round(hr.z5.min.total / duration.min.total * 100, 1),
         hr.zones.perc.total = hr.z0.perc.total + hr.z1.perc.total + hr.z2.perc.total + hr.z3.perc.total + hr.z4.perc.total + hr.z5.perc.total) %>% 
  rename(duration.min.total.withHR = duration.min.total)

hr_summary_NOHR <- all.activities %>%
  # remove activities without HR
  filter(is.na(hrmax.activity)) %>% 
  group_by(ath.id) %>%
  summarise(duration.min.total.noHR = sum(duration.min, na.rm = T))

hr_summary <- hr_summary %>% 
  left_join(hr_summary_NOHR, by = "ath.id") %>% 
  mutate(duration.min.total.noHR = ifelse(is.na(duration.min.total.noHR), 0, duration.min.total.noHR),
         duration.min.total = duration.min.total.withHR + duration.min.total.noHR,
         perc.total.withHR = round(duration.min.total.withHR / duration.min.total * 100,1)) %>% 
  relocate(duration.min.total.noHR, duration.min.total, perc.total.withHR, .after=duration.min.total.withHR)

ath.info.test <- left_join(ath.info.test, hr_summary, by="ath.id")

# easier to write, then name back to ath.info.test
res <- ath.info.test

# 
# res <- ath.info.test %>% 
#   select(ath.id,name,total_activities, test_date_1,test_date_2)
# res


## -----------------------------------------------------------------------------
for (i in ath.info.test$ath.id) {
# for (i in "BA_002") {
  ath.activities <- all.activities %>% filter(ath.id==i) %>% filter(!is.na(sport_type))
  tests <- c(ath.info.test$test_date_1[ath.info.test$ath.id == i]
             # ,ath.info.test$test_date_2[ath.info.test$ath.id == i]
             )
  for (test_i in seq_len(sum(!is.na(tests)))) {
    if (test_i == 1){
      t = "test1"
      t.date = tests[test_i]
    } else {
      t = "test2"
      t.date = tests[test_i]
    }
    ## activity counter per month
    for (befnext in c("prev", "next")) {
      days.prev = 0
      for (period in c(
        # "prevweek",
        "month1",
        "month2",
        "month3",
        "month4",
        "month5",
        "month6",
        "month7",
        "month8",
        "month9",
        "month10",
        "month11",
        "month12")){
        days=case_when(period == "month1" ~ 28,            # 4 weeks
                       period == "month2" ~ 56,            # 4 weeks
                       period == "month3" ~ 91,            # 5 weeks
                       period == "month4" ~ 119,           # 4 weeks
                       period == "month5" ~ 147,           # 4 weeks
                       period == "month6" ~ 182,           # 5 weeks
                       period == "month7" ~ 210,           # 4 weeks
                       period == "month8" ~ 238,           # 4 weeks
                       period == "month9" ~ 273,           # 5 weeks
                       period == "month10" ~ 301,          # 4 weeks 
                       period == "month11" ~ 329,          # 4 weeks
                       period == "month12" ~ 364, TRUE ~ 0)    # 5 weeks
        # choose if next or previous
        if(befnext == "prev"){
          sport.period.activities <- ath.activities %>% 
            filter(as.Date(as.character(date),"%Y%m%d") < t.date - days.prev,
                   as.Date(as.character(date),"%Y%m%d") >= t.date - days)
        } else {
          # get all activities for the the period
          sport.period.activities <- ath.activities %>% 
            filter(as.Date(as.character(date),"%Y%m%d") > t.date + days.prev,
                   as.Date(as.character(date),"%Y%m%d") <= t.date + days)
        }
        res[res$ath.id == i,paste0(t,".sessions.total.", befnext,period)] <- sport.period.activities %>%
          count() %>% as.numeric()
        days.prev <- days
      } # end period
    } #end befnext
    # end activity counter
    
    for (period in c(
                     # "prevweek",
                     "prev1months",
                     # "prev2months",
                     "prev3months",
                     # "prev4months",
                     # "prev5months",
                     "prev6months",
                     # "prev7months",
                     # "prev8months",
                     # "prev9months",
                     # "prev10months",
                     # "prev11months",
                     "prev12months",
                     "next1months",
                     "next3months",
                     "next6months",
                     "next12months")){
      days=case_when(period == "prevweek" ~ 7,
                     period == "prev1months" ~ 28,            # 4 weeks
                     period == "next1months" ~ 28,            # 4 weeks
                     period == "prev2months" ~ 56,            # 4 weeks
                     period == "prev3months" ~ 91,            # 5 weeks
                     period == "next3months" ~ 91,            # 5 weeks
                     period == "prev4months" ~ 119,           # 4 weeks
                     period == "prev5months" ~ 147,           # 4 weeks
                     period == "prev6months" ~ 182,           # 5 weeks
                     period == "next6months" ~ 182,           # 5 weeks
                     period == "prev7months" ~ 210,           # 4 weeks
                     period == "prev8months" ~ 238,           # 4 weeks
                     period == "prev9months" ~ 273,           # 5 weeks
                     period == "prev10months" ~ 301,          # 4 weeks 
                     period == "prev11months" ~ 329,          # 4 weeks
                     period == "prev12months" ~ 364,
                     period == "next12months" ~ 364, TRUE ~ 0)    # 5 weeks
      # choose if next or previous
      if(grepl("next", period)){
        sport.period.activities <- ath.activities %>% 
          filter(as.Date(as.character(date),"%Y%m%d") > t.date,
                 as.Date(as.character(date),"%Y%m%d") <= t.date + days)
      } else {
        # get all activities for the the period
        sport.period.activities <- ath.activities %>% 
          filter(as.Date(as.character(date),"%Y%m%d") < t.date,
                 as.Date(as.character(date),"%Y%m%d") >= t.date - days)
      }
      
      ######################################
      # ## HR ZONES PER TIME PERIOD SRC
      hr_summary <- sport.period.activities %>%
        # remove activities without HR
        filter(!is.na(hrmax.activity)) %>%
        group_by(ath.id) %>% 
        # filter(ath.id == "CMR_010") %>% 
        summarise(duration.min.total = sum(duration.min, na.rm = T),
                  hr.z0.min.total = sum(hr.z0.time, na.rm = T),
                  hr.z1.min.total = sum(hr.z1.time, na.rm = T),
                  hr.z2.min.total = sum(hr.z2.time, na.rm = T),
                  hr.z3.min.total = sum(hr.z3.time, na.rm = T),
                  hr.z4.min.total = sum(hr.z4.time, na.rm = T),
                  hr.z5.min.total = sum(hr.z5.time, na.rm = T),
                  hr.zones.min.total = hr.z0.min.total + hr.z1.min.total + hr.z2.min.total + hr.z3.min.total + hr.z4.min.total + hr.z5.min.total,
                  hr.zones.min.total.difference = round(hr.zones.min.total / duration.min.total * 100,1)
        ) %>% 
        mutate(hr.z0.perc.total = round(hr.z0.min.total / duration.min.total * 100, 1),
               hr.z1.perc.total = round(hr.z1.min.total / duration.min.total * 100, 1),
               hr.z2.perc.total = round(hr.z2.min.total / duration.min.total * 100, 1),
               hr.z3.perc.total = round(hr.z3.min.total / duration.min.total * 100, 1),
               hr.z4.perc.total = round(hr.z4.min.total / duration.min.total * 100, 1),
               hr.z5.perc.total = round(hr.z5.min.total / duration.min.total * 100, 1),
               hr.zones.perc.total = hr.z0.perc.total + hr.z1.perc.total + hr.z2.perc.total + hr.z3.perc.total + hr.z4.perc.total + hr.z5.perc.total) %>% 
        rename(duration.min.total.withHR = duration.min.total)
      
      #if there are no hr activities, create a row with 0/NA but with ath.id so it can be joined to the noHR activities
      if(dim(hr_summary)[1] == 0){
        hr_summary <- data.frame(ath.id = i,
                                 duration.min.total.withHR = 0,
                                 hr.z0.min.total = NA,
                                 hr.z1.min.total = NA,
                                 hr.z2.min.total = NA,
                                 hr.z3.min.total = NA,
                                 hr.z4.min.total = NA,
                                 hr.z5.min.total = NA,
                                 hr.zones.min.total = NA,
                                 hr.zones.min.total.difference = NA,
                                 hr.z0.perc.total = NA,
                                 hr.z1.perc.total = NA,
                                 hr.z2.perc.total = NA,
                                 hr.z3.perc.total = NA,
                                 hr.z4.perc.total = NA,
                                 hr.z5.perc.total = NA,
                                 hr.zones.perc.total = NA)
      }
      
      hr_summary_NOHR <- sport.period.activities %>%
        # get activities without HR
        filter(is.na(hrmax.activity)) %>% 
        group_by(ath.id) %>%
        summarise(duration.min.total.noHR = round(sum(duration.min, na.rm = T),1))
      
      hr_summary <- hr_summary %>% 
        left_join(hr_summary_NOHR, by = "ath.id") %>% 
        mutate(duration.min.total.noHR = ifelse(is.na(duration.min.total.noHR), 0, duration.min.total.noHR),
               duration.min.total = duration.min.total.withHR + duration.min.total.noHR,
               perc.total.withHR = round(duration.min.total.withHR / duration.min.total * 100,1)) %>% 
        relocate(duration.min.total.noHR, duration.min.total, perc.total.withHR, .after=duration.min.total.withHR)
      ###################################
      
      
      if (dim(hr_summary)[1] == 0) {
        res[res$ath.id == i,paste0(t,".duration.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".duration.min.total.withHR.",period)] <- NA
        res[res$ath.id == i,paste0(t,".duration.min.total.noHR.",period)] <- NA
        res[res$ath.id == i,paste0(t,".perc.total.withHR.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z0.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z1.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z2.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z3.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z4.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z5.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z0.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z1.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z2.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z3.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z4.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z5.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.zones.perc.total.",period)] <- NA

      } else {
        res[res$ath.id == i,paste0(t,".duration.min.total.",period)] <- hr_summary$duration.min.total
        res[res$ath.id == i,paste0(t,".duration.min.total.withHR.",period)] <- hr_summary$duration.min.total.withHR
        res[res$ath.id == i,paste0(t,".duration.min.total.noHR.",period)] <- hr_summary$duration.min.total.noHR
        res[res$ath.id == i,paste0(t,".perc.total.withHR.",period)] <- hr_summary$perc.total.withHR
        res[res$ath.id == i,paste0(t,".hr.z0.min.total.",period)] <- hr_summary$hr.z0.min.total
        res[res$ath.id == i,paste0(t,".hr.z1.min.total.",period)] <- hr_summary$hr.z1.min.total
        res[res$ath.id == i,paste0(t,".hr.z2.min.total.",period)] <- hr_summary$hr.z2.min.total
        res[res$ath.id == i,paste0(t,".hr.z3.min.total.",period)] <- hr_summary$hr.z3.min.total
        res[res$ath.id == i,paste0(t,".hr.z4.min.total.",period)] <- hr_summary$hr.z4.min.total
        res[res$ath.id == i,paste0(t,".hr.z5.min.total.",period)] <- hr_summary$hr.z5.min.total
        res[res$ath.id == i,paste0(t,".hr.z0.perc.total.",period)] <- hr_summary$hr.z0.perc.total
        res[res$ath.id == i,paste0(t,".hr.z1.perc.total.",period)] <- hr_summary$hr.z1.perc.total
        res[res$ath.id == i,paste0(t,".hr.z2.perc.total.",period)] <- hr_summary$hr.z2.perc.total
        res[res$ath.id == i,paste0(t,".hr.z3.perc.total.",period)] <- hr_summary$hr.z3.perc.total
        res[res$ath.id == i,paste0(t,".hr.z4.perc.total.",period)] <- hr_summary$hr.z4.perc.total
        res[res$ath.id == i,paste0(t,".hr.z5.perc.total.",period)] <- hr_summary$hr.z5.perc.total
        res[res$ath.id == i,paste0(t,".hr.zones.perc.total.",period)] <- hr_summary$hr.zones.perc.total
      }
      #### HR END
      # total sessions
      res[res$ath.id == i,paste0(t,".sessions.total.",period)] <- sport.period.activities %>%
        count() %>% as.numeric()
      # monthly sessions average
      res[res$ath.id == i,paste0(t,".sessions.monthavg.",period)] <- round(res[res$ath.id == i,paste0(t,".sessions.total.", period)] / days * 28, 1)
      # total trimps
      res[res$ath.id == i,paste0(t,".etrimp.total.",period)] <- sport.period.activities %>%
        summarise(total_trimp = round(sum(etrimp,na.rm=T),1)) %>% as.numeric()
      # res[res$ath.id == i,paste0(t,".lutrimp.total.",period)] <- sport.period.activities %>%
        # summarise(total_trimp = round(sum(lutrimp,na.rm=T),1)) %>% as.numeric()
      # res[res$ath.id == i,paste0(t,".lutrimp_goldvt.total.",period)] <- if (all(is.na(sport.period.activities$lutrimp.goldvt)))
        # {NA} else {
        # sport.period.activities %>% summarise(total_trimp = round(sum(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric() }
      # weekly average
      if(period != "prevweek") {
        res[res$ath.id == i,paste0(t,".sessions.wkavg.",period)] <- 
          round(res[res$ath.id == i,paste0(t,".sessions.total.",period)]/(days/7),1)
        res[res$ath.id == i,paste0(t,".etrimp.wkavg.",period)] <- 
          round(res[res$ath.id == i,paste0(t,".etrimp.total.",period)]/(days/7),1)
        # res[res$ath.id == i,paste0(t,".lutrimp.wkavg.",period)] <- 
        #   round(res[res$ath.id == i,paste0(t,".lutrimp.total.",period)]/(days/7),1)
        # res[res$ath.id == i,paste0(t,".lutrimp_goldvt.wkavg.",period)] <- 
        #   round(res[res$ath.id == i,paste0(t,".lutrimp_goldvt.total.",period)]/(days/7),1)
      }
      
      ################################
      # monotony
      if (period == "prevweek"){
        res[res$ath.id == i,paste0(t,".etrimp.monotony.",period)] <- sport.period.activities %>%
          summarise(monotony = round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),1)) %>% as.numeric()
        # res[res$ath.id == i,paste0(t,".lutrimp.monotony.",period)] <- sport.period.activities %>%
          # summarise(monotony = round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),1)) %>% as.numeric()
        # res[res$ath.id == i,paste0(t,".lutrimp_goldvt.monotony.",period)] <- if (all(is.na(sport.period.activities$lutrimp.goldvt)))
        # {NA} else { sport.period.activities %>%
            # summarise(monotony = round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric()}
      } else {
        wk_monot.etrimp <- NULL
        # wk_monot.lutrimp <- NULL
        # wk_monot.lutrimp_goldvt <- NULL

        for (wk in seq_len(days/7)){
          # loop through weeks
          # change loop as we need now before and after the test! (SRC UPDATE FEB 2023)
          if (grepl("next", period)){ # next periods
            wk_monot.etrimp <- c(wk_monot.etrimp, ath.activities %>%
                                   filter(as.Date(as.character(date),"%Y%m%d") > (t.date + (7*(wk-1))),
                                          as.Date(as.character(date),"%Y%m%d") <= (t.date + (7*wk))) %>%
                                   summarise(m=round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),2)) %>% as.numeric())
          } else { # previous periods
            wk_monot.etrimp <- c(wk_monot.etrimp, ath.activities %>%
                                   filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
                                          as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>%
                                   summarise(m=round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),2)) %>% as.numeric())
          }
          ##### remove lutrimp below
          # wk_monot.lutrimp <- c(wk_monot.lutrimp, ath.activities %>%
          #                         filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
          #                                as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>%
          #                         summarise(m=round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),2)) %>% as.numeric())
          # wk_monot.lutrimp_goldvt <- c(wk_monot.lutrimp_goldvt, ath.activities %>%
          #                                filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
          #                                       as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>%
          #                                summarise(m=round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),2)) %>% as.numeric())
        } # week loop
        res[res$ath.id == i,paste0(t,".etrimp.monotony.",period)] <- round(sum(wk_monot.etrimp,na.rm=T)/(days/7)/
                                                                             sd(wk_monot.etrimp,na.rm=T),2)
        # res[res$ath.id == i,paste0(t,".lutrimp.monotony.",period)] <- round(sum(wk_monot.lutrimp,na.rm=T)/(days/7)/
                                                                              # sd(wk_monot.lutrimp,na.rm=T),2)
        # res[res$ath.id == i,paste0(t,".lutrimp_goldvt.monotony.",period)] <- round(sum(wk_monot.lutrimp_goldvt,na.rm=T)/(days/7)/
                                                                                     # sd(wk_monot.lutrimp_goldvt,na.rm=T),2)
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
        # choose if next or previous
        if(grepl("next", period)){
          sport.period.activities <- ath.activities %>% 
            filter(sport_type == sport) %>%
            filter(as.Date(as.character(date),"%Y%m%d") > t.date,
                   as.Date(as.character(date),"%Y%m%d") <= t.date + days)
        } else {
          # get all activities for the the period
          sport.period.activities <- ath.activities %>% 
            filter(sport_type == sport) %>%
            filter(as.Date(as.character(date),"%Y%m%d") < t.date,
                   as.Date(as.character(date),"%Y%m%d") >= t.date - days)
        }
        
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
          summarise(total_duration = round(sum(duration.min, na.rm = T)/60,1)) %>% as.numeric()
        # weekly average
        if(period != "prevweek") {
          # hours
          res[res$ath.id == i,paste0(t,".",sp,".hours.wkavg.",period)] <-
            round(res[res$ath.id == i,paste0(t,".",sp,".hours.total.",period)]/(days/7),1)
        }
        # total distance in km
        res[res$ath.id == i,paste0(t,".",sp,".km.total.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(sum(total_dist.km, na.rm = T),2)) %>% as.numeric()
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
    
    # # between tests
    # if(test_i == 2) {
    #   # total sessions
    #   t1.date = tests[1]
    #   t2.date = tests[2]
    #   weeks.btw = round(as.numeric(t2.date - t1.date)/7,0)
    #   period.activities <- ath.activities %>% 
    #     filter(as.Date(as.character(date),"%Y%m%d") < t2.date,
    #            as.Date(as.character(date),"%Y%m%d") > t1.date)
    #   res[res$ath.id == i,"btwtests.weeks"] <- weeks.btw
    #   res[res$ath.id == i,"btwtests.sessions.total"] <- period.activities %>% count() %>% as.numeric()
    #   res[res$ath.id == i,"btwtests.sessions.wkavg"] <- round(res[res$ath.id == i,"btwtests.sessions.total"]/weeks.btw,1)
    #   
    #   #########
    #   # total trimps
    #   res[res$ath.id == i,paste0("btwtests.etrimp.total")] <- period.activities %>%
    #     summarise(total_trimp = round(sum(etrimp,na.rm=T),1)) %>% as.numeric()
    #   res[res$ath.id == i,paste0("btwtests.lutrimp.total")] <- period.activities %>%
    #     summarise(total_trimp = round(sum(lutrimp,na.rm=T),1)) %>% as.numeric()
    #   res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.total")] <- if (all(is.na(period.activities$lutrimp.goldvt)))
    #   {NA} else {period.activities %>% summarise(total_trimp = round(sum(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric() }
    #   # weekly average
    #   res[res$ath.id == i,paste0("btwtests.etrimp.wkavg")] <- 
    #     round(res[res$ath.id == i,paste0("btwtests.etrimp.total")]/weeks.btw,1)
    #   res[res$ath.id == i,paste0("btwtests.lutrimp.wkavg")] <- 
    #     round(res[res$ath.id == i,paste0("btwtests.lutrimp.total")]/weeks.btw,1)
    #   res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.wkavg")] <- 
    #     round(res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.total")]/weeks.btw,1)
    #   # monotony
    #   wk_monot.etrimp <- NULL
    #   wk_monot.lutrimp <- NULL
    #   wk_monot.lutrimp_goldvt <- NULL
    #   for (wk in seq_len(weeks.btw)){
    #     # loop through weeks
    #     wk_monot.etrimp <- c(wk_monot.etrimp, ath.activities %>% 
    #                            filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
    #                                   as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
    #                            summarise(m=round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),2)) %>% as.numeric())
    #     wk_monot.lutrimp <- c(wk_monot.lutrimp, ath.activities %>% 
    #                             filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
    #                                    as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
    #                             summarise(m=round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),2)) %>% as.numeric())
    #     wk_monot.lutrimp_goldvt <- c(wk_monot.lutrimp_goldvt,
    #                   ath.activities %>% 
    #                     filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
    #                            as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
    #                     summarise(m=round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),2)) %>% as.numeric())
    #   } # week loop
    #   res[res$ath.id == i,paste0("btwtests.etrimp.monotony")] <- round(sum(wk_monot.etrimp,na.rm=T)/weeks.btw/
    #                                                                      sd(wk_monot.etrimp,na.rm=T),2)
    #   res[res$ath.id == i,paste0("btwtests.lutrimp.monotony")] <- round(sum(wk_monot.lutrimp,na.rm=T)/weeks.btw/
    #                                                                       sd(wk_monot.lutrimp,na.rm=T),2)
    #   res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.monotony")] <-
    #     round(sum(wk_monot.lutrimp_goldvt,na.rm=T)/weeks.btw/sd(wk_monot.lutrimp_goldvt,na.rm=T),2)
    #   # monotony calculation
    #   #########
    #   
    #   # sport loop
    #   for (sport in unique(ath.activities$sport_type)) {
    #     # to name variable
    #     if (sport == "Running") {
    #       sp="run"
    #     } else if (sport == "Cycling") {
    #       sp="bike"
    #     } else {
    #       sp="other"
    #     }
    #     # get all activities for the sport and the period
    #     sport.period.activities <- ath.activities %>% 
    #       filter(sport_type == sport) %>% 
    #       filter(as.Date(as.character(date),"%Y%m%d") < t2.date,
    #              as.Date(as.character(date),"%Y%m%d") > t1.date)
    #     
    #     if (dim(sport.period.activities)[1] == 0) {
    #       # go to next sport
    #       res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".km.total")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.avg")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".km.session.avg")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.max")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".km.session.max")] <- 0
    #       next
    #     }
    #     # total sessions
    #     res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")] <- sport.period.activities %>%
    #       count() %>% as.numeric()
    #     # weekly average sessions
    #     res[res$ath.id == i,paste0("btwtests.",sp,".sessions.wkavg")] <- 
    #       round(res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")]/weeks.btw,1)
    #     # total hours
    #     res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")] <- sport.period.activities %>%
    #       summarise(total_duration = round(sum(duration.min, na.rm = T)/60,1)) %>% as.numeric()
    #     # weekly average hours
    #     res[res$ath.id == i,paste0("btwtests.",sp,".hours.wkavg")] <- 
    #       round(res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")]/weeks.btw,1)
    #     # total distance in km
    #     res[res$ath.id == i,paste0("btwtests.",sp,".km.total")] <- sport.period.activities %>%
    #       summarise(total_distance = round(sum(total_dist.km, na.rm = T),2)) %>% as.numeric()
    #     # weekly average distance
    #     res[res$ath.id == i,paste0("btwtests.",sp,".km.wkavg")] <- 
    #       round(res[res$ath.id == i,paste0("btwtests.",sp,".km.total")]/weeks.btw,2)
    #     # single session average hours
    #     res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.avg")] <- sport.period.activities %>%
    #       summarise(total_distance = round(mean(duration.min)/60,1)) %>% as.numeric()
    #     # single session average distance
    #     res[res$ath.id == i,paste0("btwtests.",sp,".km.session.avg")] <- sport.period.activities %>%
    #       summarise(total_distance = round(mean(total_dist.km),2)) %>% as.numeric()
    #     # single session max hours
    #     res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.max")] <- sport.period.activities %>%
    #       summarise(total_distance = round(max(duration.min)/60,1)) %>% as.numeric()
    #     # single session max distance
    #     res[res$ath.id == i,paste0("btwtests.",sp,".km.session.max")] <- sport.period.activities %>%
    #       summarise(total_distance = round(max(total_dist.km),2)) %>% as.numeric()
    #   } # end sport loop
    # } # end between test calculations
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
save(ath.info.test, tp.newzones, tpeaks.all, all.activities, error.sessions, dup.sessions, file=
       paste0("out/rdas/trainingpeaks_results_",format(as.Date(Sys.Date()),format="%y%m%d"),".rda"))


## -----------------------------------------------------------------------------
# load('trainingpeaks_data_PARALLEL_200701.rda')
# 
# save CSV
# write.csv(tp.newzones,file=paste0("out/csv/tpeaks_summary_newzones_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
# write.csv(tpeaks.all, file=paste0("out/csv/tpeaks_summary_all_",format(as.Date(Sys.Date()),format="%y%m%d"),".csv"))
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

# negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
# wb <- all.activities[1:10]
# conditionalFormatting(wb, "cellIs", cols = 4, rows = 1:5, rule = ">0", style = negStyle)
# t_act <- all.activities
# write.xlsx(, keepNA=TRUE, firstActiveRow = 2, firstActiveCol = 3,
#            file=paste0("out/test_",format(as.Date(Sys.Date()),format="%y%m%d"),".xlsx"))

