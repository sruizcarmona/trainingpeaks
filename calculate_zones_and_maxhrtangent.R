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
source('src/f_gettcxactivity.R')


## -----------------------------------------------------------------------------
# DECLARE NAME OF ATHLETES TO ANALYZE
# sel.athletes <- c("NAME")
sel.athletes <- read.table("data/list_athletes.txt",stringsAsFactors = F)$V1
sel.athletes <- c(sel.athletes, read.table("data/list_athletes_2107.txt",stringsAsFactors = F)$V1)
sel.athletes <- c(sel.athletes, read.table("data/list_athletes_2201.txt",stringsAsFactors = F)$V1)
# some athletes are duplicated on the july update
sel.athletes <- unique(sel.athletes)
#sel.athletes <- sel.athletes[c(9,2)]

# all folders in PRO_HEART, where sel.athletes folders are located
ph_folders1 <- list.dirs('../../2101_TRAININGPEAKS/PRO_HEART',recursive = F)
ph_folders2 <- list.dirs(list.dirs('../../2003_TRAININGPEAKS/PRO_HEART',recursive = F),recursive=F)
ph_folders3 <- list.dirs('../../2101_TRAININGPEAKS/MASTER_HEART',recursive = F)
ph_folders4 <- list.dirs(list.dirs('../../2107_TRAININGPEAKS', recursive=F), recursive = F)
ph_folders5 <- list.dirs(list.dirs('../../2201_UPDATE', recursive=F), recursive = F)
all.dirs.PH <- c(ph_folders1, ph_folders2, ph_folders3, ph_folders4, ph_folders5)


## -----------------------------------------------------------------------------
if(file.exists('out/rdas/tpeaks_newzones.rda')){
  load('out/rdas/tpeaks_newzones.rda')
}


## -----------------------------------------------------------------------------
####################################################
#### maxHR
if (file.exists('data/athlete_maxHR.txt')){
  lab.maxHR <- read.table('data/athlete_maxHR.txt')
  colnames(lab.maxHR) <- c('name','maxHR')
}
# add july update
if (file.exists('data/athlete_maxHR_2107.txt')){
  lab.maxHR <- rbind(lab.maxHR,
                     setNames(read.table('data/athlete_maxHR_2107.txt'),c('name','maxHR')))
}
# add jan 2022 update
if (file.exists('data/athlete_maxHR_2201.txt')){
  lab.maxHR <- rbind(lab.maxHR,
                     setNames(read.table('data/athlete_maxHR_2201.txt'),c('name','maxHR')))
}
# get the most updated one, for duplicates
lab.maxHR <- lab.maxHR %>% 
  arrange(name) %>% 
  group_by(name) %>% 
  filter(row_number()==n()) %>% 
  ungroup()

####################################################
#### gold VT
if (file.exists('data/athlete_goldVT.csv')){
  lab.goldVT <- read.csv('data/athlete_goldVT.csv')
  lab.goldVT$gold.VT1 <- round(lab.goldVT$gold.VT1,2)
  lab.goldVT$gold.VT2 <- round(lab.goldVT$gold.VT2,2)
}

# add july update
if (file.exists('data/athlete_goldVT_2107.csv')){
  lab.goldVT <- rbind(lab.goldVT, read.csv('data/athlete_goldVT_2107.csv') %>% 
                        mutate(gold.VT1=round(gold.VT1,2),
                               gold.VT2=round(gold.VT2,2)))
}

# add jan 2022 update
if (file.exists('data/athlete_goldVT_2201.csv')){
       lab.goldVT <- rbind(lab.goldVT, read.csv('data/athlete_goldVT_2201.csv') %>%
                                mutate(gold.VT1=round(gold.VT1,2),
                                       gold.VT2=round(gold.VT2,2)))
}

# get the most updated one, for duplicates
lab.goldVT <- lab.goldVT %>% 
  arrange(ath.name) %>% 
  group_by(ath.name) %>% 
  # mutate(i=row_number(),
  #        n=n()) %>%
  # filter(n==2) %>%
  filter(row_number()==n()) %>%
  ungroup()

####################################################
#### test dates
if (file.exists('data/athlete_testdates.csv')){
  ath.testdates <- readr::read_csv('data/athlete_testdates.csv',
                                   col_types = cols("c",col_date(format="%d/%m/%y"),
                                                    col_date(format="%d/%m/%y")))
}
# add july update
if (file.exists('data/athlete_testdates_2107.csv')){
  ath.testdates <- rbind(ath.testdates,
                         readr::read_csv('data/athlete_testdates_2107.csv',
                                         col_types = cols("c",col_date(format="%d/%m/%y"),
                                                          col_date(format="%d/%m/%y"))))
}
# add jan 2022 update
if (file.exists('data/athlete_testdates_2201.csv')){
  ath.testdates <- rbind(ath.testdates,
                         readr::read_csv('data/athlete_testdates_2201.csv',
                                         col_types = cols("c",col_date(format="%d/%m/%Y"),
                                                          col_date(format="%d/%m/%Y"))))
}

# get the first one, as the second one is only 1 test date update
ath.testdates <- ath.testdates %>% 
  arrange(name) %>% 
  group_by(name) %>% 
  # mutate(i=row_number(),
  #        n=n(),
  #        nas=ifelse(is.na(test_date_2),2,1)) %>%
  # filter(n==2) %>%
  filter(row_number()==1) %>%
  ungroup()


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

