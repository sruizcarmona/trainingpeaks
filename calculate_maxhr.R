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
# https://alistaire.rbind.io/blog/coalescing-joins/
coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  
    joined <- join(x, y, by = by, suffix = suffix, ...)
    # names of desired output
    cols <- union(names(x), names(y))
    
    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    # remove suffixes and deduplicate
    to_coalesce <- unique(substr(
        to_coalesce, 
        1, 
        nchar(to_coalesce) - nchar(suffix_used)
    ))
    
    coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
        joined[[paste0(.x, suffix[1])]], 
        joined[[paste0(.x, suffix[2])]]
    ))
    names(coalesced) <- to_coalesce
    
    dplyr::bind_cols(joined, coalesced)[cols] %>% 
      mutate_if(is.character, list(~na_if(.,"")))
}


## -----------------------------------------------------------------------------
# DECLARE NAME OF ATHLETES TO ANALYZE
# sel.athletes <- c("NAME")
athlete_covid_dates_old <- read.csv('data/athlete_covid_dates.csv', stringsAsFactors = F)
athlete_covid_dates <- read_csv("data/Overview_20220509.csv")
athlete_covid_dates <- athlete_covid_dates %>% 
  mutate(diagnose_date = coalesce(diagnose_datum, diagnose2_datum)) %>% 
  select(athlete, MaxHR, diagnose_date, vax_1 = `Vaccin 1`, vax_2 = `Vaccin 2`, vax_3 = `Vaccin 3`, vax_4 = `Vaccin 4`)

athlete_covid_dates <- coalesce_join(athlete_covid_dates, athlete_covid_dates_old, by="athlete")

# add maxhr from old lists
####################################################
#### maxHR
if (file.exists('data/TP_data_covidex_covivax_MAXHR_220302.csv')){
  lab.maxHR_old <- read.csv('data/TP_data_covidex_covivax_MAXHR_220302.csv')
  lab.maxHR_old <- lab.maxHR_old %>%
    mutate(name = gsub(" ", "", paste0(toupper(Name), toupper(surname))),
           MaxHR = maxhr) %>%
    select(athlete=name, MaxHR ) %>%
    drop_na()
  athlete_covid_dates <- coalesce_join(athlete_covid_dates, lab.maxHR_old, by="athlete")
}

lab.maxHR <- athlete_covid_dates %>% 
  select(name = athlete, maxHR = MaxHR) %>% 
  drop_na()

sel.athletes <- athlete_covid_dates$athlete

# all folders in PRO_HEART, where sel.athletes folders are located
# ph_folders1 <- list.dirs('../COVIVAX',recursive = F)
# ph_folders2 <- list.dirs('../COVIDEX',recursive = F)
# all.dirs.PH <- c(ph_folders1, ph_folders2)

all.dirs.PH <- list.dirs('../2205_ACTIVITIES/', recursive = F)


## -----------------------------------------------------------------------------
if(file.exists('out/rdas/tpeaks_newzones.rda')){
  load('out/rdas/tpeaks_newzones.rda')
}


## -----------------------------------------------------------------------------
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
# get the most updated one, for duplicates
if(exists("lab.goldVT")){
  lab.goldVT <- lab.goldVT %>% 
    arrange(ath.name) %>% 
    group_by(ath.name) %>% 
    # mutate(i=row_number(),
    #        n=n()) %>%
    # filter(n==2) %>%
    filter(row_number()==n()) %>%
    ungroup()
}

####################################################
#### covid dates
# ath.coviddates <- readr::read_csv('data/athlete_covid_dates.csv')
athlete_covid_dates <- athlete_covid_dates %>% 
  mutate(diagnose_date = parse_date(diagnose_date,format="%d/%m/%Y"),
         vax_1 = parse_date(vax_1,format="%d/%m/%Y"),
         vax_2 = parse_date(vax_2,format="%d/%m/%Y"),
         vax_3 = parse_date(vax_3,format="%d/%m/%Y"),
         vax_4 = parse_date(vax_4,format="%d/%m/%Y"))


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
    tp.newzones <- data.frame(name=athlete, ath.id=paste0("CV_",str_pad(1,3,pad="0")),
                              maxHR= NA, stringsAsFactors=FALSE)
  } else {
    # if there is no row for the athlete, create one
    if(!athlete %in% tp.newzones$name){
      writeLines(c("Unknown athlete, creating a new entry."))
      tp.newzones[nrow(tp.newzones)+1,'name'] <- athlete
      tp.newzones$ath.id[tp.newzones$name == athlete] <- paste0("CV_",str_pad(dim(tp.newzones)[1],3,pad="0"))
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
  if(exists("lab.goldVT") && any(lab.goldVT$ath.name == athlete)){
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
  # save(tp.newzones,file='out/rdas/tpeaks_newzones.rda')
}
# stop cluster
stopCluster(cl)
# save everything in a file, so we don't need to calculate them every time
save(tp.newzones,file='out/rdas/tpeaks_newzones.rda')
tp.newzones


