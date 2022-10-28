############################################################################################################
############################################################################################################
#################################       FUNCTIONS FOR MAIN SECTION       ###################################
############################################################################################################
############################################################################################################

############################################################################################################
## LOAD LIBRARIES
############################################################################################################

library(fit)
library(dplyr)
library(stringr)
library(foreach)
library(doParallel)
library(zoo)
library(splines)

############################################################################################################
## SMOOTH DATA
## input point and return them smoothed with a window w
###
## input point vector and window w
## output same vector with the data points smoothed with a window w
############################################################################################################

smooth.data <- function(data,w,r=2){
  sm <- rollapply(data,width=w,function(...) {mean(...,na.rm=T)},partial=T,align="center")
  return(round(sm,r))
}

############################################################################################################
## GET DATE
## get real date from garmin timestamp (after garmin was born)
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html  
## http://www.nlvocables.com/blog/?p=969 (garmin timestamp)
############################################################################################################

get.date_GARMIN <- function(tstmp){
  as.POSIXct(tstmp, origin="1989-12-31",tz="UTC")
}

############################################################################################################
## SINGLE ROW DF (REPETITIVE CALL)
## return a df of 1 row and colnames for error activities
############################################################################################################
onerow.df <- function(values,colnames){
  return(setNames(data.frame(matrix(values, ncol=length(colnames), nrow=1),stringsAsFactors = FALSE), colnames))
}

############################################################################################################
## FUNCTION TO READ FILE 
## read file, find extension for format and return fitdata
## no matter what format it is, usable by both the HR determination and the activity
###
## input filename
## output fitdata
############################################################################################################

create_fitdata <- function(f) {
  # identify file format from extension
  if (str_detect(f,"\\.fit|\\.FIT")){
    fitdata <- try(read.fit(f),silent=T)
  } else if (str_detect(f,"\\.gpx|\\.GPX")){
    fitdata <- try(create.fitdata_from_gpx(f),silent=T)
  } else if (str_detect(f,"\\.tcx|\\.TCX")) {
    fitdata <- try(create.fitdata_from_tcx(f),silent=T)
  } else {
    # if here, it means there is no known extension, so will try fit > gpx > tcx and see if any works
    fitdata <- try(read.fit(f),silent=T)
    if (class(fitdata) == "try-error"){
      fitdata <- try(create.fitdata_from_gpx(f),silent=T)
      if (class(fitdata) == "try-error"){
        fitdata <- try(create.fitdata_from_tcx(f),silent=T)
      }
    }
  }
  return(fitdata)
}

############################################################################################################
## MAIN FUNCTION 
## extract heart rate, power and speed from a file
###
## input filename and maxHR
## output dataframe with hr,power, speed and correlations in columns
############################################################################################################

get.act_info_from_fitdata <- function(fitdata, ath.id, cmr = FALSE) {
  ###################
  # initialize activity (a) summary
  ###################
  date <- if(!is.null(fitdata$session$start_time)) {get.date_GARMIN(fitdata$session$start_time)} else {get.date_GARMIN(fitdata$record$timestamp[1])}
  a <- onerow.df(format(date,"%Y%m%d"),"date")
  a$year <- format(date,"%Y")
  a$month <- format(date,"%m")
  a$week <- format(date,"%W")
  a$start_time <- as.character(format(date,"%H:%M"))
  a$duration.min <- if (is.null(fitdata$session$total_timer)) {round((last(fitdata$record$timestamp)-first(fitdata$record$timestamp))/60,2)} else {round(fitdata$session$total_timer_time/60,2)}
  a$pause.min <- if(is.null(fitdata$session$total_elapsed_time)){NA}else{round((fitdata$session$total_elapsed_time-fitdata$session$total_timer_time)/60,2)}
  a$total_dist.km <- if(is.null(fitdata$session$total_distance)) {round(last(fitdata$record$distance/1000,2))} else {round(fitdata$session$total_distance/1000,2)}
  a$cum_ascent.m <- if(is.null(fitdata$session$total_ascent)){NA}else{fitdata$session$total_ascent}
  # add info of sensor
  a$hr.sensor <- ifelse(120 %in% fitdata$device_info$device_type,TRUE,FALSE)
  # add info about device (brand and product code and name)
  a$device_brand_id <- if(!is.null(fitdata$file_id$manufacturer)){fitdata$file_id$manufacturer} else if (!is.null(fitdata$session$device_brand_id)){fitdata$session$device_brand_id} else {0}
  a$device_brand_name <- ifelse(a$device_brand_id %in% brand_id$brand_id, brand_id$brand[brand_id$brand_id == a$device_brand_id], "unknown")
  a$device_model_id <- if(!is.null(fitdata$file_id$product)){fitdata$file_id$product} else {0}
  a$device_model_name <- ifelse(!is.null(fitdata$session$device_model_name),
                                fitdata$session$device_model_name,
                                ifelse(a$device_model_id %in% product_id$product_id[product_id$brand == a$device_brand_name],
                                       product_id %>% filter(brand == a$device_brand_name & product_id == a$device_model_id) %>% pull(model),
                                       "unknown"))
  # sport info
  a$sport_code   <- ifelse(!is.null(fitdata$session$sport),
                           fitdata$session$sport,
                           ifelse(!is.null(fitdata$sport$sport), 
                                  fitdata$sport$sport,
                                  NA))
  a$sport_type   <- ifelse(is.na(a$sport_code), NA, names(sport_code)[sport_code == a$sport_code])
  # heart stuff
  a$hrmax_athlete <- tp.newzones$maxHR[tp.newzones$ath.id == ath.id]
  ###################
  # remove first and last 10 seconds, to reduce risk of peaks in sensor pairing, gps or other stuff
  fd <- fitdata$record[11:(dim(fitdata$record)[1]-10),]
  rownames(fd) <- NULL
  ###################
  # get info from heart_rate
  # check that heart_rate is present in fitdata and its above 10%, otherwise return NA
  if ("heart_rate" %in% names(fd) & sum(!is.na(fd$heart_rate))/length(fd$heart_rate) > 0.1 & sum(fd$heart_rate != 0, na.rm=T)/length(fd$heart_rate) > 0.1){
    # if, after that, the first 20 values are above 180, clean them too, as they are probably artifacts
    if(any(fd$heart_rate[1:20] > 180, na.rm = T)){
      fd$heart_rate[1:20] <- NA
    }
    # fix sensor based on brand or model, given there is HR info
    a$hr.sensor = ifelse(a$device_brand_name %in% real_chest_brand, TRUE, a$hr.sensor)
    a$hr.sensor = ifelse(grepl(paste(real_chest_model, collapse="|"), a$device_model_name), TRUE, a$hr.sensor)
    # clean hr=0, as sometimes the sensor fails
    fd$heart_rate[fd$heart_rate == 0] <- NA
    # smooth hr
    # hr above maxHR reduce to maxHR as it has already been processed when getting the zones (heavier smooth)
    smooth.hr <- smooth.data(fd$heart_rate,10)
    smooth.hr[smooth.hr=="NaN"] <- NA
    # smooth.hr[smooth.hr > ath.maxHR] <- ath.maxHR
    # process all from smooth.hr
    a$hrmax.activity <- round(max(smooth.hr,na.rm=T),0)
    a$hrmax.perc <- round(a$hrmax.activity/a$hrmax_athlete*100,1)
    a$hrmax.intensity <- round(a$hrmax.perc * a$duration.min,1)
    a$hr.avg <- round(mean(smooth.hr,na.rm=T),1)
    # get hr.zones and times (standard %s)
    hr.zones <- quantile(c(0:a$hrmax_athlete),probs=seq(0,1,by=0.1))
    fd$hr.zones <- findInterval(smooth.hr,hr.zones[6:10])
    hr.zones.table <- round(table(fd$hr.zones)/length(fd$hr.zones)*100,2)
    hr.zones.table[c("0","1","2","3","4","5")[!c("0","1","2","3","4","5") %in% names(hr.zones.table)]] <- 0
    a$hr.z01 <- as.numeric(hr.zones.table['1'] + hr.zones.table['0'])
    a$hr.z2 <- as.numeric(hr.zones.table['2'])
    a$hr.z3 <- as.numeric(hr.zones.table['3'])
    a$hr.z4 <- as.numeric(hr.zones.table['4'])
    a$hr.z5 <- as.numeric(hr.zones.table['5'])
    a$hr.z1.time <- as.numeric(round(hr.zones.table['1']/100 * a$duration.min,1))
    a$hr.z2.time <- as.numeric(round(hr.zones.table['2']/100 * a$duration.min,1))
    a$hr.z3.time <- as.numeric(round(hr.zones.table['3']/100 * a$duration.min,1))
    a$hr.z4.time <- as.numeric(round(hr.zones.table['4']/100 * a$duration.min,1))
    a$hr.z5.time <- as.numeric(round(hr.zones.table['5']/100 * a$duration.min,1))
    # ONLY IF NOT CMR (NO NEED AFTER 221028)
    if (cmr == FALSE) {
      # same with vt zones
      vt.zones <- quantile(c(0:a$hrmax_athlete),probs=c(0,0.83,0.94,1))
      fd$vt.zones <- findInterval(smooth.hr,vt.zones)
      vt.zones.table <- round(table(fd$vt.zones)/length(fd$vt.zones)*100,2)
      vt.zones.table[c("1","2","3")[!c("1","2","3") %in% names(vt.zones.table)]] <- 0
      a$vt.z1 <- as.numeric(vt.zones.table['1'])
      a$vt.z2 <- as.numeric(vt.zones.table['2'])
      a$vt.z3 <- as.numeric(vt.zones.table['3'])
      a$vt.z1.time <- as.numeric(round(vt.zones.table['1']/100 * a$duration.min,1))
      a$vt.z2.time <- as.numeric(round(vt.zones.table['2']/100 * a$duration.min,1))
      a$vt.z3.time <- as.numeric(round(vt.zones.table['3']/100 * a$duration.min,1))
      ############################# GOLD VT
      # same with GOLD vt zones
      goldvt.zones <- tp.newzones[tp.newzones$ath.id == ath.id, c("gold.vt1", "gold.vt2")]
      if (any(is.na(goldvt.zones))){
        a <- cbind(a, onerow.df(NA,colnames=c("goldvt.z1","goldvt.z2","goldvt.z3",
                                              "goldvt.z1.time","goldvt.z2.time","goldvt.z3.time")))
      } else {
        fd$goldvt.zones <- findInterval(smooth.hr,goldvt.zones) + 1
        goldvt.zones.table <- round(table(fd$goldvt.zones)/length(fd$goldvt.zones)*100,2)
        goldvt.zones.table[c("1","2","3")[!c("1","2","3") %in% names(goldvt.zones.table)]] <- 0
        a$goldvt.z1 <- as.numeric(goldvt.zones.table['1'])
        a$goldvt.z2 <- as.numeric(goldvt.zones.table['2'])
        a$goldvt.z3 <- as.numeric(goldvt.zones.table['3'])
        a$goldvt.z1.time <- as.numeric(round(goldvt.zones.table['1']/100 * a$duration.min,1))
        a$goldvt.z2.time <- as.numeric(round(goldvt.zones.table['2']/100 * a$duration.min,1))
        a$goldvt.z3.time <- as.numeric(round(goldvt.zones.table['3']/100 * a$duration.min,1))
      }
    }
    ############################# END GOLD VT
    # calculate trimp scores
    a$etrimp <- round(a$hr.z1.time * 1 + a$hr.z2.time * 2 + a$hr.z3.time * 3 + a$hr.z4.time * 4 + a$hr.z5.time * 5,2)
    # ONLY IF NOT CMR (NO NEED AFTER 221028)
    if (cmr == FALSE) {
      a$lutrimp <- round(a$vt.z1.time * 1 + a$vt.z2.time * 2 + a$vt.z3.time * 3,2)
      # trimp goldvt
      if (any(is.na(goldvt.zones))){
        a$lutrimp.goldvt <- NA
      } else {
        a$lutrimp.goldvt <- round(a$goldvt.z1.time * 1 + a$goldvt.z2.time * 2 + a$goldvt.z3.time * 3,2)
      }
    }
  } else {
    # add same columns with NA
    if (cmr == FALSE) {
      a <- cbind(a, onerow.df(NA,colnames=c("hrmax.activity","hrmax.perc","hrmax.intensity","hr.avg",
                                            "hr.z01","hr.z2","hr.z3","hr.z4","hr.z5",
                                            "hr.z1.time","hr.z2.time","hr.z3.time","hr.z4.time","hr.z5.time",
                                            "vt.z1","vt.z2","vt.z3","vt.z1.time","vt.z2.time","vt.z3.time",
                                            "goldvt.z1","goldvt.z2","goldvt.z3",
                                            "goldvt.z1.time","goldvt.z2.time","goldvt.z3.time",
                                            "etrimp","lutrimp",
                                            "lutrimp.goldvt")))
    } else {
      a <- cbind(a, onerow.df(NA,colnames=c("hrmax.activity","hrmax.perc","hrmax.intensity","hr.avg",
                                            "hr.z01","hr.z2","hr.z3","hr.z4","hr.z5",
                                            "hr.z1.time","hr.z2.time","hr.z3.time","hr.z4.time","hr.z5.time",
                                            "etrimp")))
    }
  }
  # ONLY IF NOT CMR (NO NEED AFTER 221028)
  if (cmr == FALSE) {
    ###################
    # get info from power
    # check that power is present in fitdata and its above 10%, otherwise return NA
    pow.cor.check <- tp.newzones$pow.cor[tp.newzones$ath.id == ath.id]
    if ("power" %in% names(fd) & sum(!is.na(fd$power))/length(fd$power) > 0.1 & !(is.na(pow.cor.check))) {
      smooth.pow <- smooth.data(fd$power,100)
      smooth.pow <- smooth.pow[smooth.pow != "NaN" & smooth.pow != 0 & !is.na(smooth.pow)]
      # get zones
      pow.zones.table <- tp.newzones[tp.newzones$ath.id == ath.id,c("pow.z1.thld", "pow.z2.thld", "pow.z3.thld",
                                                                    "pow.z4.thld", "pow.z5.thld")]
      pow.zones <- findInterval(smooth.pow,pow.zones.table)
      pow.zones.summary <- round(table(pow.zones)/length(pow.zones)*100,2)
      pow.zones.summary[c("0","1","2","3","4","5")[!c("0","1","2","3","4","5") %in% names(pow.zones.summary)]] <- 0
      pow.z1.time <- as.numeric(round(pow.zones.summary['1']/100 * a$duration.min,1))
      pow.z2.time <- as.numeric(round(pow.zones.summary['2']/100 * a$duration.min,1))
      pow.z3.time <- as.numeric(round(pow.zones.summary['3']/100 * a$duration.min,1))
      pow.z4.time <- as.numeric(round(pow.zones.summary['4']/100 * a$duration.min,1))
      pow.z5.time <- as.numeric(round(pow.zones.summary['5']/100 * a$duration.min,1))
      # same with vt zones
      pow.vt.zones.table <- tp.newzones[tp.newzones$ath.id == ath.id,c("pow.vt.z1.thld", "pow.vt.z2.thld")]
      pow.vt.zones <- findInterval(smooth.pow,pow.vt.zones.table) + 1 # add one to correct, as zone 0 should be 1, etc
      pow.vt.zones.summary <- round(table(pow.vt.zones)/length(pow.vt.zones)*100,2)
      pow.vt.zones.summary[c("1","2","3")[!c("1","2","3") %in% names(pow.vt.zones.summary)]] <- 0
      pow.vt.z1.time <- as.numeric(round(pow.vt.zones.summary['1']/100 * a$duration.min,1))
      pow.vt.z2.time <- as.numeric(round(pow.vt.zones.summary['2']/100 * a$duration.min,1))
      pow.vt.z3.time <- as.numeric(round(pow.vt.zones.summary['3']/100 * a$duration.min,1))
      # new trimp.pow scores
      a$etrimp.power <- round(pow.z1.time*1 + pow.z2.time*2 + pow.z3.time*3 + pow.z4.time*4 + pow.z5.time*5,2)
      a$lutrimp.power <- round(pow.vt.z1.time * 1 + pow.vt.z2.time * 2 + pow.vt.z3.time * 3,2)
    } else {
      a <- cbind(a, onerow.df(NA,colnames=c("etrimp.power","lutrimp.power")))
    }
    ###################
    # get info from speed
    # check that speed is present in fitdata and its above 10%, otherwise return NA
    speed.cor.check <- tp.newzones$speed.cor[tp.newzones$ath.id == ath.id]
    if ("speed" %in% names(fd) & sum(!is.na(fd$speed))/length(fd$speed) > 0.1) {
      # smooth speed
      # remove Inf from fd$speed
      smooth.speed <- smooth.data(3.79*fd$speed[is.finite(fd$speed)],20)
      smooth.speed <- smooth.speed[smooth.speed != "NaN" & !is.na(smooth.speed)]
      # run trimp scores, only if speedcorcheck exists and its higher than 0.2
      if(is.na(speed.cor.check) | speed.cor.check < 0.2) {
        a <- cbind(a, onerow.df(NA,colnames=c("etrimp.speed","lutrimp.speed")))
      } else {
        # get zones
        speed.zones.table <- tp.newzones[tp.newzones$ath.id == ath.id,c("speed.z1.thld", "speed.z2.thld", "speed.z3.thld", 
                                                                        "speed.z4.thld", "speed.z5.thld")]
        speed.zones <- findInterval(smooth.speed,speed.zones.table)
        speed.zones.summary <- round(table(speed.zones)/length(speed.zones)*100,2)
        speed.zones.summary[c("0","1","2","3","4","5")[!c("0","1","2","3","4","5") %in% names(speed.zones.summary)]] <- 0
        speed.z1.time <- as.numeric(round(speed.zones.summary['1']/100 * a$duration.min,1))
        speed.z2.time <- as.numeric(round(speed.zones.summary['2']/100 * a$duration.min,1))
        speed.z3.time <- as.numeric(round(speed.zones.summary['3']/100 * a$duration.min,1))
        speed.z4.time <- as.numeric(round(speed.zones.summary['4']/100 * a$duration.min,1))
        speed.z5.time <- as.numeric(round(speed.zones.summary['5']/100 * a$duration.min,1))
        # same with vt zones
        speed.vt.zones.table <- tp.newzones[tp.newzones$ath.id == ath.id,c("speed.vt.z1.thld", "speed.vt.z2.thld")]
        speed.vt.zones <- findInterval(smooth.speed,speed.vt.zones.table) + 1
        speed.vt.zones.summary <- round(table(speed.vt.zones)/length(speed.vt.zones)*100,2)
        speed.vt.zones.summary[c("1","2","3")[!c("1","2","3") %in% names(speed.vt.zones.summary)]] <- 0
        speed.vt.z1.time <- as.numeric(round(speed.vt.zones.summary['1']/100 * a$duration.min,1))
        speed.vt.z2.time <- as.numeric(round(speed.vt.zones.summary['2']/100 * a$duration.min,1))
        speed.vt.z3.time <- as.numeric(round(speed.vt.zones.summary['3']/100 * a$duration.min,1))
        # new trimp.speed scores
        a$etrimp.speed <- round(speed.z1.time*1 + speed.z2.time*2 + speed.z3.time*3 + speed.z4.time*4 + speed.z5.time*5,2)
        a$lutrimp.speed <- round(speed.vt.z1.time * 1 + speed.vt.z2.time * 2 + speed.vt.z3.time * 3,2)
      }
      # extra speed stuff
      a$speed.avg <- round(mean(smooth.speed,na.rm=T),1)
      a$speed.max <- round(max(smooth.speed,na.rm=T),1)
    } else {
      a <- cbind(a, onerow.df(NA,colnames=c("etrimp.speed","lutrimp.speed","speed.avg","speed.max")))
    }
  }
  ###################
  ## EXTRA STUFF
  ## extracted from fitfile session info
  # ONLY IF NOT CMR (NO NEED AFTER 221028)
  if (cmr == FALSE) {
    a$cal          <- if (is.null(fitdata$session$total_calories)) {NA} else {fitdata$session$total_calories}
    a$power.max    <- if (is.null(fitdata$session$max_power)) {NA} else {fitdata$session$max_power}
    a$power.avg    <- if (is.null(fitdata$session$avg_power)) {NA} else {fitdata$session$avg_power}
    a$power.norm   <- if (is.null(fitdata$session$normalized_power)) {NA} else {fitdata$session$normalized_power}
    a$work         <- if (is.null(fitdata$session$total_work)) {NA} else {fitdata$session$total_work}
    a$stress.score <- if (is.null(fitdata$session$training_stress_score)) {NA} else {fitdata$session$training_stress_score}
    a$intensity.factor <- if (is.null(fitdata$session$intensity_factor)) {NA} else {fitdata$session$intensity_factor}
    a$training.effect <- if (is.null(fitdata$session$total_training_effect)) {NA} else {fitdata$session$total_training_effect}
  }
  return(a)
}

############################################################################################################
## WRAPPER FUNCTION 
## check for errors in file and activity
## process file
###
## input filename and ath.id
## output activity info
############################################################################################################

process.fitfile <- function(file, ath.id, cmr = FALSE) {
  # read file and create fitdata
  fitdata <- create_fitdata(file)
  ###################
  # check errors in file or before processing the activity
  # skip files with errors
  if (class(fitdata) == "try-error"){
    if (str_detect(file,"\\.pwx|\\.PWX")) {
      act.err <- onerow.df(c(ath.id,rep('file error (PWX format)',length(act.err.names)-2),file), act.err.names)
    } else if (str_detect(file,"\\.srm|\\.SRM")) {
      act.err <- onerow.df(c(ath.id,rep('file error (SRM format)',length(act.err.names)-2),file), act.err.names)
    } else {
      act.err <- onerow.df(c(ath.id,rep('file error (reading)',length(act.err.names)-2),file), act.err.names)
    }
    return(act.err)
  }
  # discard if there are no records or it's shorter than a minute
  if (is.null(dim(fitdata$record)) || dim(fitdata$record)[1] < 60) {
    act.err <- onerow.df(c(ath.id,rep('activity error (short/no data)',length(act.err.names)-2),file), act.err.names)
    return(act.err)
  }
  
  # discard if activity contains more than 1 row
  if(length(fitdata$session$sport) > 1 | length(fitdata$sport$sport) > 1) {
    act.err <- onerow.df(c(ath.id,rep('activity error (multiple sports/rows)',length(act.err.names)-2),file), act.err.names)
    return(act.err)
  }
  
  ###################
  # process activity
  ###################
  act <- try(get.act_info_from_fitdata(fitdata, ath.id, cmr = cmr), silent=T)
  # print(act) #debug
  ###################
  # check errors in activity
  # skip files with errors in processing activity
  if (class(act) == "try-error"){
    act.err <- onerow.df(c(ath.id,rep('activity error (unknown)',length(act.err.names)-2),file), act.err.names)
    return(act.err)
  }
  # act hr higher than maxhr
  if(!is.na(act$hrmax.activity) & act$hrmax.activity > act$hrmax_athlete) {
    act.err <- onerow.df(c(ath.id,rep('activity error (maxHR too high)',length(act.err.names)-2),file),act.err.names)
    return(act.err)
  }
  # timestamp missing
  if(is.na(act$date)) {
    act.err <- onerow.df(c(ath.id,rep('activity error (missing timestamp)',length(act.err.names)-2),file),act.err.names)
    return(act.err)
  }
  # wrong year (in the future), hard to find an auto fix and only 1 or 2 examples
  if (act$year > as.numeric(format(as.Date(Sys.Date()),"%Y"))) {
    act.err <- onerow.df(c(ath.id,rep('activity error (wrong year)',length(act.err.names)-2),file), act.err.names)
    return(act.err)
  }
  # average speed higher than 65km/h, as its probably an error activity while driving
  # ONLY IF NOT CMR (NO NEED AFTER 221028)
  if (cmr == FALSE) {
    if (!is.na(act$speed.avg) & act$speed.avg > 65) {
      act.err <- onerow.df(c(ath.id,rep('activity error (speed avg too high)',length(act.err.names)-2),file), act.err.names)
      return(act.err)
    }
  }
  # suunto watch (we have seen they are very unreliable)
  if (!is.na(act$device_brand_id) & act$device_brand_id == 23) {
    act.err <- onerow.df(c(ath.id, rep('activity error (suunto device, unreliable)', length(act.err.names)-2), file), act.err.names)
    return(act.err)
  }
  ###################
  # save activity
  ###################
  # update session to be counted and add the info to results df
  act.info <- cbind(ath.id, act, file=file)
  return(act.info)
}

############################################################################################################
############################################################################################################
############################################################################################################

