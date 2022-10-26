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
## to get HHMMSS format of a time
## 
############################################################################################################
get.hhmmss <- function(minutes){
  h <- as.integer(minutes / 60)
  m <- as.integer((minutes / 60 - h) * 60)
  s <- round((minutes - m - h * 60) * 60, 0)
  hhmmss <- paste(str_pad(c(h,m,s),2,pad="0"), collapse = "")
  return(hhmmss)
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

get.act_info_from_fitdata <- function(fitdata, ath.id) {
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
  a$duration.hhmmss <- get.hhmmss(a$duration.min)
  a$pause.min <- if(is.null(fitdata$session$total_elapsed_time)){NA}else{round((fitdata$session$total_elapsed_time-fitdata$session$total_timer_time)/60,2)}
  a$total_dist.km <- if(is.null(fitdata$session$total_distance)) {round(last(fitdata$record$distance/1000,2))} else {round(fitdata$session$total_distance/1000,2)}
  ###################
  ## EXTRA STUFF
  ## extracted from fitfile session info
  a$sport_code   <- ifelse(!is.null(fitdata$session$sport),
                           fitdata$session$sport,
                           ifelse(!is.null(fitdata$sport$sport), 
                                  fitdata$sport$sport,
                                  NA))
  a$sport_type   <- ifelse(is.na(a$sport_code), NA, names(sport_code)[sport_code == a$sport_code])
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
    # hr.zones <- quantile(c(0:a$hrmax_athlete),probs=seq(0,1,by=0.1))
    hr.zones <- quantile(c(0:a$hrmax_athlete), probs = c(0, 0.61, 0.70, 0.75, 0.80, 0.84, 0.90, 1)) # FOR KRISTEL
    fd$hr.zones <- findInterval(smooth.hr,hr.zones[2:8])
    hr.zones.table <- round(table(fd$hr.zones)/length(fd$hr.zones)*100,2)
    hr.zones.table[c("0","1","2","3","4","5","6")[!c("0","1","2","3","4","5","6") %in% names(hr.zones.table)]] <- 0
    a$hr.z61 <- as.numeric(hr.zones.table['0'])
    a$hr.z70 <- as.numeric(hr.zones.table['1'])
    a$hr.z75 <- as.numeric(hr.zones.table['2'])
    a$hr.z80 <- as.numeric(hr.zones.table['3'])
    a$hr.z84 <- as.numeric(hr.zones.table['4'])
    a$hr.z90 <- as.numeric(hr.zones.table['5'])
    a$hr.z100 <- as.numeric(hr.zones.table['6'])
    a$hr.total75 <- a$hr.z61 + a$hr.z70 + a$hr.z75
    a$hr.total85 <- a$hr.z90 + a$hr.z100
    a$hr.z61.time <- as.numeric(round(hr.zones.table['0']/100 * a$duration.min,2))
    a$hr.z70.time <- as.numeric(round(hr.zones.table['1']/100 * a$duration.min,2))
    a$hr.z75.time <- as.numeric(round(hr.zones.table['2']/100 * a$duration.min,2))
    a$hr.z80.time <- as.numeric(round(hr.zones.table['3']/100 * a$duration.min,2))
    a$hr.z84.time <- as.numeric(round(hr.zones.table['4']/100 * a$duration.min,2))
    a$hr.z90.time <- as.numeric(round(hr.zones.table['5']/100 * a$duration.min,2))
    a$hr.z100.time <- as.numeric(round(hr.zones.table['6']/100 * a$duration.min,2))
    a$hr.total75.time <- as.numeric(round(a$hr.total75/100 * a$duration.min,2))
    a$hr.total85.time <- as.numeric(round(a$hr.total85/100 * a$duration.min,2))
    a$hr.z61.hhmmss <- get.hhmmss(a$hr.z61.time)
    a$hr.z70.hhmmss <- get.hhmmss(a$hr.z70.time)
    a$hr.z75.hhmmss <- get.hhmmss(a$hr.z75.time)
    a$hr.z80.hhmmss <- get.hhmmss(a$hr.z80.time)
    a$hr.z84.hhmmss <- get.hhmmss(a$hr.z84.time)
    a$hr.z90.hhmmss <- get.hhmmss(a$hr.z90.time)
    a$hr.z100.hhmmss <- get.hhmmss(a$hr.z100.time)
    a$hr.total75.hhmmss <- get.hhmmss(a$hr.total75.time)
    a$hr.total85.hhmmss <- get.hhmmss(a$hr.total85.time)
  } else {
    # add same columns with NA
    a <- cbind(a, onerow.df(NA,colnames=c("hrmax.activity","hrmax.perc","hrmax.intensity","hr.avg",
                                          "hr.z61","hr.z70","hr.z75","hr.z80","hr.z84","hr.z90","hr.z100",
                                          "hr.total75", "hr.total85",
                                          "hr.z61.time","hr.z70.time","hr.z75.time","hr.z80.time","hr.z84.time","hr.z90.time","hr.z100.time",
                                          "hr.total75.time", "hr.total85.time",
                                          "hr.z61.hhmmss","hr.z70.hhmmss","hr.z75.hhmmss","hr.z80.hhmmss","hr.z84.hhmmss","hr.z90.hhmmss","hr.z100.hhmmss",
                                          "hr.total75.hhmmss", "hr.total85.hhmmss")))
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

process.fitfile <- function(file,ath.id) {
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
  act <- try(get.act_info_from_fitdata(fitdata, ath.id), silent=T)
  # print(act) #debug
  ###################
  # check errors in activity
  # skip files with errors in processing activity
  if (class(act) == "try-error"){
    act.err <- onerow.df(c(ath.id,rep('activity error (unknown)',length(act.err.names)-2),file), act.err.names)
    return(act.err)
  }
  # act hr higher than maxhr
  # if(!is.na(act$hrmax.activity) & act$hrmax.activity > act$hrmax_athlete) {
  #   act.err <- onerow.df(c(ath.id,rep('activity error (maxHR too high)',length(act.err.names)-2),file),act.err.names)
  #   return(act.err)
  # }
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

