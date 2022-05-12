############################################################################################################
############################################################################################################
#################################       FUNCTIONS FOR MAIN SECTION       ###################################
############################################################################################################
############################################################################################################

############################################################################################################
## LOAD LIBRARIES
############################################################################################################

library(geosphere)
library(XML)
library(tidyr)
library(xml2)

############################################################################################################
## readTCX
## from trackeR
## https://github.com/trackerproject/trackeR/blob/7834a6ef6b91f6844acb4b87b52490713baecd05/R/read.R
############################################################################################################

readTCX <- function(file, timezone = "", speedunit = "m_per_s", distanceunit = "m",
                    parallel = FALSE, cores = getOption("mc.cores", 2L),...) {
  
  doc <- read_xml(file)
  ns <- xml_ns(doc)
  
  children_names <- function(x, xpath, ns) {
    unique(xml_name(xml_children(xml_find_all(x, xpath, ns))))
  }
  
  ## Core namespaces
  activity_ns <- names(which(ns == "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")[1])
  extensions_ns <- names(which(ns == "http://www.garmin.com/xmlschemas/ActivityExtension/v2")[1])

  ## Sport
  sport <- xml_attr(xml_find_all(doc, paste0("//", activity_ns, ":", "Activity")), "Sport")
  # some files report it as "biking"
  sport <- ifelse(sport == "Biking", "Cycling", sport)
  
  ## Tp
  tp_xpath <- paste0("//", activity_ns, ":", "Trackpoint")
  tp_vars <- data.frame(name = children_names(doc, tp_xpath, ns),
                        ns = activity_ns)
  
  ## Position
  position_xpath <- paste0("//", activity_ns, ":", "Position")
  ## Add any nested fields here
  is_position <- tp_vars$name == "Position"
  if (any(is_position)) {
    ## remove position
    tp_vars <- tp_vars[!is_position, ]
    ## Add longitude/latitude
    children <- data.frame(name = children_names(doc, position_xpath, ns[activity_ns]),
                           ns = activity_ns)
    tp_vars <- rbind(tp_vars, children)
  }
  
  ## Extensions
  extensions_xpath <- paste0("//", extensions_ns, ":", "TPX")
  is_extensions <- tp_vars$name == "Extensions"
  if (any(is_extensions)) {
    ## remove position
    tp_vars <- tp_vars[!is_extensions, ]
    ## Add any extensions
    children <- data.frame(name = children_names(doc, extensions_xpath, ns[extensions_ns]),
                           ns = extensions_ns)
    tp_vars <- rbind(tp_vars, children)
  }
  
  is_time <- tp_vars$name == "Time"
  is_sensorstate <- tp_vars$name == "SensorState"
  
  tps <- xml_find_all(doc, tp_xpath, ns[activity_ns])
  ## Double loop to extract obs
  observations <- apply(tp_vars, 1, function(var) {
    c_xpath <- paste0(".", "//", var["ns"], ":", var["name"])
    c_ns <- ns[var["ns"]]
    sapply(tps, function(x) {
      xml_text(xml_find_first(x, c_xpath, c_ns))
    })
  })
  
  observations <- as.data.frame(observations, stringsAsFactors = FALSE)
  ## Rename RunCadence to Cadence
  
  names(observations) <- tp_vars$name
  run_cadence <- tp_vars$name == "RunCadence"
  if (any(run_cadence)) {
    names(observations)[run_cadence] <- "Cadence"
  }
  observations[!(is_time | is_sensorstate)] <- apply(observations[!(is_time | is_sensorstate)], 2, as.numeric)
  
  ## convert speed from speedunit to m/s
  if (speedunit != "m_per_s") {
    speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
    observations$speed <- speedConversion(observations$speed)
  }
  
  ## convert distance from distanceunit to m
  if (distanceunit != "m") {
    distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
    observations$distance <- distanceConversion(observations$distance)
  }
  
  #### src test for device
  device_brand_id <- if(str_detect(xml_text(doc), "Polar")) {device_brand_id <- 123} else 
    if(str_detect(xml_text(doc), "Suunto")) {device_brand_id <- 23} else 
      {device_brand_id <- 0}
  device_info_xml <- doc %>% xml_ns_strip() %>% 
    xml_find_all(".//Activity")  %>% 
    xml_children() %>% 
    xml_find_all("//Creator") %>% 
    xml_children() %>% 
    xml_find_all("//Name") %>% xml_text()
  info_xml_patterns <- c("Hardlopen", "Kajakken", "Fietsen", "Overig",
                         "Mountainbiken", "Track & field", "Indoor cycling",
                         "Kayaking", "Other", "Road running", "Running",
                         "Road cycling", "Core training", "Cycling", "Zwemmen",
                         "Open water", "Zwembad", "Triathlon", "Wandelen")
  device_model_name <- device_info_xml[!str_detect(device_info_xml, paste(info_xml_patterns, collapse="|"))][1]
  # fix device brand if garmin
  device_brand_id <- ifelse(str_detect(device_model_name, "Garmin"), 1, device_brand_id)
  ###
  
  attr(observations, "sport") <- sport
  attr(observations, "device_brand_id") <- device_brand_id
  attr(observations, "device_model_name") <- device_model_name
  
  return(observations)
}

############################################################################################################
## MAIN FUNCTION 
## 
## TCX contains less info than fit, and it's organized differently, so we need to recreate the same as if it came from a fit file
## minimum:
## - session: with date, duration, ascent, total dist
## - record: with speed and hr, et al.
## - sport? unknown --> estimate from average speed, filename (!)
###
## input filename 
## output fitdata, same format as from read.fit
############################################################################################################

create.fitdata_from_tcx <- function(tcxfile) {
  # read tcx file (returns df already)
  tcxdata <- readTCX(tcxfile)
  # if there is no altitude, or lat or lon, just add it as 0
  if(!"AltitudeMeters" %in% names(tcxdata)) {
    tcxdata$AltitudeMeters <- 0
  }
  if(!"LatitudeDegrees" %in% names(tcxdata)) {
    tcxdata$LatitudeDegrees <- 0
  }
  if(!"LongitudeDegrees" %in% names(tcxdata)) {
    tcxdata$LongitudeDegrees <- 0
  }
  # manipulate tcxdata to create needed fields, as in fitdata$record
  tcx <- tcxdata %>%
    plyr::rename(replace=c(AltitudeMeters="altitude",
                           HeartRateBpm="heart_rate",
                           Time="time",
                           DistanceMeters="distance",
                           Cadence="cadence",
                           LatitudeDegrees="lat",
                           LongitudeDegrees="lon",
                           Watts="power",
                           SensorState = "sensor"
                           ),
                 warn_missing=FALSE) %>%
    select(time, one_of("lat", "lon", "altitude", "power", "sensor", "distance", "heart_rate")) %>%
    mutate_at(if('heart_rate' %in% names(.)) 'heart_rate' else integer(0), as.numeric) %>%
    mutate(altitude = as.numeric(altitude),
           ascent = altitude-lag(altitude),
           ascent = cumsum(replace_na((ascent+abs(ascent))/2,0)),
           # date=format(as.POSIXct(time,format=dateformat,tz="UTC")),
           # day=format(as.POSIXct(date),"%y%m%d"),
           # timeofday=format(as.POSIXct(date),"%H:%M:%S"),
           # time=as.POSIXct(time,format=dateformat,tz="UTC"),
           timestamp = as.numeric(as.POSIXct(time,format="%Y-%m-%dT%H:%M:%SZ",tz="UTC"))-631065600,
           timestamp_corr = as.numeric(as.POSIXct(time,format="%Y-%m-%dT%H:%M:%S.000Z",tz="UTC"))-631065600,
           time2 = str_remove(time, "\\..*"),
           timestamp2 = as.numeric(as.POSIXct(time2,format="%Y-%m-%dT%H:%M:%S",tz="UTC"))-631065600,
           timestamp = coalesce(timestamp, timestamp_corr, timestamp2),
           laglon=lag(lon),
           laglat=lag(lat)) %>%
    rowwise() %>%
    mutate(step_distance=distm(c(lon,lat),c(laglon,laglat),fun=distHaversine)) %>%
    ungroup() %>%
    mutate(step_distance=as.numeric(step_distance),
           # calculate distance, as we avoid errors when does not come from tcx file. very similar results to tcx distance
           distance=round(cumsum(replace_na(step_distance,0)),2),
           step_s=as.numeric(timestamp-lag(timestamp)),
           # cum_s=cumsum(replace_na(step_s,0)),
           # cum_min=round(cum_s/60,2),
           speed=replace_na((step_distance/1000)/(step_s/3600),0),
           speed=speed/3.79, # same 3.79 conversion factor as fit files
           speed=smooth.data(speed,5)
           ) %>%
    select(-step_s,-laglon,-laglat,-step_distance,-time, -timestamp_corr, -time2, -timestamp2)
  # create session with same fields as in fitdata$session
  session <- data.frame(start_time=0)
  session$avg_altitude <- round(mean(tcx$altitude),1)
  session$start_time <- tcx$timestamp[1]
  session$total_timer_time <- last(tcx$timestamp) - tcx$timestamp[1]
  session$total_elapsed_time <- session$total_timer_time
  session$total_distance <- last(tcx$distance)
  session$total_ascent <- last(tcx$ascent)
  # readTCX exports sport info as an attribute
  session$sport <- sum(sport_code[attributes(tcxdata)$sport])
  session$device_brand_id <- attributes(tcxdata)$device_brand_id
  session$device_model_name <- attributes(tcxdata)$device_model_name
  # combine both into fitdata
  fitdata <- NULL
  fitdata$session <- session
  fitdata$record <- tcx
  # fitdata
  return(fitdata)
}
############################################################################################################
############################################################################################################
############################################################################################################

