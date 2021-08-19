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

############################################################################################################
## readGPX
## use XML library to read and parse a gpx file (from surfingwaves)
############################################################################################################
readGPX <- function(
  gpx.file,
  metadata = TRUE,
  bounds = TRUE,
  waypoints = TRUE, 
  tracks = TRUE,  
  routes = TRUE   
)
  
{    
  opt <- options(warn=-1)    
  if(!file.exists(gpx.file)) stop("The file '", gpx.file, "'\n  does not exist in ", getwd() )
  
  if(metadata==TRUE) { metadata <- .readGPX.element(gpx.file, "name") }    
  if(bounds==TRUE) { bounds <- .readGPX.element(gpx.file, "bounds") }    
  if(waypoints==TRUE) { waypoints <- .readGPX.element(gpx.file, "wpt") }
  if(tracks==TRUE) { tracks <- .readGPX.element(gpx.file, "trk") }
  if(routes==TRUE) { routes <- .readGPX.element(gpx.file, "rte") }
  
  gpx <- list(metadata=metadata, bounds=bounds, waypoints=waypoints, tracks=tracks, routes=routes)
  return(gpx)
  on.exit(options(opt))
}

## Read various elements from a *.gpx file:

.readGPX.element <- function(gpx.file, element) {
  # element = "metadata", "wpt", "rte", "trk"
  
  ret <- xmlTreeParse(gpx.file, useInternalNodes = TRUE)
  # top structure: 
  top <- xmlRoot(ret)
  
  # check if there is any content:
  if(any(grep(element, names(top)))) {
    
    # tracks:
    if(element=="trk"){   
      ret <- NULL
      nu <- which(names(top) %in% element)
      for(c in seq_along(nu)){
        lst <- which(names(top[[nu[c]]]) %in% "trkseg")
        nm <- names(top[[nu[c]]][[lst[1]]][[1]])
        ret[[c]] <- list(NULL)
        for(i in seq_along(lst)) {
          trkpt <- top[[nu[c]]][[lst[i]]]
          ret[[c]][[i]] <- data.frame(NULL)
          ## get columns (http://www.topografix.com/GPX/1/1/#type_wptType)
          lon <- as.numeric(xmlSApply(trkpt, xmlGetAttr, "lon"))
          lat <- as.numeric(xmlSApply(trkpt, xmlGetAttr, "lat"))
          ret[[c]][[i]][1:length(lon),"lon"] <- lon
          ret[[c]][[i]][1:length(lat),"lat"] <- lat
          if(!nm[[1]]=="NULL"){
            for(j in 1:length(nm)){
              xm <- as.character(sapply(sapply(xmlChildren(trkpt), function(x) x[[nm[[j]]]]), xmlValue))
              ret[[c]][[i]][1:length(xm), nm[[j]]] <- xm 
            }
          } 
        }
        names(ret[[c]]) <- xmlValue(top[[nu[c]]][["name"]])
      }   
    }
    
    if(element=="wpt"){
      ret <- data.frame(NULL)
      nu <- which(names(top) %in% element)
      nm <- names(top[[nu[1]]])
      for(i in seq_along(nu)) {
        # coordinates:
        ret[i, "lon"] <- as.numeric(xmlGetAttr(top[[nu[i]]], "lon"))
        ret[i, "lat"] <- as.numeric(xmlGetAttr(top[[nu[i]]], "lat"))
        if(!nm[[1]]=="NULL"){
          for(j in 1:length(nm)){
            ret[i, nm[[j]]] <- xmlValue(xmlChildren(top[[nu[i]]])[[nm[[j]]]])
          }  
        }
      }
    }
    
    if(element=="rte"){
      ret <- NULL
      nu <- which(names(top) %in% element)
      for(c in seq_along(nu)){
        ret[[c]] <- data.frame(NULL)
        lst <- which(names(top[[nu[c]]]) %in% "rtept")
        nm <- names(top[[nu[c]]][[lst[1]]])
        for(i in seq_along(lst)) {
          rtept <- top[[nu[c]]][[lst[i]]]
          ret[[c]][i, "lon"] <- as.numeric(xmlGetAttr(rtept, "lon"))
          ret[[c]][i, "lat"] <- as.numeric(xmlGetAttr(rtept, "lat"))
          if(!nm[[1]]=="NULL"){
            for(j in c("name","cmt","desc","sym","type")){
              try(ret[[c]][i, j] <- xmlValue(rtept[[j]]), silent = TRUE)
            }
          } 
        }
        names(ret)[c] <- xmlValue(top[[nu[c]]][["name"]])
      }
    }
    
    # bounds
    if(element=="bounds"){
      nu <- which(names(top) %in% element)
      ret <- matrix(rep(NA, 4), nrow=2, dimnames = list(c("lat", "lon"), c("min", "max")))
      # coordinates:
      ret[1,1] <- as.numeric(xmlGetAttr(top[[nu[1]]], "minlon"))
      ret[1,2] <- as.numeric(xmlGetAttr(top[[nu[1]]], "maxlon"))
      ret[2,1] <- as.numeric(xmlGetAttr(top[[nu[1]]], "minlat"))
      ret[2,2] <- as.numeric(xmlGetAttr(top[[nu[1]]], "maxlat"))
    }
    
    # metadata
    if(element=="name"){
      lst <- c("name","desc","author","email","url","urlname","time")
      nu <- which(names(top) %in% lst)
      if(!nu[[1]]=="NULL"){      
        ret <- data.frame(NULL)
        for(i in seq_along(lst)) {
          try(ret[1,lst[i]] <- xmlValue(top[[nu[[i]]]]), silent = TRUE)
        }
      }
    }
    
  }
  else { ret <- NULL }
  
  return(ret)
}

############################################################################################################
## MAIN FUNCTION 
## 
## gpx contains much less info than fit, so we need to recreate the same as if it came from a fit file
## minimum:
## - session: with date, duration, ascent, total dist
## - record: with speed and hr, et al.
## - sport? unknown --> estimate from average speed, filename (!)
###
## input filename 
## output fitdata, same format as from read.fit
############################################################################################################

create.fitdata_from_gpx <- function(gpxfile) {
  # read gpx file and get tracks df
  gpxdata.raw <- readGPX(gpxfile)
  gpxdata <- gpxdata.raw$tracks[[1]][[1]]
  # manipulate gpxdata to create needed fields, as in fitdata$record
  gpx <- gpxdata %>%
    plyr::rename(replace=c(ele="altitude",
                           extensions="heart_rate"),
                 warn_missing=FALSE) %>% 
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
           time2 = str_remove(time, "\\...."),
           timestamp2 = as.numeric(as.POSIXct(time2,format="%Y-%m-%dT%H:%M:%SZ",tz="UTC"))-631065600,
           timestamp = coalesce(timestamp, timestamp_corr, timestamp2),
           laglon=lag(lon),
           laglat=lag(lat)) %>% 
    rowwise() %>%
    mutate(step_distance=distm(c(lon,lat),c(laglon,laglat),fun=distHaversine)) %>% 
    ungroup() %>% 
    mutate(step_distance=as.numeric(step_distance),
           distance=round(cumsum(replace_na(step_distance,0)),2),
           step_s=as.numeric(timestamp-lag(timestamp)),
           # cum_s=cumsum(replace_na(step_s,0)),
           # cum_min=round(cum_s/60,2),
           speed=replace_na((step_distance/1000)/(step_s/3600),0),
           speed=speed/3.79, # same 3.79 conversion factor as fit files
           speed=smooth.data(speed,5)) %>% 
    select(-step_s,-laglon,-laglat,-step_distance,-time, -timestamp_corr, -time2, -timestamp2)
  # create session with same fields as in fitdata$session
  session <- data.frame(start_time=0)
  session$avg_altitude <- round(mean(gpx$altitude),1)
  session$start_time <- gpx$timestamp[1]
  session$total_timer_time <- last(gpx$timestamp) - gpx$timestamp[1]
  session$total_elapsed_time <- session$total_timer_time
  session$total_distance <- last(gpx$distance)
  session$total_ascent <- last(gpx$ascent)
  session$sport <- case_when(
    str_detect(tolower(last(strsplit(gpxfile,"/")[[1]])),"run") ~ 1,
    str_detect(tolower(last(strsplit(gpxfile,"/")[[1]])),"ride") ~ 2,
    TRUE ~ 0)
  # combine both into fitdata
  fitdata <- NULL
  fitdata$session <- session
  fitdata$record <- gpx
  # fitdata
  return(fitdata)
}
############################################################################################################
############################################################################################################
############################################################################################################

