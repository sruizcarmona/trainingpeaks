
library(xml2)

# from trackeR
# https://github.com/trackerproject/trackeR/blob/7834a6ef6b91f6844acb4b87b52490713baecd05/R/read.R

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
  observations[!is_time] <- apply(observations[!is_time], 2, as.numeric)
  
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
  
  attr(observations, "sport") <- sport
  
  return(observations)
  
}
