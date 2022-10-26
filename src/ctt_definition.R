####################################
### EXTRA VARIABLE DEFINITION ######
####################################

########################
#### Sport type  #######
# Specify main sports with data (running, cycling, swimming, rowing)
## https://developer.garmin.com/downloads/connect-iq/monkey-c/doc/Toybox/ActivityRecording.html
## https://developer.garmin.com/connect-iq/api-docs/Toybox/ActivityRecording.html
########################

sport_code <- c(0,1,2,5,
                10,11,12,13,14,
                15,16,17)
names(sport_code) <- c("Undefined","Running","Cycling","Swimming",
                       "Cardio","Walking", "XCSki", "AlpineSki", "Snowboard",
                       "Rowing", "Mountaineering", "Hiking")

########################
#### column names for activity errors
########################
# act.err.names <- c("ath.id","date","year","month","week","start_time","duration.min","pause.min",
#                    "hrmax.activity","hrmax.athlete","hrmax.perc","hrmax.intensity",
#                    "hr.z1","hr.z2","hr.z3","hr.z4","hr.z5","hr.avg",
#                    "hr.z1.time","hr.z2.time","hr.z3.time","hr.z4.time","hr.z5.time","etrimp",
#                    "vt.z1","vt.z2","vt.z3","vt.z1.time","vt.z2.time","vt.z3.time","lutrimp",
#                    "sport_code","sport_type",
#                    "speed.avg","speed.avg.smooth","speed.max.session","speed.max.smooth",
#                    "cal","ascent","power.max","power.avg","power.norm","work","stress.score",
#                    "total.dist","intensity.factor","training.effect","file")
#act.err.names <- c("ath.id","date","year","month","week","start_time","duration.min","pause.min","total_dist.km",
#                   "cum_ascent.m","hr.sensor", "device_brand_id", "device_brand_name", "device_model_id", "device_model_name",
#                   "hrmax_athlete","hrmax.activity","hrmax.perc","hrmax.intensity","hr.avg",
#                   "hr.z01","hr.z2","hr.z3","hr.z4","hr.z5",
#                   "hr.z1.time","hr.z2.time","hr.z3.time","hr.z4.time","hr.z5.time",
#                   "vt.z1","vt.z2","vt.z3","vt.z1.time","vt.z2.time","vt.z3.time",
#                   "goldvt.z1","goldvt.z2","goldvt.z3",
#                   "goldvt.z1.time","goldvt.z2.time","goldvt.z3.time",
#                   "etrimp","lutrimp","lutrimp.goldvt",
#                   "etrimp.power","lutrimp.power","etrimp.speed","lutrimp.speed",
#                   "speed.avg","speed.max","sport_code","sport_type","cal","power.max","power.avg","power.norm",
#                   "work","stress.score","intensity.factor","training.effect","file")
act.err.names <- c("ath.id","date","year","month","week","start_time",
                   "duration.min","pause.min","total_dist.km","cum_ascent.m","hr.sensor",
                   "device_brand_id","device_brand_name","device_model_id","device_model_name","hrmax_athlete",
                   "hrmax.activity","hrmax.perc","hrmax.intensity","hr.avg","hr.z61",
                   "hr.z70","hr.z75","hr.z80","hr.z84","hr.z90",
                   "hr.z100","hr.total75","hr.total85","hr.z61.time","hr.z70.time",
                   "hr.z75.time","hr.z80.time","hr.z84.time","hr.z90.time","hr.z100.time",
                   "hr.total75.time","hr.total85.time","hr.z61.hhmmss","hr.z70.hhmmss","hr.z75.hhmmss",
                   "hr.z80.hhmmss","hr.z84.hhmmss","hr.z90.hhmmss","hr.z100.hhmmss","hr.total75.hhmmss",
                   "hr.total85.hhmmss","sport_code","sport_type", "file")

########################
#### get product and manufacturer
########################
product_id <- setNames(read.table("data/product_id.tsv", sep="\t", stringsAsFactors=F), c("brand", "model", "product_id"))
product_id$model <- tolower(product_id$model)
brand_id <- setNames(read.table("data/manufacturer_id.tsv", sep="\t", stringsAsFactors=F), c("brand", "brand_id"))

########################
#### brands that are chest-based even if they don't say so
########################
real_chest_brand <- c("srm", "zwift", "giant_manufacturing_co",
                      "bryton", "stages_cycling"
                      # "tacx", "sigmasport", "the_sufferfest", "wahoo_fitness",
                      )

real_chest_model <- c("edge", "Edge", "Polar V650", "Polar V800", "Polar M400", "Polar BEAT")
