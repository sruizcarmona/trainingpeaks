####################################
### EXTRA VARIABLE DEFINITION ######
####################################

########################
#### Sport type  #######
# Specify main sports with data (running, cycling, swimming, rowing)
## https://developer.garmin.com/downloads/connect-iq/monkey-c/doc/Toybox/ActivityRecording.html
## https://developer.garmin.com/connect-iq/api-docs/Toybox/ActivityRecording.html
########################

sport_code <- c(0,1,2,5,10,15)
names(sport_code) <- c("Undefined","Running","Cycling","Swimming","Cardio","Rowing")

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
act.err.names <- c("ath.id","date","year","month","week","start_time","duration.min","pause.min","total_dist.km",
                   "cum_ascent.m","hr.sensor", "device_brand_id", "device_brand_name", "device_model_id", "device_model_name",
                   "hrmax_athlete","hrmax.activity","hrmax.perc","hrmax.intensity","hr.avg",
                   "hr.z01","hr.z2","hr.z3","hr.z4","hr.z5",
                   "hr.z1.time","hr.z2.time","hr.z3.time","hr.z4.time","hr.z5.time",
                   "vt.z1","vt.z2","vt.z3","vt.z1.time","vt.z2.time","vt.z3.time",
                   "goldvt.z1","goldvt.z2","goldvt.z3",
                   "goldvt.z1.time","goldvt.z2.time","goldvt.z3.time",
                   "etrimp","lutrimp","lutrimp.goldvt",
                   "etrimp.power","lutrimp.power","etrimp.speed","lutrimp.speed",
                   "speed.avg","speed.max","sport_code","sport_type","cal","power.max","power.avg","power.norm",
                   "work","stress.score","intensity.factor","training.effect","file")

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

