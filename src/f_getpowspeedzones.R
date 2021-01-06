############################################################################################################
############################################################################################################
###############################     FUNCTIONS FOR POWER / SPEED ZONES     ##################################
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
## MAIN FUNCTION 
## extract heart rate, power and speed from a file
###
## input filename and maxHR
## output dataframe with hr,power, speed and correlations in columns
############################################################################################################

get.hr_vs_all <- function(f,maxHR) {
  fitdata <- try(read.fit(f),silent=T)
  if (class(fitdata) == "try-error"){return(NULL)}
  # discard if its shorter than a minute
  if (is.null(dim(fitdata$record)) || dim(fitdata$record)[1] < 60) {return(NULL)}
  # save sport (to filter later)
  sport <- if (!is.null(fitdata$session$sport)){fitdata$session$sport}else{fitdata$sport$sport}
  # if heart rate is not present, skip file as there is no use
  if (!'heart_rate' %in% names(fitdata$record)){
    return(NULL)
  }
  # if there is no power/speed record, save hr but leave sport as NA to filter later
  if (is.null(fitdata$record$power) & is.null(fitdata$record$speed)){
    sport <- NA
  }
  # mark sport as NA if NA in heart_rate/power/speed > than 50% (but keep HR) (discard if HR==NA > 0.5)
  if (sum(fitdata$record$heart_rate == 0,na.rm=T) / length(fitdata$record$heart_rate) > 0.5){
    return(NULL)
  } else if (!is.null(fitdata$record$power)
             & sum(fitdata$record$power == 0,na.rm=T) / length(fitdata$record$power) > 0.5
             & !is.null(fitdata$record$speed) 
             & sum(fitdata$record$speed == 0,na.rm=T) / length(fitdata$record$speed) > 0.5){
    sport <- NA
  }

  # clean first 10 seconds of each activity
  fitdata$record$heart_rate[1:10] <- NA
  hrlength <- length(fitdata$record$heart_rate)
  fitdata$record$heart_rate[(hrlength-9):hrlength] <- NA
  # clean hr=0, as sometimes the sensor fails
  fitdata$record$heart_rate[fitdata$record$heart_rate == 0] <- NA
  # smooth hr and save
  hva.act <- setNames(data.frame(matrix(smooth.data(fitdata$record$heart_rate,10),ncol=1)),"hr")
  # same for power
  if (!is.null(fitdata$record$power)){
    fitdata$record$power[1:10] <- NA
    fitdata$record$power[(hrlength-9):hrlength] <- NA
    hva.act$power <- smooth.data(fitdata$record$power,100)
    hva.act$power[hva.act$power == "NaN"] <- NA
    # # remove all power = 0
    # hva.act <- hva.act[hva.act$power != "NaN",]
    # hva.act <- hva.act[hva.act$power !=0,]
  } else {
    hva.act$power <- NA
  }
  # same for speed
  if (!is.null(fitdata$record$speed)){
    fitdata$record$speed[1:10] <- NA
    fitdata$record$speed[(hrlength-9):hrlength] <- NA
    hva.act$speed <- smooth.data(3.79*fitdata$record$speed,20)
    hva.act$speed[hva.act$speed == "NaN"] <- NA
    # # remove all speed = 0
    # hva.act <- hva.act[hva.act$speed != "NaN",]
    # hva.act <- hva.act[hva.act$speed !=0,]
  } else {
    hva.act$speed <- NA
  }
  
  # remove all hr = NaN and return null if there are no rows
  hva.act <- hva.act[hva.act$hr != "NaN",]
  if (dim(hva.act)[1] == 0){
    return(NULL)
  }
  # check if max hr is higher than 10% maxHR for that athlete
  # if so, smooth a bit more to discard outliers
  if (max(hva.act$hr,na.rm=T) > maxHR*1.1){
    index.max <- which.max(hva.act$hr)
    i.start <- max(1,(index.max-100))
    i.stop <-min(length(hva.act$hr),(index.max+100))
    hva.act$hr[i.start:i.stop] <- smooth.data(hva.act$hr[i.start:i.stop],200)
  }
  # check how pow and hr are correlated and store r2 to filter later
  if (!is.na(sport) & !is.null(fitdata$record$power) & !all(is.na(hva.act$power))){
    knots <- quantile(hva.act$hr, p = c(0.5))
    lm_fit <- lm (power ~ bs(hr,knots=knots,degree = 3), data = hva.act)
    hva.act$power.cor <- round(summary(lm_fit)$r.squared ,2)
    hva.act$power.cor[hva.act$power.cor == "NaN"] <- NA
  } else {
    hva.act$power.cor <- NA
  }
  # check how speed and hr are correlated and store r2
  if (!is.na(sport) & !is.null(fitdata$record$speed) & !all(is.na(hva.act$speed))){
    lm_fit <- lm (speed ~ hr,data=hva.act)
    # correction to avoid negatively correlated regressions (e.g. cycling that are not correctly labeled)
    pos.neg.corr <- lm_fit$coefficients[2]/abs(lm_fit$coefficients[2])
    hva.act$speed.cor <- round(summary(lm_fit)$r.squared*pos.neg.corr,2)
  } else {
    hva.act$speed.cor <- NA
  }
  hva.act$sport <- sport
  hva.act$file <- last(str_split(f,"/")[[1]]) #debug
  return(hva.act)
}

############################################################################################################
## GET POWER ZONES 
## calculate power zones based on hrmax and hr/power correlation
## run spline regression with knot at 0.5 hr and 4 degrees of freedom
## calculate power intersection with the regression for the hr zone thresholds
###
## input data points (clean, no NA) and maxHR
## output dataframe with correlation and all power zone thresholds
############################################################################################################

get.power_zones <- function(data,hrmax) {
  knots <- quantile(data$hr, p = c(0.5))
  lm_fit <- lm (power ~ bs(hr,knots=knots,degree = 3), data = data)
  cor <- round(summary(lm_fit)$r.squared,2)
  hr.zones <- quantile(c(0:hrmax),probs=c(seq(0,1,by=0.1),0.83,0.94))
  pow.zones <- round(predict(lm_fit,newdata = data.frame(hr=hr.zones)),2)
  return(setNames(data.frame(cor,t(pow.zones)),c("cor",paste0('pow',seq(0,100,10)),paste0('vt.pow',c(83,94)))))
}

############################################################################################################
## GET SPEED ZONES 
## calculate speed zones based on hrmax and hr/speed correlation
## run LINEAR regression
## calculate power intersection with the regression for the hr zone thresholds
###
## input data points (clean, no NA) and maxHR
## output dataframe with correlation and all speed zone thresholds
############################################################################################################

get.speed_zones <- function(data,hrmax) {
  lm_fit <- lm (speed ~ hr, data = data)
  cor <- round(summary(lm_fit)$r.squared,2)
  hr.zones <- quantile(c(0:hrmax),probs=c(seq(0,1,by=0.1),0.83,0.94))
  speed.zones <- round(predict(lm_fit,newdata = data.frame(hr=hr.zones)),2)
  return(setNames(data.frame(cor,t(speed.zones)),c("cor",paste0('speed',seq(0,100,10)),paste0('vt.speed',c(83,94)))))
}

############################################################################################################
## GET MAXHR WITH TANGENT METHOD 
## use tangent method with all maxhr for all activities to calculate the actual maxHR
###
## input 
## output maxHR and plot saved in "png" folder
############################################################################################################

get.maxhr_tangent <- function(hr_vs_all,ath.code) {
  maxhr_per_activity <- hr_vs_all %>%
    rowwise() %>%
    mutate(file=last(str_split(file,"/")[[1]])) %>%
    ungroup() %>%
    group_by(file) %>%
    summarize(hrmax = max(hr))
  
  pdata = ggplot_build(ggplot(maxhr_per_activity) + stat_density(aes(x=hrmax),bw=3))$data[[1]]
  pdata$deriv = c(0,diff(pdata$y)/diff(pdata$x))
  # calc interc
  yinterc <- pdata$y[which.min(pdata$deriv)] - pdata$x[which.min(pdata$deriv)]*min(pdata$deriv)
  xinterc <- -yinterc/min(pdata$deriv)
  error_act <- sum(maxhr_per_activity$hrmax > xinterc)
  error_perc <- sum(maxhr_per_activity$hrmax > xinterc)/length(maxhr_per_activity$hrmax) * 100
  # save plot
  png(filename=paste0('png/maxHR-',ath.code,'.png'),width = 1440, height = 1440,res=300)
  p <- ggplot(pdata,aes(x,y)) +
    geom_line(cex=2) +
    geom_abline(slope=min(pdata$deriv),intercept=yinterc,color="red",cex=1.5) +
    geom_abline(slope=0,intercept=0) +
    geom_vline(xintercept = xinterc,col='blue',cex=0.5,linetype="dashed") +
    labs(x='HR (bpm)',y="",title=paste0("maxHR calculation for athlete ",ath.code)) +
    annotate("text",cex=3,
             label=paste0('maxHR=',round(xinterc,0)," bpm"),
             x=min(pdata$x)+3,y=max(pdata$y),col='blue',hjust=0)  +
    annotate("text", cex=3,
             label=paste0('n > maxHR=',error_act," (",round(error_perc,1),"%)"),
             x=min(pdata$x)+3,y=max(pdata$y)*0.95,col='blue',hjust=0) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank())
  plot(p)
  dev.off()
  hrmax <- round(xinterc,0)
  return(hrmax)
}

############################################################################################################
## GET ATHLETE INFO WITH ZONES 
## wrapper function for all the above
## for each athlete, run the main function and the power/speed zone calculations
## works as an update of ath.info input
###
## input ath.info data frame (name,ath_id,maxHR, at least), athlete name (same as in ath.info), maxHR
## output same ath.info with extra columns with all the zone thresholds
############################################################################################################

update.ath_info_with_newzones <- function(ath.info, athlete, maxHR) {
  sel.dirs <- list.dirs(all.dirs.PH[str_detect(all.dirs.PH,athlete)],recursive=F)
  sel.dirs <- sel.dirs[str_detect(sel.dirs,"YEAR")]
  files <- list.files(sel.dirs,pattern=".fit",full.names = TRUE)
  
  hr_vs_all <- data.frame(hr=character(), power=character(), speed=character(),
                     power.cor=character(), speed.cor=character(),sport=character(),
                     # maxhr=character(),
                     file=character(), #debug
                     stringsAsFactors=FALSE) 
  cores <- detectCores()
  cl <- makeCluster(cores[1]-1)
  registerDoParallel(cl)
  start <- Sys.time()
  hr_vs_all <- foreach (file=files,.combine=rbind,
                        .packages=c("dplyr", "fit","stringr","zoo","splines"),
                        .export=c("get.hr_vs_all","smooth.data")) %dopar% {
    temp <- get.hr_vs_all(file,maxHR)
    temp
  }
  rownames(hr_vs_all) <- NULL
  end <- Sys.time()
  duration <- end - start
  print(duration)
  stopCluster(cl)
  
  # save(hr_vs_all,file='kk.hr_vs_all.rda') # debug
  
  #### CALCULATE MAX HR WITH TANGENT METHOD
  ath.code <- ath.info$ath.id[ath.info$name == athlete]
  hrmax.athlete <- get.maxhr_tangent(hr_vs_all,ath.code)
  ath.info$maxHR[ath.info$name == athlete] <- hrmax.athlete
  
  # remove activities with hr higher than updated maxHR
  hr_vs_all <- hr_vs_all %>%
    group_by(file) %>%
    mutate(hrmax = max(hr)) %>% 
    ungroup() %>% 
    mutate(outlier = if_else(hrmax > hrmax.athlete,1,0)) %>% 
    filter(outlier == 0)
  
  # check maxHR (NOT ANYMORE, TANGENT WORKS!)
  # hrmax.athlete <- maxHR
  # if (round(max(hr_vs_all$hr),0) > hrmax.athlete){
  #   hrmax.athlete <- round(max(hr_vs_all$hr),0)
  #   ath.info$maxHR[ath.info$name == athlete] <- hrmax.athlete
  # }
  
  
  # get power zones
  # filter out activities that we do not want
  # sport == NA, power.cor < 0.2, power == NA
  hr_power <- hr_vs_all[hr_vs_all$power != "NaN" 
                        & hr_vs_all$power != 0 
                        & !is.na(hr_vs_all$power) 
                        & hr_vs_all$power.cor > 0.2
                        & hr_vs_all$sport == 2
                        & !is.na(hr_vs_all$sport),]
  if (dim(hr_power)[1] != 0){
    pow.zones <- get.power_zones(hr_power,hrmax.athlete)
    ath.info$pow.cor[ath.info$name == athlete] <- pow.zones$cor
    ath.info$pow.z1.thld[ath.info$name == athlete] <- pow.zones$pow50
    ath.info$pow.z2.thld[ath.info$name == athlete] <- pow.zones$pow60
    ath.info$pow.z3.thld[ath.info$name == athlete] <- pow.zones$pow70
    ath.info$pow.z4.thld[ath.info$name == athlete] <- pow.zones$pow80
    ath.info$pow.z5.thld[ath.info$name == athlete] <- pow.zones$pow90
    ath.info$pow.vt.z1.thld[ath.info$name == athlete] <- pow.zones$vt.pow83
    ath.info$pow.vt.z2.thld[ath.info$name == athlete] <- pow.zones$vt.pow94
  } else {
    ath.info$pow.cor[ath.info$name == athlete] <- NA
    ath.info$pow.z1.thld[ath.info$name == athlete] <- NA
    ath.info$pow.z2.thld[ath.info$name == athlete] <- NA
    ath.info$pow.z3.thld[ath.info$name == athlete] <- NA
    ath.info$pow.z4.thld[ath.info$name == athlete] <- NA
    ath.info$pow.z5.thld[ath.info$name == athlete] <- NA
    ath.info$pow.vt.z1.thld[ath.info$name == athlete] <- NA
    ath.info$pow.vt.z2.thld[ath.info$name == athlete] <- NA
  }
  # get speed zones
  # filter out activities that we do not want
  # sport != NA !=2 and 0 (undefined, but probably cycling), speed.cor < 0.2, speed == NA
  # sport only running (as some unknown == 0 might be altering the results, skiing)...
  hr_speed <- hr_vs_all[hr_vs_all$speed != "NaN" 
                        & hr_vs_all$speed != 0 
                        & !is.na(hr_vs_all$speed) 
                        & hr_vs_all$speed.cor > 0.2
                        & hr_vs_all$sport == 1
                        & !is.na(hr_vs_all$sport),]
  if (dim(hr_speed)[1] != 0){
    speed.zones <- get.speed_zones(hr_speed,hrmax.athlete)
    ath.info$speed.cor[ath.info$name == athlete] <- speed.zones$cor
    ath.info$speed.z1.thld[ath.info$name == athlete] <- speed.zones$speed50
    ath.info$speed.z2.thld[ath.info$name == athlete] <- speed.zones$speed60
    ath.info$speed.z3.thld[ath.info$name == athlete] <- speed.zones$speed70
    ath.info$speed.z4.thld[ath.info$name == athlete] <- speed.zones$speed80
    ath.info$speed.z5.thld[ath.info$name == athlete] <- speed.zones$speed90
    ath.info$speed.vt.z1.thld[ath.info$name == athlete] <- speed.zones$vt.speed83
    ath.info$speed.vt.z2.thld[ath.info$name == athlete] <- speed.zones$vt.speed94
  } else {
    ath.info$speed.cor[ath.info$name == athlete] <- NA
    ath.info$speed.z1.thld[ath.info$name == athlete] <- NA
    ath.info$speed.z2.thld[ath.info$name == athlete] <- NA
    ath.info$speed.z3.thld[ath.info$name == athlete] <- NA
    ath.info$speed.z4.thld[ath.info$name == athlete] <- NA
    ath.info$speed.z5.thld[ath.info$name == athlete] <- NA
    ath.info$speed.vt.z1.thld[ath.info$name == athlete] <- NA
    ath.info$speed.vt.z2.thld[ath.info$name == athlete] <- NA
  }
  return(ath.info)
}

############################################################################################################
############################################################################################################
############################################################################################################