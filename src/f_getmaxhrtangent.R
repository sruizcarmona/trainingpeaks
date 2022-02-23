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
library(ggplot2)
library(scales)

############################################################################################################
## PROCESS MAXHR vs ACTIVITY
## use tangent method with all maxhr for all activities to calculate the actual maxHR
###
## input  myact and the different weights (currently optimized by brute force)
## output pdata for plotting below
############################################################################################################

process_maxhr_activity <- function(myact, w_e, w_d, w_x, w_h, nlag = 10, d2f = 0.1) {
  myres <- ggplot_build(ggplot(myact) + stat_density(aes(x=hrmax),bw=3))$data[[1]] %>% 
    select(x,y) %>% 
    mutate(deriv = (y - lag(y))/ (x - lag(x)),
           deriv = smooth.data(deriv, 20, r=1000),
           # add second derivative as a filter to avoid min/max and focus on slopes (removing all on the bottom and top 10%)
           deriv2 = (deriv - lag(deriv)) / (x - lag(x)),
           score_deriv2 = ifelse(deriv2 < quantile(deriv2, na.rm=T, d2f) | deriv2 > quantile(deriv2, na.rm=T, 1-d2f), 0, 1),
           yinterc = y - x*deriv,
           xinterc = round(-yinterc/deriv,0),
           xinterc = ifelse(is.na(xinterc) | !is.finite(xinterc),999,xinterc), # fix errors in xinterc exactly 0
           xinterc_diff = xinterc - lag(xinterc, default = 0, n = nlag)) %>% 
    rowwise() %>% 
    mutate(error_act = sum(myact$hrmax > xinterc),
           error_perc = sum(myact$hrmax > xinterc)/length(myact$hrmax) * 100,
           error_perc5 = ifelse(error_perc > 5, error_perc, 0)
    ) %>% 
    ungroup() %>% 
    mutate(rank_error = dense_rank(error_act * error_perc), 
           # better to have a scaled score instead of a ranked normalization
           # substract 5% to error_perc so we don't mind if there is 5% error --> no difference
           score_error = scale(error_act * (error_perc5)) + abs(min(scale(error_act * (error_perc5)), na.rm=T)),
           score_error_perc = scale(error_perc5) + abs(min(scale(error_perc5), na.rm=T)),
           rank_deriv = dense_rank(desc(deriv)),
           # same with derivative, no point on ranking, better to leave it scaled as it is
           score_deriv = scale(-deriv),
           # score_deriv2 = scale(deriv2),
           # the "xintercdiff" is a bit tricky, as I implemented it to avoid correcting high error rates by going to flat derivative
           # this way, if the section of the curve is a highly variable one, such as maximums or minimums, it gets penalized
           # I will also score is as a scale
           rank_xintercdiff = dense_rank(abs(xinterc_diff)),
           score_xintercdiff = ifelse(abs(xinterc_diff) < 3,
                                      scale(abs(xinterc_diff)) + abs(min(scale(abs(xinterc_diff)), na.rm=T)),
                                      abs(xinterc_diff)))
  # NEW THING, after the nice optimization, some athletes were prioritizing the late and small peaks
  # to correct for that, I will create a "highness" measure, so higher peaks are prioritized
  # first, find local maxs
  loc_max <- myres %>% filter(deriv * lag(deriv) < 0 & deriv2 < 0) %>% 
    mutate(y = ifelse(x < 140, y[y!=max(y)]*1.1,y)) # correct for way too big peaks before 140... artifacts
  loc_inflex <- myres %>% filter(deriv2 * lag(deriv2) < 0 | row_number() == n())
  # now, create a new column with highness value
  myres <- myres %>% 
    rowwise() %>% 
    mutate(highness = loc_max$y[last(which(loc_max$x < x))],
           score_error = ifelse(length(myact$hrmax) < 100,
                                score_error / 15,
                                ifelse(length(myact$hrmax) < 200,
                                       score_error / 5,
                                       score_error)),
           # add inflexion points (some plots with inflex points at the end are giving errors)
           score_deriv2 = ifelse(any(x < (loc_inflex$x + 1) & x > (loc_inflex$x - 1)),
                                 1, score_deriv2)
    ) %>% 
    ungroup() %>% 
    mutate(highness = scale(highness**3) + abs(min(scale(highness**3), na.rm=T)),
           score_highness = case_when(is.na(highness) ~ 0,
                                      highness == 0 ~ 0.1,
                                      TRUE ~ highness / 5),
           optim = (-score_error * w_e + score_deriv * w_d - score_xintercdiff * w_x + score_highness * w_h),
           optim = ifelse(xinterc > 230, NA, optim), # fix for way too high xinterc
           optim = ifelse(x < 140 & xinterc > 200, NA, optim), # fix for wrongly found x, before 140
           optim = ifelse(deriv > 0, NA, optim), # to remove "going up" slopes
           optim = ifelse(score_deriv2 == 0, NA, optim), # last one, to apply score_deriv filter
    )
  
  return(myres)
}


############################################################################################################
## GET MAXHR WITH TANGENT METHOD 
## use tangent method with all maxhr for all activities to calculate the actual maxHR
###
## input 
## output maxHR and plot saved in "png" folder
############################################################################################################

get.maxhr_tangent <- function(hr_vs_all,ath.code) {
  if (is.null(hr_vs_all)){
    return (0)
  }
  maxhr_per_activity <- hr_vs_all %>%
    # rowwise() %>%
    # mutate(file=last(str_split(file,"/")[[1]])) %>%
    # ungroup() %>%
    group_by(file) %>%
    summarise(hrmax = max(hr),
              sport=first(sport_plot),
              device_brand = ifelse(first(device_brand_id) %in% brand_id$brand_id,
                                    brand_id$brand[brand_id$brand_id == first(device_brand_id)],
                                    "unknown"),
              device_model = ifelse(first(device_product_id) %in% product_id$product_id[product_id$brand == device_brand],
                                    product_id %>% filter(brand == device_brand & product_id == first(device_product_id)) %>%           pull(model),
                                    "unknown"),
              hr.sensor=first(hr.sensor),
              .groups="drop")
  maxhr_per_activity.140 <- maxhr_per_activity %>% 
    filter(hrmax > 1) # 11FEB meeting addition (down again from 140 to 1 on FEB22, as it is wrong)

  sourcedata <- maxhr_per_activity %>% 
    filter(hrmax > 1) %>% 
    mutate(hr.sensor = ifelse(device_brand %in% real_chest_brand, TRUE, hr.sensor)) %>% 
    mutate(hr.sensor = ifelse(grepl("edge_", device_model), TRUE, hr.sensor)) %>% 
    filter(hr.sensor == TRUE)
  wrist_removed <- TRUE
  
  # if wrist is more than 70%, do not remove them
  if((dim(sourcedata)[1] / dim(maxhr_per_activity)[1]) < 0.3 | dim(sourcedata)[1] < 10){
    sourcedata <- maxhr_per_activity %>% 
      filter(hrmax > 1) %>% 
      mutate(hr.sensor = ifelse(device_brand %in% real_chest_brand, TRUE, hr.sensor)) %>% 
      mutate(hr.sensor = ifelse(grepl("edge_", device_model), TRUE, hr.sensor))
    wrist_removed <- FALSE
  }
  
  # weights obtained in "optim" folder with an optimization of 150 individuals with a grid search approach
  pdata <- process_maxhr_activity(sourcedata, w_e = 80, w_d = 1, w_x = 4, w_h = 0, nlag = 10, d2f = 0.2)

  best_row <- pdata %>% arrange(-optim) %>% filter(row_number() == 1)
  yinterc <- best_row$yinterc
  xinterc <- best_row$xinterc
  tan_slope <- best_row$deriv
  error_act <- sum(sourcedata$hrmax > xinterc)
  error_perc <- sum(sourcedata$hrmax > xinterc) / length(sourcedata$hrmax) * 100
  
  if(dim(sourcedata)[1] < 10 | best_row$x < 150) {mylim_x <- 89} else {mylim_x <- 139}
  
  # add chest and device info
  chest_perc <- sum(sourcedata$hr.sensor == TRUE) / length(sourcedata$hr.sensor) * 100
  dev_info <- (table(paste(sourcedata$device_brand, sourcedata$device_model)) / length(sourcedata$device_brand) * 100) %>%
    sort(decreasing = T)
  # colors for boxes
  box_sum_col <- 'white'
  box_sum_text <- 'red'
  if(wrist_removed == F){
    box_sum_col <- 'red'
    box_sum_text <- 'white'
  }
  
  # save plot
  png(filename=paste0('out/png/maxHR-',ath.code,'.png'),width = 1440, height = 1440,res=300)
  p <- ggplot(pdata,aes(x,y)) +
    geom_line(cex=1.2) +
    geom_abline(slope=tan_slope,intercept=yinterc,color="red",cex=0.8) +
    geom_abline(slope=0,intercept=0) +
    geom_vline(xintercept = xinterc,col='gray50',cex=0.5,linetype="dashed") +
    geom_point(data=sourcedata %>% filter(hrmax > mylim_x + 1),
               aes(x=hrmax,y=max(pdata$y,na.rm=T)/100 * -2), cex=1,alpha=0.15,shape=16,col="black") +
    annotate("text",cex=1.5, label="all",
             x=mylim_x, y=max(pdata$y,na.rm=T)/100 * -2,col='black',hjust=1)  +
    {if(dim(sourcedata %>% filter(sport == 1) %>% filter(hrmax > mylim_x + 1))[1] > 0) {
      geom_point(data=sourcedata %>% filter(sport == 1) %>% filter(hrmax > mylim_x + 1),
                 aes(x=hrmax,y=max(pdata$y,na.rm=T)/100 * -4), cex=1,alpha=0.15,shape=16,col="gray50")}} +
    annotate("text",cex=1.2, label="run",
             x=mylim_x, y=max(pdata$y,na.rm=T)/100 * -4,col='black',hjust=1)  +
    {if(dim(sourcedata %>% filter(sport == 2) %>% filter(hrmax > mylim_x + 1))[1] > 0) {
      geom_point(data=sourcedata %>% filter(sport == 2) %>% filter(hrmax > mylim_x + 1),
                 aes(x=hrmax,y=max(pdata$y,na.rm=T)/100 * -5.5), cex=1,alpha=0.15,shape=16,col="gray50")}} +
    annotate("text",cex=1.2, label="bike",
             x=mylim_x, y=max(pdata$y,na.rm=T)/100 * -5.5,col='black',hjust=1)  +
    {if(dim(sourcedata %>% filter(hr.sensor == TRUE) %>% filter(hrmax > mylim_x + 1))[1] > 0) {
      geom_point(data=sourcedata %>% filter(hr.sensor == TRUE) %>% filter(hrmax > mylim_x + 1),
                 aes(x=hrmax,y=max(pdata$y,na.rm=T)/100 * -7), cex=1,alpha=0.15,shape=16,col="gray50")}} +
    annotate("text",cex=1.2, label="chestHR",
             x=mylim_x, y=max(pdata$y,na.rm=T)/100 * -7,col='black',hjust=1)  +
    # ylim(0,NA) +
    ylim(max(pdata$y,na.rm=T)/100 * -7,NA) +
    # xlim(140,NA) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(mylim_x, NA)) +
    labs(x='HR (bpm)',y="",title=paste0("maxHR calculation for athlete ",ath.code)) +
    {if(wrist_removed == FALSE){
      annotate("rect",
               xmin=mylim_x*1.012,xmax=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.48+(mylim_x+1), #to get 35% of the window
               ymin=max(pdata$y,na.rm=T)*0.77,ymax=max(pdata$y,na.rm=T)*1.03,
               fill=box_sum_col,color=NA,alpha=0.85)} else {
                 annotate("rect",
                          xmin=mylim_x*1.012,xmax=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.48+(mylim_x+1), #to get 35% of the window
                          ymin=max(pdata$y,na.rm=T)*0.88,ymax=max(pdata$y,na.rm=T)*1.03,
                          fill=box_sum_col,color=NA,alpha=0.95)}} +
    {if(chest_perc < 30){
      annotate("text",cex=2,
               label='WARNING!\nWrist-based HR activities not removed: \nChest-based HR activities < 30%',
               x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1), # to get 5% of the window
               y=max(pdata$y,na.rm=T) *0.82,col=box_sum_text,hjust=0, fontface=2, lineheight = .93)}} +
    {if(chest_perc > 30 & (dim(sourcedata)[1] * chest_perc / 100) < 10){
      annotate("text",cex=2,
               label='WARNING!\nWrist-based HR activities not removed: \nChest-based HR activities < 10',
               x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1), # to get 5% of the window
               y=max(pdata$y,na.rm=T) *0.82,col=box_sum_text,hjust=0, fontface=2, lineheight = .93)}} +
    annotate("text",cex=2.5,
             label=paste0('maxHR = ',round(xinterc,0)," bpm"),
             x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1), # to get 5% of the window
             y=max(pdata$y,na.rm=T),col=box_sum_text,hjust=0, fontface = 2)  +
    annotate("text", cex=2.5,
             label=paste0('n = ',length(sourcedata$hrmax), " activities"),
             x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1),
             y=max(pdata$y,na.rm=T)*0.96,col=box_sum_text,hjust=0) +
    annotate("text", cex=2.5,
             label=paste0('n > maxHR = ',error_act," (",round(error_perc,1),"%)"),
             x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1),
             y=max(pdata$y,na.rm=T)*0.92,col=box_sum_text,hjust=0) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank()) +
    # add info about devices
    annotate("rect",
             xmin=mylim_x*1.012,xmax=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.4+(mylim_x+1), #to get 35% of the window
             ymin=max(pdata$y,na.rm=T)/10*0.1,ymax=max(pdata$y,na.rm=T)/10*1.9,
             fill="white",color=NA,alpha=0.95) +
    annotate("text",cex=2.5,
             label=paste0("Devices:"),
             x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1), # to get 5% of the window
             y=max(pdata$y,na.rm=T)/10 * 1.7,col='gray10',hjust=0) +
    annotate("text",cex=2,
             label=paste0(names(dev_info)[1],": ", round(dev_info[1],1), "%"),
             x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1), # to get 5% of the window
             y=max(pdata$y,na.rm=T)/10 * 1.3,col='gray10',hjust=0) +
    {if(length(dev_info) > 1) {
      annotate("text",cex=2,
               label=paste0(names(dev_info)[2],": ", round(dev_info[2],1), "%"),
               x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1), # to get 5% of the window
               y=max(pdata$y,na.rm=T)/10,col='gray10',hjust=0)}} +
    # add percentage of chest
    annotate("text",cex=2,
             label=paste0("chest-based HR: ", round(chest_perc,1), "%"),
             x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1), # to get 5% of the window
             y=max(pdata$y,na.rm=T)/10 * 0.3,col='gray60',hjust=0)
  plot(p)
  dev.off()
  hrmax <- round(xinterc,0)
  return(hrmax)
}
