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

process_maxhr_activity <- function(myact, w_e, w_d, w_x) {
  mypdata <- ggplot_build(ggplot(myact) + stat_density(aes(x=hrmax),bw=3))$data[[1]] %>% 
    select(x,y) %>% 
    mutate(deriv = (y - lag(y))/ (x - lag(x)),
           yinterc = y - x*deriv,
           xinterc = round(-yinterc/deriv,0),
           xinterc_diff = xinterc - lag(xinterc, default = 0)) %>% 
    rowwise() %>% 
    mutate(error_act = sum(myact$hrmax > xinterc),
           error_perc = sum(myact$hrmax > xinterc)/length(myact$hrmax) * 100
    ) %>% 
    ungroup() %>% 
    mutate(rank_error = dense_rank(desc(error_act * error_perc)),
           rank_xintercdiff = dense_rank(desc(abs(xinterc_diff))),
           score_error = (rank_error - min(rank_error, na.rm=T)) / (max(rank_error, na.rm=T) - min(rank_error, na.rm=T)),
           score_deriv = -(deriv-max(deriv, na.rm = T)) / (max(deriv,na.rm=T) - min(deriv, na.rm=T)),
           score_xintercdiff = (rank_xintercdiff - min(rank_xintercdiff, na.rm=T)) / (max(rank_xintercdiff, na.rm=T) - min(rank_xintercdiff, na.rm=T)),
           score_all = score_error * w_e + score_deriv * w_d + score_xintercdiff * w_x,
           score_all_rank = ntile(desc(score_all), n())
    )
  return(mypdata)
}


############################################################################################################
## GET MAXHR WITH TANGENT METHOD 
## use tangent method with all maxhr for all activities to calculate the actual maxHR
###
## input 
## output maxHR and plot saved in "png" folder
############################################################################################################

get.maxhr_tangent <- function(hr_vs_all,ath.code) {
  mylim_x <- NULL
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
              hr.sensor=first(hr.sensor),
              .groups="drop")
  maxhr_per_activity.140 <- maxhr_per_activity %>% 
    filter(hrmax > 140) # 11FEB meeting addition
  
  # some cases with very few activities with >140, so in case there are less than 10, I will use the whole dataset
  if(dim(maxhr_per_activity.140)[1] < 10) {
    maxhr_per_activity.140 <- maxhr_per_activity
    #change limits for plotting later
    mylim_x <- 89
  }

  # pdata = ggplot_build(ggplot(maxhr_per_activity.140) + stat_density(aes(x=hrmax),bw=3))$data[[1]]
  # updated process 210818
  pdata <- process_maxhr_activity(maxhr_per_activity.140, w_e = 0.3, w_d = 1, w_x = 1)
  best_row <- pdata %>% filter(score_all_rank == 1 )
  yinterc <- best_row$yinterc
  xinterc <- best_row$xinterc
  tan_slope <- best_row$deriv
  error_act <- sum(maxhr_per_activity$hrmax > xinterc)
  error_perc <- sum(maxhr_per_activity$hrmax > xinterc) / length(maxhr_per_activity$hrmax) * 100
  
  # fix mylim depending on whether there are activities with max 140 (see above)
  if(is.null(mylim_x)) {mylim_x <- 139}
  
  # save plot
  png(filename=paste0('out/png/maxHR-',ath.code,'.png'),width = 1440, height = 1440,res=300)
  p <- ggplot(pdata,aes(x,y)) +
    geom_line(cex=1.2) +
    geom_abline(slope=tan_slope,intercept=yinterc,color="red",cex=0.8) +
    geom_abline(slope=0,intercept=0) +
    geom_vline(xintercept = xinterc,col='gray50',cex=0.5,linetype="dashed") +
    # annotate("rect",
    #          xmin=140,xmax=max(pdata$x,na.rm=T), 
    #          ymin=max(pdata$y,na.rm=T)/100 * -10,ymax=0,
    #          fill="white",color=NA) +
    geom_point(data=maxhr_per_activity.140,
               aes(x=hrmax,y=max(pdata$y,na.rm=T)/100 * -2), cex=1,alpha=0.15,shape=16,col="black") +
    annotate("text",cex=1.5, label="all",
             x=mylim_x, y=max(pdata$y,na.rm=T)/100 * -2,col='black',hjust=1)  +
    {if(dim(maxhr_per_activity.140 %>% filter(sport == 1))[1] > 0) {
      geom_point(data=maxhr_per_activity.140 %>% filter(sport == 1),
                 aes(x=hrmax,y=max(pdata$y,na.rm=T)/100 * -4), cex=1,alpha=0.15,shape=16,col="gray50")}} +
    annotate("text",cex=1.2, label="run",
             x=mylim_x, y=max(pdata$y,na.rm=T)/100 * -4,col='black',hjust=1)  +
    {if(dim(maxhr_per_activity.140 %>% filter(sport == 2))[1] > 0) {
      geom_point(data=maxhr_per_activity.140 %>% filter(sport == 2),
                 aes(x=hrmax,y=max(pdata$y,na.rm=T)/100 * -5.5), cex=1,alpha=0.15,shape=16,col="gray50")}} +
    annotate("text",cex=1.2, label="bike",
             x=mylim_x, y=max(pdata$y,na.rm=T)/100 * -5.5,col='black',hjust=1)  +
    {if(dim(maxhr_per_activity.140 %>% filter(hr.sensor == TRUE))[1] > 0) {
      geom_point(data=maxhr_per_activity.140 %>% filter(hr.sensor == TRUE),
                 aes(x=hrmax,y=max(pdata$y,na.rm=T)/100 * -7), cex=1,alpha=0.15,shape=16,col="gray50")}} +
    annotate("text",cex=1.2, label="chestHR",
             x=mylim_x, y=max(pdata$y,na.rm=T)/100 * -7,col='black',hjust=1)  +
    # ylim(0,NA) +
    ylim(max(pdata$y,na.rm=T)/100 * -7,NA) +
    # xlim(140,NA) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(mylim_x, NA)) +
    labs(x='HR (bpm)',y="",title=paste0("maxHR calculation for athlete ",ath.code)) +
    annotate("rect",
             xmin=mylim_x+2,xmax=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.35+(mylim_x+1), #to get 35% of the window
             ymin=max(pdata$y,na.rm=T)*0.93,ymax=max(pdata$y,na.rm=T)*1.03,
             fill="white",color=NA,alpha=0.95) +
    annotate("text",cex=2.5,
             label=paste0('maxHR=',round(xinterc,0)," bpm"),
             x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1), # to get 5% of the window
             y=max(pdata$y,na.rm=T),col='red',hjust=0)  +
    annotate("text", cex=2.5,
             label=paste0('n > maxHR=',error_act," (",round(error_perc,1),"%)"),
             x=(max(pdata$x,na.rm=T)-(mylim_x+1))*0.02+(mylim_x+1),
             y=max(pdata$y,na.rm=T)*0.96,col='red',hjust=0) +
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