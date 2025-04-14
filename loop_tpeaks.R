for (i in ath.info.test$ath.id) {
  # for (i in "BA_002") {
  ath.activities <- all.activities %>% filter(ath.id==i) %>% filter(!is.na(sport_type))
  tests <- c(ath.info.test$test_date_1[ath.info.test$ath.id == i]
             # ,ath.info.test$test_date_2[ath.info.test$ath.id == i]
  )
  for (test_i in seq_len(sum(!is.na(tests)))) {
    if (test_i == 1){
      t = "test1"
      t.date = tests[test_i]
    } else {
      t = "test2"
      t.date = tests[test_i]
    }
    ## activity counter per month
    for (befnext in c("prev", "next")) {
      days.prev = 0
      for (period in c(
        # "prevweek",
        "month1",
        "month2",
        "month3",
        "month4",
        "month5",
        "month6",
        "month7",
        "month8",
        "month9",
        "month10",
        "month11",
        "month12")){
        days=case_when(period == "month1" ~ 28,            # 4 weeks
                       period == "month2" ~ 56,            # 4 weeks
                       period == "month3" ~ 91,            # 5 weeks
                       period == "month4" ~ 119,           # 4 weeks
                       period == "month5" ~ 147,           # 4 weeks
                       period == "month6" ~ 182,           # 5 weeks
                       period == "month7" ~ 210,           # 4 weeks
                       period == "month8" ~ 238,           # 4 weeks
                       period == "month9" ~ 273,           # 5 weeks
                       period == "month10" ~ 301,          # 4 weeks 
                       period == "month11" ~ 329,          # 4 weeks
                       period == "month12" ~ 364, TRUE ~ 0)    # 5 weeks
        # choose if next or previous
        if(befnext == "prev"){
          sport.period.activities <- ath.activities %>% 
            filter(as.Date(as.character(date),"%Y%m%d") < t.date - days.prev,
                   as.Date(as.character(date),"%Y%m%d") >= t.date - days)
        } else {
          # get all activities for the the period
          sport.period.activities <- ath.activities %>% 
            filter(as.Date(as.character(date),"%Y%m%d") > t.date + days.prev,
                   as.Date(as.character(date),"%Y%m%d") <= t.date + days)
        }
        res[res$ath.id == i,paste0(t,".sessions.total.", befnext,period)] <- sport.period.activities %>%
          count() %>% as.numeric()
        days.prev <- days
      } # end period
    } #end befnext
    # end activity counter
    
    for (period in c(
      # "prevweek",
      "prev1months",
      # "prev2months",
      "prev3months",
      # "prev4months",
      # "prev5months",
      "prev6months",
      # "prev7months",
      # "prev8months",
      # "prev9months",
      # "prev10months",
      # "prev11months",
      "prev12months",
      "next1months",
      "next3months",
      "next6months",
      "next12months")){
      days=case_when(period == "prevweek" ~ 7,
                     period == "prev1months" ~ 28,            # 4 weeks
                     period == "next1months" ~ 28,            # 4 weeks
                     period == "prev2months" ~ 56,            # 4 weeks
                     period == "prev3months" ~ 91,            # 5 weeks
                     period == "next3months" ~ 91,            # 5 weeks
                     period == "prev4months" ~ 119,           # 4 weeks
                     period == "prev5months" ~ 147,           # 4 weeks
                     period == "prev6months" ~ 182,           # 5 weeks
                     period == "next6months" ~ 182,           # 5 weeks
                     period == "prev7months" ~ 210,           # 4 weeks
                     period == "prev8months" ~ 238,           # 4 weeks
                     period == "prev9months" ~ 273,           # 5 weeks
                     period == "prev10months" ~ 301,          # 4 weeks 
                     period == "prev11months" ~ 329,          # 4 weeks
                     period == "prev12months" ~ 364,
                     period == "next12months" ~ 364, TRUE ~ 0)    # 5 weeks
      # choose if next or previous
      if(grepl("next", period)){
        sport.period.activities <- ath.activities %>% 
          filter(as.Date(as.character(date),"%Y%m%d") > t.date,
                 as.Date(as.character(date),"%Y%m%d") <= t.date + days)
      } else {
        # get all activities for the the period
        sport.period.activities <- ath.activities %>% 
          filter(as.Date(as.character(date),"%Y%m%d") < t.date,
                 as.Date(as.character(date),"%Y%m%d") >= t.date - days)
      }
      
      ######################################
      # ## HR ZONES PER TIME PERIOD SRC
      hr_summary <- sport.period.activities %>%
        # remove activities without HR
        filter(!is.na(hrmax.activity)) %>%
        group_by(ath.id) %>% 
        # filter(ath.id == "CMR_010") %>% 
        summarise(duration.min.total = sum(duration.min, na.rm = T),
                  hr.z0.min.total = sum(hr.z0.time, na.rm = T),
                  hr.z1.min.total = sum(hr.z1.time, na.rm = T),
                  hr.z2.min.total = sum(hr.z2.time, na.rm = T),
                  hr.z3.min.total = sum(hr.z3.time, na.rm = T),
                  hr.z4.min.total = sum(hr.z4.time, na.rm = T),
                  hr.z5.min.total = sum(hr.z5.time, na.rm = T),
                  hr.zones.min.total = hr.z0.min.total + hr.z1.min.total + hr.z2.min.total + hr.z3.min.total + hr.z4.min.total + hr.z5.min.total,
                  hr.zones.min.total.difference = round(hr.zones.min.total / duration.min.total * 100,1)
        ) %>% 
        mutate(hr.z0.perc.total = round(hr.z0.min.total / duration.min.total * 100, 1),
               hr.z1.perc.total = round(hr.z1.min.total / duration.min.total * 100, 1),
               hr.z2.perc.total = round(hr.z2.min.total / duration.min.total * 100, 1),
               hr.z3.perc.total = round(hr.z3.min.total / duration.min.total * 100, 1),
               hr.z4.perc.total = round(hr.z4.min.total / duration.min.total * 100, 1),
               hr.z5.perc.total = round(hr.z5.min.total / duration.min.total * 100, 1),
               hr.zones.perc.total = hr.z0.perc.total + hr.z1.perc.total + hr.z2.perc.total + hr.z3.perc.total + hr.z4.perc.total + hr.z5.perc.total) %>% 
        rename(duration.min.total.withHR = duration.min.total)
      
      hr_summary_NOHR <- sport.period.activities %>%
        # get activities without HR
        filter(is.na(hrmax.activity)) %>% 
        group_by(ath.id) %>%
        summarise(duration.min.total.noHR = round(sum(duration.min, na.rm = T),1))
      
      hr_summary <- hr_summary %>% 
        left_join(hr_summary_NOHR, by = "ath.id") %>% 
        mutate(duration.min.total.noHR = ifelse(is.na(duration.min.total.noHR), 0, duration.min.total.noHR),
               duration.min.total = duration.min.total.withHR + duration.min.total.noHR,
               perc.total.withHR = round(duration.min.total.withHR / duration.min.total * 100,1)) %>% 
        relocate(duration.min.total.noHR, duration.min.total, perc.total.withHR, .after=duration.min.total.withHR)
      ###################################
      
      
      if (dim(hr_summary)[1] == 0) {
        res[res$ath.id == i,paste0(t,".duration.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".duration.min.total.withHR.",period)] <- NA
        res[res$ath.id == i,paste0(t,".duration.min.total.noHR.",period)] <- NA
        res[res$ath.id == i,paste0(t,".perc.total.withHR.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z0.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z1.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z2.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z3.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z4.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z5.min.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z0.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z1.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z2.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z3.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z4.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.z5.perc.total.",period)] <- NA
        res[res$ath.id == i,paste0(t,".hr.zones.perc.total.",period)] <- NA
        
      } else {
        res[res$ath.id == i,paste0(t,".duration.min.total.",period)] <- hr_summary$duration.min.total
        res[res$ath.id == i,paste0(t,".duration.min.total.withHR.",period)] <- hr_summary$duration.min.total.withHR
        res[res$ath.id == i,paste0(t,".duration.min.total.noHR.",period)] <- hr_summary$duration.min.total.noHR
        res[res$ath.id == i,paste0(t,".perc.total.withHR.",period)] <- hr_summary$perc.total.withHR
        res[res$ath.id == i,paste0(t,".hr.z0.min.total.",period)] <- hr_summary$hr.z0.min.total
        res[res$ath.id == i,paste0(t,".hr.z1.min.total.",period)] <- hr_summary$hr.z1.min.total
        res[res$ath.id == i,paste0(t,".hr.z2.min.total.",period)] <- hr_summary$hr.z2.min.total
        res[res$ath.id == i,paste0(t,".hr.z3.min.total.",period)] <- hr_summary$hr.z3.min.total
        res[res$ath.id == i,paste0(t,".hr.z4.min.total.",period)] <- hr_summary$hr.z4.min.total
        res[res$ath.id == i,paste0(t,".hr.z5.min.total.",period)] <- hr_summary$hr.z5.min.total
        res[res$ath.id == i,paste0(t,".hr.z0.perc.total.",period)] <- hr_summary$hr.z0.perc.total
        res[res$ath.id == i,paste0(t,".hr.z1.perc.total.",period)] <- hr_summary$hr.z1.perc.total
        res[res$ath.id == i,paste0(t,".hr.z2.perc.total.",period)] <- hr_summary$hr.z2.perc.total
        res[res$ath.id == i,paste0(t,".hr.z3.perc.total.",period)] <- hr_summary$hr.z3.perc.total
        res[res$ath.id == i,paste0(t,".hr.z4.perc.total.",period)] <- hr_summary$hr.z4.perc.total
        res[res$ath.id == i,paste0(t,".hr.z5.perc.total.",period)] <- hr_summary$hr.z5.perc.total
        res[res$ath.id == i,paste0(t,".hr.zones.perc.total.",period)] <- hr_summary$hr.zones.perc.total
      }
      #### HR END
      # total sessions
      res[res$ath.id == i,paste0(t,".sessions.total.",period)] <- sport.period.activities %>%
        count() %>% as.numeric()
      # monthly sessions average
      res[res$ath.id == i,paste0(t,".sessions.monthavg.",period)] <- round(res[res$ath.id == i,paste0(t,".sessions.total.", period)] / days * 28, 1)
      # total trimps
      res[res$ath.id == i,paste0(t,".etrimp.total.",period)] <- sport.period.activities %>%
        summarise(total_trimp = round(sum(etrimp,na.rm=T),1)) %>% as.numeric()
      # res[res$ath.id == i,paste0(t,".lutrimp.total.",period)] <- sport.period.activities %>%
      # summarise(total_trimp = round(sum(lutrimp,na.rm=T),1)) %>% as.numeric()
      # res[res$ath.id == i,paste0(t,".lutrimp_goldvt.total.",period)] <- if (all(is.na(sport.period.activities$lutrimp.goldvt)))
      # {NA} else {
      # sport.period.activities %>% summarise(total_trimp = round(sum(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric() }
      # weekly average
      if(period != "prevweek") {
        res[res$ath.id == i,paste0(t,".sessions.wkavg.",period)] <- 
          round(res[res$ath.id == i,paste0(t,".sessions.total.",period)]/(days/7),1)
        res[res$ath.id == i,paste0(t,".etrimp.wkavg.",period)] <- 
          round(res[res$ath.id == i,paste0(t,".etrimp.total.",period)]/(days/7),1)
        # res[res$ath.id == i,paste0(t,".lutrimp.wkavg.",period)] <- 
        #   round(res[res$ath.id == i,paste0(t,".lutrimp.total.",period)]/(days/7),1)
        # res[res$ath.id == i,paste0(t,".lutrimp_goldvt.wkavg.",period)] <- 
        #   round(res[res$ath.id == i,paste0(t,".lutrimp_goldvt.total.",period)]/(days/7),1)
      }
      
      ################################
      # monotony
      if (period == "prevweek"){
        res[res$ath.id == i,paste0(t,".etrimp.monotony.",period)] <- sport.period.activities %>%
          summarise(monotony = round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),1)) %>% as.numeric()
        # res[res$ath.id == i,paste0(t,".lutrimp.monotony.",period)] <- sport.period.activities %>%
        # summarise(monotony = round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),1)) %>% as.numeric()
        # res[res$ath.id == i,paste0(t,".lutrimp_goldvt.monotony.",period)] <- if (all(is.na(sport.period.activities$lutrimp.goldvt)))
        # {NA} else { sport.period.activities %>%
        # summarise(monotony = round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric()}
      } else {
        wk_monot.etrimp <- NULL
        # wk_monot.lutrimp <- NULL
        # wk_monot.lutrimp_goldvt <- NULL
        
        for (wk in seq_len(days/7)){
          # loop through weeks
          # change loop as we need now before and after the test! (SRC UPDATE FEB 2023)
          if (grepl("next", period)){ # next periods
            wk_monot.etrimp <- c(wk_monot.etrimp, ath.activities %>%
                                   filter(as.Date(as.character(date),"%Y%m%d") > (t.date + (7*(wk-1))),
                                          as.Date(as.character(date),"%Y%m%d") <= (t.date + (7*wk))) %>%
                                   summarise(m=round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),2)) %>% as.numeric())
          } else { # previous periods
            wk_monot.etrimp <- c(wk_monot.etrimp, ath.activities %>%
                                   filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
                                          as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>%
                                   summarise(m=round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),2)) %>% as.numeric())
          }
          ##### remove lutrimp below
          # wk_monot.lutrimp <- c(wk_monot.lutrimp, ath.activities %>%
          #                         filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
          #                                as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>%
          #                         summarise(m=round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),2)) %>% as.numeric())
          # wk_monot.lutrimp_goldvt <- c(wk_monot.lutrimp_goldvt, ath.activities %>%
          #                                filter(as.Date(as.character(date),"%Y%m%d") < (t.date - (7*(wk-1))),
          #                                       as.Date(as.character(date),"%Y%m%d") >= (t.date - (7*wk))) %>%
          #                                summarise(m=round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),2)) %>% as.numeric())
        } # week loop
        res[res$ath.id == i,paste0(t,".etrimp.monotony.",period)] <- round(sum(wk_monot.etrimp,na.rm=T)/(days/7)/
                                                                             sd(wk_monot.etrimp,na.rm=T),2)
        # res[res$ath.id == i,paste0(t,".lutrimp.monotony.",period)] <- round(sum(wk_monot.lutrimp,na.rm=T)/(days/7)/
        # sd(wk_monot.lutrimp,na.rm=T),2)
        # res[res$ath.id == i,paste0(t,".lutrimp_goldvt.monotony.",period)] <- round(sum(wk_monot.lutrimp_goldvt,na.rm=T)/(days/7)/
        # sd(wk_monot.lutrimp_goldvt,na.rm=T),2)
      } # end monotony calculation
      # sessions per sport
      for (sport in unique(ath.activities$sport_type)) {
        # to name variable
        if (sport == "Running") {
          sp="run"
        } else if (sport == "Cycling") {
          sp="bike"
        } else {
          sp="other"
        }
        
        # get all activities for the sport and the period
        # choose if next or previous
        if(grepl("next", period)){
          sport.period.activities <- ath.activities %>% 
            filter(sport_type == sport) %>%
            filter(as.Date(as.character(date),"%Y%m%d") > t.date,
                   as.Date(as.character(date),"%Y%m%d") <= t.date + days)
        } else {
          # get all activities for the the period
          sport.period.activities <- ath.activities %>% 
            filter(sport_type == sport) %>%
            filter(as.Date(as.character(date),"%Y%m%d") < t.date,
                   as.Date(as.character(date),"%Y%m%d") >= t.date - days)
        }
        
        if (dim(sport.period.activities)[1] == 0) {
          # go to next sport
          res[res$ath.id == i,paste0(t,".",sp,".sessions.total.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".hours.total.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".km.total.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".hours.session.avg.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".km.session.avg.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".hours.session.max.",period)] <- 0
          res[res$ath.id == i,paste0(t,".",sp,".km.session.max.",period)] <- 0
          next
        }
        # total sessions
        res[res$ath.id == i,paste0(t,".",sp,".sessions.total.",period)] <- sport.period.activities %>%
          count() %>% as.numeric()
        # weekly average
        if(period != "prevweek") {
          # sessions
          res[res$ath.id == i,paste0(t,".",sp,".sessions.wkavg.",period)] <-
            round(res[res$ath.id == i,paste0(t,".",sp,".sessions.total.",period)]/(days/7),1)
        }
        # total hours
        res[res$ath.id == i,paste0(t,".",sp,".hours.total.",period)] <- sport.period.activities %>%
          summarise(total_duration = round(sum(duration.min, na.rm = T)/60,1)) %>% as.numeric()
        # weekly average
        if(period != "prevweek") {
          # hours
          res[res$ath.id == i,paste0(t,".",sp,".hours.wkavg.",period)] <-
            round(res[res$ath.id == i,paste0(t,".",sp,".hours.total.",period)]/(days/7),1)
        }
        # total distance in km
        res[res$ath.id == i,paste0(t,".",sp,".km.total.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(sum(total_dist.km, na.rm = T),2)) %>% as.numeric()
        # weekly average
        if(period != "prevweek") {
          # distance
          res[res$ath.id == i,paste0(t,".",sp,".km.wkavg.",period)] <-
            round(res[res$ath.id == i,paste0(t,".",sp,".km.total.",period)]/(days/7),2)
        }
        # single session average hours
        res[res$ath.id == i,paste0(t,".",sp,".hours.session.avg.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(mean(duration.min)/60,1)) %>% as.numeric()
        # single session average distance
        res[res$ath.id == i,paste0(t,".",sp,".km.session.avg.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(mean(total_dist.km),2)) %>% as.numeric()
        # single session max hours
        res[res$ath.id == i,paste0(t,".",sp,".hours.session.max.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(max(duration.min)/60,1)) %>% as.numeric()
        # single session max distance
        res[res$ath.id == i,paste0(t,".",sp,".km.session.max.",period)] <- sport.period.activities %>%
          summarise(total_distance = round(max(total_dist.km),2)) %>% as.numeric()
      } # end sport loop
    } # end period loop
    
    # # between tests
    # if(test_i == 2) {
    #   # total sessions
    #   t1.date = tests[1]
    #   t2.date = tests[2]
    #   weeks.btw = round(as.numeric(t2.date - t1.date)/7,0)
    #   period.activities <- ath.activities %>% 
    #     filter(as.Date(as.character(date),"%Y%m%d") < t2.date,
    #            as.Date(as.character(date),"%Y%m%d") > t1.date)
    #   res[res$ath.id == i,"btwtests.weeks"] <- weeks.btw
    #   res[res$ath.id == i,"btwtests.sessions.total"] <- period.activities %>% count() %>% as.numeric()
    #   res[res$ath.id == i,"btwtests.sessions.wkavg"] <- round(res[res$ath.id == i,"btwtests.sessions.total"]/weeks.btw,1)
    #   
    #   #########
    #   # total trimps
    #   res[res$ath.id == i,paste0("btwtests.etrimp.total")] <- period.activities %>%
    #     summarise(total_trimp = round(sum(etrimp,na.rm=T),1)) %>% as.numeric()
    #   res[res$ath.id == i,paste0("btwtests.lutrimp.total")] <- period.activities %>%
    #     summarise(total_trimp = round(sum(lutrimp,na.rm=T),1)) %>% as.numeric()
    #   res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.total")] <- if (all(is.na(period.activities$lutrimp.goldvt)))
    #   {NA} else {period.activities %>% summarise(total_trimp = round(sum(lutrimp.goldvt,na.rm=T),1)) %>% as.numeric() }
    #   # weekly average
    #   res[res$ath.id == i,paste0("btwtests.etrimp.wkavg")] <- 
    #     round(res[res$ath.id == i,paste0("btwtests.etrimp.total")]/weeks.btw,1)
    #   res[res$ath.id == i,paste0("btwtests.lutrimp.wkavg")] <- 
    #     round(res[res$ath.id == i,paste0("btwtests.lutrimp.total")]/weeks.btw,1)
    #   res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.wkavg")] <- 
    #     round(res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.total")]/weeks.btw,1)
    #   # monotony
    #   wk_monot.etrimp <- NULL
    #   wk_monot.lutrimp <- NULL
    #   wk_monot.lutrimp_goldvt <- NULL
    #   for (wk in seq_len(weeks.btw)){
    #     # loop through weeks
    #     wk_monot.etrimp <- c(wk_monot.etrimp, ath.activities %>% 
    #                            filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
    #                                   as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
    #                            summarise(m=round(sum(etrimp,na.rm=T)/7/sd(etrimp,na.rm=T),2)) %>% as.numeric())
    #     wk_monot.lutrimp <- c(wk_monot.lutrimp, ath.activities %>% 
    #                             filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
    #                                    as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
    #                             summarise(m=round(sum(lutrimp,na.rm=T)/7/sd(lutrimp,na.rm=T),2)) %>% as.numeric())
    #     wk_monot.lutrimp_goldvt <- c(wk_monot.lutrimp_goldvt,
    #                   ath.activities %>% 
    #                     filter(as.Date(as.character(date),"%Y%m%d") < (t2.date - (7*(wk-1))),
    #                            as.Date(as.character(date),"%Y%m%d") >= (t2.date - (7*wk))) %>% 
    #                     summarise(m=round(sum(lutrimp.goldvt,na.rm=T)/7/sd(lutrimp.goldvt,na.rm=T),2)) %>% as.numeric())
    #   } # week loop
    #   res[res$ath.id == i,paste0("btwtests.etrimp.monotony")] <- round(sum(wk_monot.etrimp,na.rm=T)/weeks.btw/
    #                                                                      sd(wk_monot.etrimp,na.rm=T),2)
    #   res[res$ath.id == i,paste0("btwtests.lutrimp.monotony")] <- round(sum(wk_monot.lutrimp,na.rm=T)/weeks.btw/
    #                                                                       sd(wk_monot.lutrimp,na.rm=T),2)
    #   res[res$ath.id == i,paste0("btwtests.lutrimp_goldvt.monotony")] <-
    #     round(sum(wk_monot.lutrimp_goldvt,na.rm=T)/weeks.btw/sd(wk_monot.lutrimp_goldvt,na.rm=T),2)
    #   # monotony calculation
    #   #########
    #   
    #   # sport loop
    #   for (sport in unique(ath.activities$sport_type)) {
    #     # to name variable
    #     if (sport == "Running") {
    #       sp="run"
    #     } else if (sport == "Cycling") {
    #       sp="bike"
    #     } else {
    #       sp="other"
    #     }
    #     # get all activities for the sport and the period
    #     sport.period.activities <- ath.activities %>% 
    #       filter(sport_type == sport) %>% 
    #       filter(as.Date(as.character(date),"%Y%m%d") < t2.date,
    #              as.Date(as.character(date),"%Y%m%d") > t1.date)
    #     
    #     if (dim(sport.period.activities)[1] == 0) {
    #       # go to next sport
    #       res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".km.total")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.avg")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".km.session.avg")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.max")] <- 0
    #       res[res$ath.id == i,paste0("btwtests.",sp,".km.session.max")] <- 0
    #       next
    #     }
    #     # total sessions
    #     res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")] <- sport.period.activities %>%
    #       count() %>% as.numeric()
    #     # weekly average sessions
    #     res[res$ath.id == i,paste0("btwtests.",sp,".sessions.wkavg")] <- 
    #       round(res[res$ath.id == i,paste0("btwtests.",sp,".sessions.total")]/weeks.btw,1)
    #     # total hours
    #     res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")] <- sport.period.activities %>%
    #       summarise(total_duration = round(sum(duration.min, na.rm = T)/60,1)) %>% as.numeric()
    #     # weekly average hours
    #     res[res$ath.id == i,paste0("btwtests.",sp,".hours.wkavg")] <- 
    #       round(res[res$ath.id == i,paste0("btwtests.",sp,".hours.total")]/weeks.btw,1)
    #     # total distance in km
    #     res[res$ath.id == i,paste0("btwtests.",sp,".km.total")] <- sport.period.activities %>%
    #       summarise(total_distance = round(sum(total_dist.km, na.rm = T),2)) %>% as.numeric()
    #     # weekly average distance
    #     res[res$ath.id == i,paste0("btwtests.",sp,".km.wkavg")] <- 
    #       round(res[res$ath.id == i,paste0("btwtests.",sp,".km.total")]/weeks.btw,2)
    #     # single session average hours
    #     res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.avg")] <- sport.period.activities %>%
    #       summarise(total_distance = round(mean(duration.min)/60,1)) %>% as.numeric()
    #     # single session average distance
    #     res[res$ath.id == i,paste0("btwtests.",sp,".km.session.avg")] <- sport.period.activities %>%
    #       summarise(total_distance = round(mean(total_dist.km),2)) %>% as.numeric()
    #     # single session max hours
    #     res[res$ath.id == i,paste0("btwtests.",sp,".hours.session.max")] <- sport.period.activities %>%
    #       summarise(total_distance = round(max(duration.min)/60,1)) %>% as.numeric()
    #     # single session max distance
    #     res[res$ath.id == i,paste0("btwtests.",sp,".km.session.max")] <- sport.period.activities %>%
    #       summarise(total_distance = round(max(total_dist.km),2)) %>% as.numeric()
    #   } # end sport loop
    # } # end between test calculations
  } # end test loop
} # end athlete loop