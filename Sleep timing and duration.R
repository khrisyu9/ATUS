library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(GGally)
library(gridExtra)
library(grid)
library(latex2exp)
library(weights)
library(scales)
library(stats)
# library(SDMTools)
library(knitr)

options(scipen=999)
##############################################################################
# Original time in r: as.numeric(as.POSIXct("1969-12-31 19:00:00"))
# today's 00:00:00 -- as.numeric(as.POSIXct(paste(Sys.Date(), "00:00:00")))
for (s.year in 2003:2018) {
  file.name = sprintf("D:/galit/7th Report_selection compare 2 and act 2 hours bef/actlog_galit.%s.csv",s.year)
  g.actlog = data.table(read.csv(file.name) %>%
    mutate(start = as.POSIXct(start) +
             as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         stop = as.POSIXct(stop) +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))))) %>%
    filter(keep == 1) %>%
    group_by(tucaseid) %>%
    mutate(primary.sleep.number = ifelse(primary.sleep == "primary sleep", tuactivity_n, 0),
           primary.sleep.number = ifelse(primary.sleep.number == max(primary.sleep.number),
                                         primary.sleep.number, 0)) %>%
    select(tucaseid, tuactivity_n, trcode, start, stop, last,
           primary.sleep, keep, primary.sleep.number)

  gid = unique((g.actlog %>% filter(primary.sleep == "primary sleep", last == 0))$tucaseid)

  g.actlog2 = g.actlog %>% filter(!(tucaseid %in% gid)) %>% ungroup()

  for (id in gid) {
    data = g.actlog %>%
      filter(tucaseid == id) %>% ungroup()

    data0 = data %>%
      group_by(tucaseid) %>%
      filter(tuactivity_n >= max(primary.sleep.number)) %>%
      filter(start < min(stop)+2*60*60, trcode == 10101)

    if(nrow(data0)>1){
      data0 = data0 %>% mutate(primary.sleep = ifelse(primary.sleep=="primary sleep",
                                                      "old primary sleep",
                                                      "other sleep"),
                               primary.sleep.number = 0, keep = 0) %>%
        ungroup()
      new_sleep = data.table(tucaseid = id, tuactivity_n = max(data0$tuactivity_n),
                           trcode = 10101, start = data0$start[1],
                           stop = data0$stop[nrow(data0)], last = max(data0$last),
                           primary.sleep = "primary sleep", keep = 1,
                           primary.sleep.number = max(data0$tuactivity_n))
      result = data %>%
        group_by(tucaseid) %>%
        filter(tuactivity_n < new_sleep$primary.sleep.number, primary.sleep == "other sleep") %>%
        ungroup()
      
      
      
      result = rbindlist(list(result, new_sleep)) 
      result = rbindlist(list(result, data0))
      
      result = rbindlist(list(result, data %>%
                                group_by(tucaseid) %>%
                                filter(tuactivity_n > max(new_sleep$primary.sleep.number)) %>% 
                                ungroup()))
      
      g.actlog2 = g.actlog2 %>%
        rbind(result)

    } else{
      g.actlog2 = g.actlog2 %>%
        rbind(data)
    }
  }

  gid2 = unique((g.actlog2 %>% filter(tucaseid %in% gid,
                                      primary.sleep == "primary sleep",
                                      last == 0))$tucaseid)
  g.actlog3 = g.actlog2 %>% filter(!(tucaseid %in% gid2)) %>% ungroup()

  for (id in gid2) {
    data = g.actlog2 %>%
      filter(tucaseid == id) %>% ungroup()

    data0 = data %>%
      group_by(tucaseid) %>%
      filter(tuactivity_n >= max(primary.sleep.number)) %>%
      filter(start < min(stop)+2*60*60, trcode == 10101)

    if(nrow(data0)>1){
      data0 = data0 %>% mutate(primary.sleep = ifelse(primary.sleep=="primary sleep",
                                                      "old primary sleep",
                                                      "other sleep"),
                               primary.sleep.number = 0, keep = 0) %>%
        ungroup()
      new_sleep = data.table(tucaseid = id, tuactivity_n = max(data0$tuactivity_n),
                             trcode = 10101, start = data0$start[1],
                             stop = data0$stop[nrow(data0)], last = max(data0$last),
                             primary.sleep = "primary sleep", keep = 1,
                             primary.sleep.number = max(data0$tuactivity_n))
      result = data %>%
        group_by(tucaseid) %>%
        filter(tuactivity_n < new_sleep$primary.sleep.number, primary.sleep == "other sleep") %>%
        ungroup()
      
      
      
      result = rbindlist(list(result, new_sleep)) 
      result = rbindlist(list(result, data0))
      
      result = rbindlist(list(result, data %>%
                                group_by(tucaseid) %>%
                                filter(tuactivity_n > max(new_sleep$primary.sleep.number)) %>% 
                                ungroup()))
      
      g.actlog3 = g.actlog3 %>%
        rbind(result)

    } else{
      g.actlog3 = g.actlog3 %>%
        rbind(data)
    }
  }

  # 9289 - 2018 people included in the dataset
  # 20084 - 2003 people included in the dataset
  g.primary.id = unique((g.actlog3%>%filter(primary.sleep == "primary sleep"))$tucaseid)

  primary.start = g.actlog3 %>%
    group_by(tucaseid) %>%
    mutate(primary.sleep.number = ifelse(primary.sleep == "primary sleep", tuactivity_n, 0),
           primary.sleep.number = ifelse(primary.sleep.number == max(primary.sleep.number),
                                         primary.sleep.number, 0)) %>%
    filter(primary.sleep.number>0) %>%
    mutate(primary.start = start) %>%
    select(tucaseid, primary.start)
  
  g.actlog0 = g.actlog3 %>%
    group_by(tucaseid) %>%
    mutate(primary.sleep.number = ifelse(primary.sleep == "primary sleep", tuactivity_n, 0),
           primary.sleep.number = ifelse(primary.sleep.number == max(primary.sleep.number),
                                         primary.sleep.number, 0),
           primary.sleep.start = ifelse(primary.sleep == "primary sleep", start, 0)) %>%
    inner_join(primary.start, by = "tucaseid") %>%
    filter(start <= primary.start) %>%
    filter(stop > primary.start - 2*60*60) %>%
    select(tucaseid, tuactivity_n, trcode, start, stop, last,
           primary.sleep, primary.sleep.number) %>%
    arrange(tucaseid,tuactivity_n) %>%
    ungroup() 
  a = g.actlog0 %>% filter(tucaseid %in% gid2)
  for (id in g.primary.id) {
    g.actlog0[g.actlog0$tucaseid == id,]$start[1] = g.actlog0[g.actlog0$tucaseid == id,]$stop[
      nrow(g.actlog0[g.actlog0$tucaseid == id,])-1]-2*60*60
  }

  file.name = sprintf("D:/galit/7th Report_selection compare 2 and act 2 hours bef/act_2_and_primary_sleep.Galit.%s.csv",s.year)
  write.csv(g.actlog0 %>%
              mutate(start = start -
                       as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
                     stop = stop -
                       as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))) %>%
              arrange(tucaseid, tuactivity_n),file.name)
  
}
###############################################################################
################## Calculate the Weighted Mean of each Act ####################
###############################################################################

library(doParallel)
library(foreach)
# Setup cores cluster using doParallel
ncores = 4 # detectCores() - 1 

cl = makeCluster(ncores)
registerDoParallel(cl)
sleep.timing.duration =
  foreach(
    s.year = 2003:2018, .combine = "rbind") %dopar% {
      options(scipen=999)
      library(survey)
      library(dplyr)
      library(tidyverse)
      library(ggplot2)
      library(data.table)
      # Original time in r: as.numeric(as.POSIXct("1969-12-31 19:00:00"))
      # today's 00:00:00 -- as.numeric(as.POSIXct(paste(Sys.Date(), "00:00:00")))
      file = sprintf("d:/galit/%s",paste0(s.year,"atus"))
      setwd("D:/galit/3rd Report_act 2 hours bef")
      act_code = read.csv("code_activity.csv")
      codes = act_code[,1]
      activities = act_code[,2]

      setwd(file)

      actsum = read.csv("actsum.csv") %>%
        select(tucaseid, teage, tesex, tudiaryday, trholiday, t010101, tufinlwgt) %>%
        mutate(days = ifelse(tudiaryday %in% c("Sunday", "Monday", "Tuesday",
                                               "Wednesday", "Thursday"), "Weekday", "Weekend"),
               days = ifelse(trholiday == "Diary day was a holiday", "Holiday", days),
               age = ifelse(teage>22,2,1),
               age = ifelse(teage>30,3,age),
               age = ifelse(teage>50,4,age),
               age = ifelse(teage>64,5,age)) %>%
        select(tucaseid, tesex, teage, age, days, tufinlwgt)

      file = sprintf("D:/galit/7th Report_selection compare 2 and act 2 hours bef/act_2_bef_sleep.Galit.%s",
                     paste0(s.year,".csv"))
      actlog = read.csv(file) %>%
        mutate(start = as.POSIXct(start) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
               stop = as.POSIXct(stop) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))))


      ###############################################################################
      ###################### Activities 2 hours before bedtime ######################
      ###############################################################################

      sleep.timing.duration0 = actlog %>%
        filter(primary.sleep == "primary sleep") %>%
        inner_join(actsum, by = "tucaseid") %>%
        mutate(duration = as.numeric(stop - start, units = "mins")) %>%
        select(tucaseid, tesex, teage, age, days, trcode, duration, start, stop) %>%
        group_by(tucaseid) %>%
        filter(start == max(start)) %>%
        mutate(year = s.year) %>%
        ungroup()
    }
stopCluster(cl)
# write.csv(sleep.timing.duration%>%
#             mutate(start = start -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))) %>%
#             arrange(tucaseid),
#           "D:/galit/9th Report_sleep timing and duration/sleep.timing.duration.csv")

sleep.timing.duration = read.csv("D:/galit/9th Report_sleep timing and duration/sleep.timing.duration.csv") %>%
  mutate(start = as.POSIXct(start) +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))))


# Histogram: Sleep Duration in different year
for (s.year in 2003:2018) {
  p1 = sleep.timing.duration %>% filter(year == s.year) %>%
    ggplot(aes(x = duration/60)) +
    geom_histogram(stat = "bin", binwidth = 0.5, color = "black", fill = "steelblue") +
    scale_x_continuous(breaks = seq(0, 24, 1)) +
    labs(x = "Duration (hours)", y = "Number of People", 
         title = paste0("Histogram: Sleep Duration in ", s.year)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplotly(p1)
}


# Histogram: Sleep Timing in different year
for (s.year in 2003:2018) {
  p2 = sleep.timing.duration %>% filter(year == s.year) %>%
    ggplot(aes(x = start)) +
    geom_histogram(stat = "bin", binwidth = 0.5*60*60, color = "black", fill = "steelblue") +
    labs(x = "Sleep Timing", y = "Number of People", 
         title = paste0("Histogram: Sleep Timing in ", s.year)) +
    scale_x_datetime(breaks = date_breaks("1 hour"),
                     date_minor_breaks = "1 hour",
                     labels = date_format("%H:%M:%S")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplotly(p2)
}
