###############################################################################
################## Collect the activities before primary sleep ################
###############################################################################

library(doParallel)
library(foreach)
# Setup cores cluster using doParallel
ncores = 4 # detectCores() - 1

cl = makeCluster(ncores)
registerDoParallel(cl)
act_before_sleep =
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
        select(tucaseid, tesex, teage, age, days, tudiaryday, tufinlwgt)
      
      actlog = read.csv("actlog.csv") %>%
        mutate(year = s.year,
               trcode = as.numeric(paste0(tutier1code,
                                          paste0(ifelse(tutier2code<10,
                                                        paste0(0,tutier2code),
                                                        tutier3code),
                                                 ifelse(tutier3code<10,
                                                        paste0(0,tutier3code),
                                                        tutier3code))))) %>%
        # filter(trcode == 10101) %>%
        mutate(start = as.POSIXct(strptime(tustarttim, format="%H:%M:%S")),
               stop = as.POSIXct(strptime(tustoptime, format="%H:%M:%S"))) %>%
        select(year, tucaseid, tuactivity_n, trcode, start, stop, tucumdur24, tucumdur)
      # Setting start and end time:
      actlog = actlog %>%
        mutate(index_1 = ifelse(start<stop, 0, 1),
               index_2 = ifelse(tuactivity_n != 1 &
                                  start < as.POSIXct(strptime("04:00:00",format = "%H:%M:%S")),
                                1, 0)) %>%
        group_by(tucaseid) %>%
        mutate(last = ifelse(tuactivity_n == max(tuactivity_n), 1, 0))
      
      actlog[actlog$index_1 == 1,]$stop = actlog[actlog$index_1 == 1,]$stop + 24*60*60
      actlog[actlog$index_2 == 1,]$start = actlog[actlog$index_2 == 1,]$start + 24*60*60
      actlog[actlog$index_2 == 1,]$stop = actlog[actlog$index_2 == 1,]$stop + 24*60*60
      
      
      actcps = read.csv("cps.csv") %>% select(tucaseid, gestfips, ptdtrace, hrmonth, hryear4) %>%
        filter(tucaseid %in% actsum$tucaseid) %>% 
        group_by(tucaseid) %>%
        summarise(gestfips = gestfips[1],
                  ptdtrace = ptdtrace[1],
                  hrmonth = hrmonth[1],
                  hryear4 = hryear4[1])
      
      file = sprintf("D:/galit/7th Report_selection compare 2 and act 2 hours bef/act_2_and_primary_sleep.Galit.%s",
                     paste0(s.year,".csv"))
      actlog_new = read.csv(file) %>%
        mutate(start = as.POSIXct(start) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
               stop = as.POSIXct(stop) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))))
      
      id_selected = unique(actlog_new$tucaseid)
      ###############################################################################
      ###################### Activities before primary sleep ########################
      ###############################################################################
      primary.start = actlog_new %>% group_by(tucaseid) %>%
        filter(stop == max(stop)) %>%
        mutate(primary.start = start) %>%
        select(tucaseid,primary.start)
      
      need.data = actlog %>% filter(tucaseid %in% id_selected)%>% 
        left_join(primary.start, by = "tucaseid") %>%
        group_by(tucaseid) %>%
        filter(stop <= primary.start) %>%
        select(year, tucaseid, tuactivity_n, start, stop, trcode) %>%
        left_join(actsum, by = "tucaseid") %>%
        left_join(actcps, by = "tucaseid") %>%
        mutate(start = start -
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
               stop = stop -
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))))
      need.data
    }
stopCluster(cl)

# write.csv(act_before_sleep, "D:/galit/10th Report_Three Model/all_act_before_sleep.csv")



###############################################################################
######### Calculate primary sleep duration, #gap and gap duration #############
###############################################################################
cl = makeCluster(ncores)
registerDoParallel(cl)
primary.sleep =
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
      
      # This data set was saved by 9th:sleep timing and duration.R
      # It solved two problems:
      # 1) It continues the other sleep after primary sleep 
      #    if that sleep not more than 2 hours after
      # 2) make a cut-off at 2 hours before bed
      file = sprintf("D:/galit/18th Debug/act_2_and_primary_sleep.Galit.debug.%s",
                     paste0(s.year,".csv"))
      actlog = read.csv(file) %>%
        mutate(start = as.POSIXct(start) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
               stop = as.POSIXct(stop) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))))
      
      # There are a little bug in this dataset "act_2_bef_sleep.Galit.year.csv", 
      # whenever use it, please do the clearing step as below:
      id_noclean = actlog %>% 
        filter(primary.sleep.number>0) %>%
        group_by(tucaseid) %>%
        summarise(n=n()) %>%
        filter(n>1)
      
      clean_it = actlog %>% 
        filter(tucaseid %in% id_noclean$tucaseid) %>%
        filter(primary.sleep.number>0) %>% group_by(tucaseid) %>%
        filter(start == min(start)) %>% ungroup()
      
      actlog %>% 
        filter(!(tucaseid %in% id_noclean$tucaseid)) %>%
        filter(primary.sleep.number>0) %>%
        ungroup() %>%
        rbind(clean_it)
      
    }
stopCluster(cl)
# write.csv(primary.sleep %>%
#             mutate(start = start -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                    stop = stop -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))),
#           "D:/galit/7th Report_selection compare 2 and act 2 hours bef/primary_sleep_timing and duration.csv")

write.csv(primary.sleep %>%
            mutate(start = start -
                     as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
                   stop = stop -
                     as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))),
          "D:/galit/18th Debug/primary_sleep_timing and duration.debug.csv")


###############################################################################
################## Get Sleep Incontinuity: Gap Duration and Num ###############
###############################################################################


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
# 20090112081291 20100807100987
options(scipen=999)
gap_total = data.frame(tucaseid = 0, gap_duration = 0, gap_num = 0, year = 0)
##############################################################################
# Original time in r: as.numeric(as.POSIXct("1969-12-31 19:00:00"))
# today's 00:00:00 -- as.numeric(as.POSIXct(paste(Sys.Date(), "00:00:00")))
for (s.year in 2003:2018) {
  setwd(sprintf("D:/galit/%satus/",s.year))
  actlog = read.csv("actlog.csv") %>%
    mutate(year = s.year,
           trcode = as.numeric(paste0(tutier1code,
                                      paste0(ifelse(tutier2code<10,
                                                    paste0(0,tutier2code),
                                                    tutier2code),
                                             ifelse(tutier3code<10,
                                                    paste0(0,tutier3code),
                                                    tutier3code))))) %>%
    mutate(start = as.POSIXct(strptime(tustarttim, format="%H:%M:%S")),
           stop = as.POSIXct(strptime(tustoptime, format="%H:%M:%S"))) %>%
    select(tucaseid, tuactivity_n, trcode, start, stop) %>%
    group_by(tucaseid) %>%
    mutate(last = ifelse(tuactivity_n == max(tuactivity_n),1,0)) %>%
    ungroup()
  
  # Setting start and end time:
  actlog = actlog %>%
    mutate(index_1 = ifelse(start<stop, 0, 1),
           index_2 = ifelse(tuactivity_n != 1 &
                              start < as.POSIXct(strptime("04:00:00",format = "%H:%M:%S")),
                            1, 0)) %>%
    group_by(tucaseid) %>%
    mutate(last = ifelse(tuactivity_n == max(tuactivity_n), 1, 0))
  
  actlog[actlog$index_1 == 1,]$stop = actlog[actlog$index_1 == 1,]$stop + 24*60*60
  actlog[actlog$index_2 == 1,]$start = actlog[actlog$index_2 == 1,]$start + 24*60*60
  actlog[actlog$index_2 == 1,]$stop = actlog[actlog$index_2 == 1,]$stop + 24*60*60
  
  file.name = sprintf("D:/galit/18th Debug/act_2_and_primary_sleep.Galit.debug.%s.csv",s.year)
  g.actlog = data.table(read.csv(file.name) %>%
                          mutate(begin = as.POSIXct(start) +
                                   as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
                                 end = as.POSIXct(stop) +
                                   as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))))) %>%
    filter(primary.sleep == "primary sleep") %>% 
    select(tucaseid, begin, end)
  
  gap_people = g.actlog %>%
    left_join(actlog, by = "tucaseid") %>%
    filter(start > begin, stop < end, trcode != 10101) %>%
    mutate(gap_duration = as.numeric(stop - start, units = "mins")) %>%
    group_by(tucaseid) %>%
    summarise(gap_duration = sum(gap_duration), gap_num = n(), year = s.year)
  
  gap_total = rbind(gap_total,gap_people)
}
gap_total = gap_total[-1,]
# write.csv(gap_total, "D:/galit/10th Report_Three Model/gap_total.csv")
write.csv(gap_total, "D:/galit/18th Debug/gap_total.debug.csv")


###############################################################################
############## Collect the activities 2 hours before primary sleep #############
###############################################################################
library(dplyr)
library(doParallel)
library(foreach)
# Setup cores cluster using doParallel
ncores = 4 # detectCores() - 1

cl = makeCluster(ncores)
registerDoParallel(cl)
act_before_sleep =
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
      
      file = sprintf("D:/ATUS/Processed_Data/act_2_and_primary_sleep.Galit.debug.%s",
                     paste0(s.year,".csv"))
      actlog_new = read.csv(file) %>%
        mutate(start = as.POSIXct(start) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
               stop = as.POSIXct(stop) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))))
      
      ###############################################################################
      ###################### Activities before primary sleep ########################
      ###############################################################################
      actlog_new = actlog_new %>% filter(primary.sleep == "other sleep") %>% 
        group_by(tucaseid) %>%
        mutate(sleep_timing = max(stop)) %>%
        # get activities 2-1 hour before bed
        filter(stop > sleep_timing - 2*60*60) %>%
        mutate(duration = ifelse(start < sleep_timing - 2*60*60, 
                                 as.numeric(stop - sleep_timing + 2*60*60, units = "mins"),
                                 as.numeric(stop - start, units = "mins")))
      
      
      actlog_new %>% mutate(year = s.year) %>%
        group_by(year,tucaseid,trcode) %>%
        summarise(duration = sum(duration)) %>%
        spread(trcode,duration) %>%
        replace(.,is.na(.),0)
      
    }
stopCluster(cl)
act_before_sleep2 = act_before_sleep%>%replace(.,is.na(.),0)
# write.csv(act_before_sleep1, "D:/galit/11th Three Model improve/act_duration.1hr.csv")
write.csv(act_before_sleep2, "D:/ATUS/Processed_Data/act_duration.2hr.debug.csv")


###############################################################################
###################### Activities 2-1 hour before primary sleep ########################
###############################################################################
actlog_new = actlog_new %>% filter(primary.sleep == "other sleep") %>% 
  group_by(tucaseid) %>%
  mutate(sleep_timing = max(stop)) %>%
  # get activities two hours to one hour before bed
  filter(stop > sleep_timing - 2*60*60) %>%
  mutate(duration = ifelse(start < sleep_timing - 2*60*60,
                           as.numeric(stop - sleep_timing + 2*60*60, units = "mins"),
                           as.numeric(stop - start, units = "mins")))


actlog_new %>% mutate(year = s.year) %>%
  group_by(year,tucaseid,trcode) %>%
  summarise(duration = sum(duration)) %>%
  spread(trcode,duration) %>%
  replace(.,is.na(.),0)

    }
stopCluster(cl)
act_before_sleep1 = act_before_sleep%>%replace(.,is.na(.),0)
# write.csv(act_before_sleep1, "D:/galit/11th Three Model improve/act_duration.1hr.csv")
write.csv(act_before_sleep1, "D:/galit/18th Debug/act_duration.2hr.debug.csv")





###############################################################################
############## Collect Exercise Timing of Primary Sleep Respodants ##############
###############################################################################

library(doParallel)
library(foreach)
# Setup cores cluster using doParallel
ncores = 4 # detectCores() - 1

cl = makeCluster(ncores)
registerDoParallel(cl)
eat_timing =
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
      
      setwd(sprintf("D:/galit/%satus/",s.year))
      actlog = read.csv("actlog.csv") %>%
        mutate(year = s.year,
               trcode = as.numeric(paste0(tutier1code,
                                          paste0(ifelse(tutier2code<10,
                                                        paste0(0,tutier2code),
                                                        tutier2code),
                                                 ifelse(tutier3code<10,
                                                        paste0(0,tutier3code),
                                                        tutier3code))))) %>%
        mutate(start = as.POSIXct(strptime(tustarttim, format="%H:%M:%S")),
               stop = as.POSIXct(strptime(tustoptime, format="%H:%M:%S"))) %>%
        select(tucaseid, tuactivity_n, trcode, start, stop) %>%
        group_by(tucaseid) %>%
        mutate(last = ifelse(tuactivity_n == max(tuactivity_n),1,0)) %>%
        ungroup()
      
      # Setting start and end time:
      actlog = actlog %>%
        mutate(index_1 = ifelse(start<stop, 0, 1),
               index_2 = ifelse(tuactivity_n != 1 &
                                  start < as.POSIXct(strptime("04:00:00",format = "%H:%M:%S")),
                                1, 0)) %>%
        group_by(tucaseid) %>%
        mutate(last = ifelse(tuactivity_n == max(tuactivity_n), 1, 0))
      
      actlog[actlog$index_1 == 1,]$stop = actlog[actlog$index_1 == 1,]$stop + 24*60*60
      actlog[actlog$index_2 == 1,]$start = actlog[actlog$index_2 == 1,]$start + 24*60*60
      actlog[actlog$index_2 == 1,]$stop = actlog[actlog$index_2 == 1,]$stop + 24*60*60
      
      
      file = sprintf("D:/galit/18th Debug/act_2_and_primary_sleep.Galit.debug.%s",
                     paste0(s.year,".csv"))
      actlog_new = read.csv(file) %>%
        filter(primary.sleep == "primary sleep") %>%
        mutate(primary.begin = as.POSIXct(start) +
                 as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))) %>%
        select(tucaseid, primary.begin)
      
      actlog_new %>% left_join(actlog, by="tucaseid") %>%
        filter(start < primary.begin, trcode == 110101) %>%
        group_by(tucaseid) %>%
        filter(stop == max(stop)) %>%
        select(tucaseid, primary.begin, start, stop, tuactivity_n)
      
    }
stopCluster(cl)

# write.csv(eat_timing %>% mutate(primary.begin = primary.begin -
#                                   as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                                 start = start -
#                                   as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                                 stop = stop -
#                                   as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))), 
#           "D:/galit/18th Debug/Eating_Timing.csv")

