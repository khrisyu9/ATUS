###############################################################################
################### Collect the other demographic information #################
###############################################################################

library(doParallel)
library(foreach)
# Setup cores cluster using doParallel
ncores = 4 # detectCores() - 1

cl = makeCluster(ncores)
registerDoParallel(cl)
other.factor =
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
      setwd(file)
      if(s.year < 2006){
      actsum = read.csv("actsum.csv") %>%
        mutate(year = s.year) %>%
        select(year, tucaseid, teage, tesex, tudiaryday, trholiday,
               t010101, tu06fwgt, peeduca, trchildnum,
               telfs, trsppres, pehspnon) %>%
        mutate(days = ifelse(tudiaryday %in% c("Sunday", "Monday", "Tuesday",
                                               "Wednesday", "Thursday"), "Weekday", "Weekend"),
               days = ifelse(trholiday == "Diary day was a holiday", "Holiday", days),
               age = ifelse(teage>22,'2','1'),
               age = ifelse(teage>30,'3',age),
               age = ifelse(teage>50,'4',age),
               age = ifelse(teage>64,'5',age),
               age.c = ifelse(teage>22,'23 - 30','15 - 22'),
               age.c = ifelse(teage>30,'31 - 50',age.c),
               age.c = ifelse(teage>50,'51 - 64',age.c),
               age.c = ifelse(teage>64,'65 +',age.c),
               final.weight = tu06fwgt,
               education = ifelse(peeduca %in% c("Doctoral degree (PhD, EdD, etc.) ",
                                                 "Master's degree (MA, MS, MEng, MEd, MSW, etc.)"),
                                  "Master's degree or higher", "Less than high school"),
               education = ifelse(peeduca %in% c("Bachelor's degree (BA, AB, BS, etc.)",
                                                 "Professional school degree (MD, DDS, DVM, etc.)",
                                                 "Associate degree - academic program",
                                                 "Associate degree - occupational/vocational"),
                                  "College graduate", education),
               education = ifelse(peeduca %in% c("High school graduate - diploma or equivalent (GED)",
                                                 "Some college but no degree"),
                                  "High school graduate", education),
               education = as.factor(education)) %>%
        select(-tu06fwgt)
      }else{
        actsum = read.csv("actsum.csv") %>%
          mutate(year = s.year) %>%
          select(year, tucaseid, teage, tesex, tudiaryday, trholiday,
                 t010101, tufinlwgt, peeduca, trchildnum,
                 telfs, trsppres, pehspnon) %>%
          mutate(days = ifelse(tudiaryday %in% c("Sunday", "Monday", "Tuesday",
                                                 "Wednesday", "Thursday"), "Weekday", "Weekend"),
                 days = ifelse(trholiday == "Diary day was a holiday", "Holiday", days),
                 age = ifelse(teage>22,'2','1'),
                 age = ifelse(teage>30,'3',age),
                 age = ifelse(teage>50,'4',age),
                 age = ifelse(teage>64,'5',age),
                 age.c = ifelse(teage>22,'23 - 30','15 - 22'),
                 age.c = ifelse(teage>30,'31 - 50',age.c),
                 age.c = ifelse(teage>50,'51 - 64',age.c),
                 age.c = ifelse(teage>64,'65 +',age.c),
                 final.weight =tufinlwgt,
                 education = ifelse(peeduca %in% c("Doctoral degree (PhD, EdD, etc.)",
                                                   "Master's degree (MA, MS, MEng, MEd, MSW, etc.)"),
                                    "Master's degree or higher", "Less than high school"),
                 education = ifelse(peeduca %in% c("Bachelor's degree (BA, AB, BS, etc.)",
                                                   "Professional school degree (MD, DDS, DVM, etc.)",
                                                   "Associate degree - academic program",
                                                   "Associate degree - occupational/vocational"),
                                    "College graduate", education),
                 education = ifelse(peeduca %in% c("High school graduate - diploma or equivalent (GED)",
                                                   "Some college but no degree"),
                                    "High school graduate", education),
                 education = as.factor(education)) %>%
          select(-tufinlwgt)
      }
      
      actcps = read.csv("cps.csv") %>% select(tucaseid, gestfips, ptdtrace, hrmonth, hryear4) %>%
        filter(tucaseid %in% actsum$tucaseid) %>% 
        group_by(tucaseid) %>%
        summarise(gestfips = gestfips[1],
                  ptdtrace = ptdtrace[1],
                  hrmonth = hrmonth[1],
                  hryear4 = hryear4[1])
      
      
      actsum %>% left_join(actcps, by = "tucaseid")
      
    }
stopCluster(cl)
# write.csv(other.factor, "D:/galit/12th Sleep Duration/other_factor.csv")
