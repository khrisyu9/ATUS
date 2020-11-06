library(dplyr)
library(tidyverse)
library(data.table)
library(doParallel)
library(foreach)
# Setup cores cluster using doParallel
ncores = 4 # detectCores() - 1

cl = makeCluster(ncores)
registerDoParallel(cl)
WASO =
  foreach(
    s.year = 2003:2018, .combine = "rbind", .packages = "dplyr") %dopar% {
  # Original time in r: as.numeric(as.POSIXct("1969-12-31 19:00:00"))
  # today's 00:00:00 -- as.numeric(as.POSIXct(paste(Sys.Date(), "00:00:00")))
  file = sprintf("d:/galit/%s",paste0(s.year,"atus"))
  setwd(file)
  
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
  
  
  # primary.sleep = read.csv("D:/galit/10th Report_Three Model/primary_sleep_timing and duration.csv") %>%
  primary.sleep = read.csv("D:/galit/18th Debug/primary_sleep_timing and duration.debug.csv") %>%
    mutate(start = as.POSIXct(start),
           stop = as.POSIXct(stop),
           start_sleep = start +
             as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
           stop_sleep = stop +
             as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))) %>%
    select(tucaseid, start_sleep, stop_sleep)
  
  
  actlog = actlog %>% 
    left_join(primary.sleep, by = "tucaseid") %>%
    filter(start > start_sleep, stop < stop_sleep, trcode != 10101) %>%
    mutate(gap_duration = as.numeric(stop - start, units = "mins"))
  
  actlog
}
stopCluster(cl)
# write.csv(WASO %>%
#             mutate(start = start -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                    stop = stop -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                    start_sleep = start_sleep -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                    stop_sleep = stop_sleep -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))) %>%
#             arrange(tucaseid),"D:\\galit\\17th Gap outliers + Weight adjustment\\Gap_activity.debug.csv")

# write.csv(WASO %>%
#             mutate(start = start -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                    stop = stop -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                    start_sleep = start_sleep -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#                    stop_sleep = stop_sleep -
#                      as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")))) %>%
#             arrange(tucaseid),"D:\\galit\\18th Debug\\Gap_activity.debug.csv")


# [1] 20030302031463 20030606031684 20030908032325 20040201041141 20040504040542 20040909042363 20051010051881 20070212061891
# [9] 20071010070585 20100202102524 20100504101333 20160403162257 20170201171749 20170604172381 20170706170605 20171211170646
# [1] 145 135 130 125 125 156 152 180 190 180 121 130 150 125 135 122

WASO = read.csv("D:\\galit\\17th Gap outliers + Weight adjustment\\Gap_activity.csv") %>%
  mutate(start = as.POSIXct(start),
         stop = as.POSIXct(stop),
         start_sleep = as.POSIXct(start_sleep),
         stop_sleep = as.POSIXct(stop_sleep),
         start = start +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         stop = stop +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         duration = as.numeric(stop - start, units = "mins"),
         start_sleep = start_sleep +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         stop_sleep = stop_sleep +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         sleep_duration = as.numeric(stop_sleep - start_sleep,
                                     units = "mins"))

other.factor = read.csv("D:/galit/12th Sleep Duration/other_factor.csv") %>%
  left_join(state.info%>%select(gestfips,longitude), by = "gestfips") %>%
  mutate(education.new = ifelse(education %in% c("College graduate",
                                                 "Master's degree or higher"),
                                "College graduate",
                                "Less than high school"),
         education.new = ifelse(education == "High school graduate",
                                "High school graduate", education.new),
         education.new = as.factor(education.new)) %>%
  select(tucaseid, tesex, teage, tudiaryday, days, age.c, education.new, final.weight)


WASO_MATCH = WASO %>% left_join(other.factor, by = "tucaseid") %>%
  filter(days == "Weekday") %>%
  filter(start_sleep > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S"))) %>%
  group_by(tucaseid) %>%
  mutate(gap_total = sum(gap_duration)) %>%
  filter(gap_total >= 30)

###############################################################################
##################### Sleep Duration for WASO people ##########################
###############################################################################
WASO_MATCH %>% group_by(tucaseid) %>%
  summarise(sleep_duration = sleep_duration[1]/60) %>%
  # filter(sleep_duration < 900) %>%
  mutate() %>%
  ggplot() + 
  geom_histogram(aes(x = sleep_duration), 
                 binwidth = 0.5, color = "black", fill = "lightblue") +
  geom_vline(aes(xintercept = quantile(sleep_duration, 0.5)), color = "red") +
  geom_text(aes(x = quantile(sleep_duration, 0.5), y = 100,
                label = quantile(sleep_duration, 0.5)), color = "red") +
  geom_vline(aes(xintercept = quantile(sleep_duration, c(0.25))), color = "blue") +
  geom_text(aes(x = quantile(sleep_duration, 0.25), y = 200,
                label = quantile(sleep_duration, 0.25)), color = "blue") +
  geom_vline(aes(xintercept = quantile(sleep_duration, c(0.75))), color = "blue") +
  geom_text(aes(x = quantile(sleep_duration, 0.75), y = 200,
                label = quantile(sleep_duration, 0.75)), color = "blue") +
  labs(x = "Sleep Duration", y = "Frequency", 
       title = "Distribution of sleep duration for respondents who have WASO") +
  theme(plot.title = element_text(hjust = 0.5))

summary(WASO_MATCH %>% group_by(tucaseid) %>%
          summarise(sleep_duration = sleep_duration[1]/60))

###############################################################################
################ What are they doing during their WASO? #######################
###############################################################################
q2.3 = WASO_MATCH %>% group_by(tucaseid, trcode, final.weight) %>%
  summarise(gap_duration = sum(gap_duration)) %>%
  ungroup() %>% select(tucaseid, trcode, gap_duration, final.weight) %>%
  spread(trcode, gap_duration) %>%
  replace(.,is.na(.),0) %>%
  gather(key = "trcode", value = gap_duration, -c("tucaseid", "final.weight")) %>%
  mutate(pop = ifelse(gap_duration>0, 1, 0))

q2.3.svy = svydesign(ids =~tucaseid, data = q2.3,
                     weights =~final.weight)

q2.3.result = svyby(formula=~pop, by=~trcode,
                    design = q2.3.svy, FUN = svymean, 
                    vartype = c("se"))

act_code = read.csv("D:/galit/3rd Report_act 2 hours bef/code_activity.csv")
codes = act_code[,1]
activities = act_code[,2]

q2.3.result = q2.3.result %>%
  filter(pop > sort(pop)[length(pop)-20]) %>%
  mutate(Activity = as.character(factor(trcode, codes, activities))) %>%
  select(Activity, trcode, pop, se) %>% arrange(-pop) %>%
  ungroup()

# q2.3.result = q2.3.result %>%
#   filter(gap_duration > sort(gap_duration)[length(gap_duration)-20]) %>%
#   mutate(Activity = as.character(factor(trcode, codes, activities))) %>%
#   select(Activity, trcode, gap_duration, se) %>% arrange(-gap_duration) %>%
#   ungroup()

q2.3.result

p2.3 = plot_ly(data = q2.3.result, x = ~Activity, y = ~gap_duration, type = 'bar', name = 'OJ',
               error_y = ~list(array = se,
                               color = '#000000')) %>%
  layout(xaxis = list(title = 'Activity', categoryarray = ~Activity, 
                      categoryorder = "duration"),
         yaxis = list(title = 'Duration (mins)'),
         title = 'Top 20 Gap Activities between Sleep in 2018')
p2.3


###############################################################################
######### How many people do have a continuous gap >= 30 mins? ################
###############################################################################

a = WASO %>% left_join(other.factor, by = "tucaseid") %>%
  filter(days == "Weekday") %>%
  filter(start_sleep > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S"))) %>%
  group_by(tucaseid) %>%
  filter(gap_duration >= 30) %>%
  summarise(gap_ind = 1) %>%
  ungroup()

sleeplessness = WASO %>% left_join(other.factor, by = "tucaseid") %>%
  filter(days == "Weekday") %>%
  filter(start_sleep > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S"))) %>%
  group_by(tucaseid) %>%
  filter(gap_duration >= 30, trcode == 10102)

length(unique(sleeplessness$tucaseid))
# 2054 people have 
id = unique(a$tucaseid)


model1.data = model1.data %>% ungroup() %>%
  left_join(a, by = "tucaseid") %>%
  replace(.,is.na(.),0)

# model1.data.male = model1.data %>% filter(tesex == "Male")
# model1.data.female = model1.data %>% filter(tesex == "Female")
print("Overall Adjust")
model3.fragment.overall.svy = svydesign(ids = ~tucaseid,
                                        weights = ~final.weight.b,
                                        data = model1.data)

model3.fragment.overall.adjusted = svyglm(gap_ind ~ as.character(year)  + 
                                            tudiaryday + race + eating_ind + (trchildnum>0) +trsppres +
                                            age.c + employment + education.new,
                                          design = model3.fragment.overall.svy, family = binomial)

a = odds.ratio(model3.fragment.overall.adjusted)
1/a['eating_indNo Eat',1:3]
print("Overall crude")
model3.fragment.overall.crude = svyglm(gap_ind ~ eating_ind,
                                       design = model3.fragment.overall.svy, family = binomial)

a = odds.ratio(model3.fragment.overall.crude)
1/a['eating_indNo Eat',1:3]


###############################################################################
######################## Fragmentation Female ###################################
###############################################################################

print("Female Adjust")
model3.fragment.female.svy = svydesign(ids = ~tucaseid,
                                       weights = ~final.weight.b,
                                       data = model1.data.female)

model3.fragment.female.adjusted = svyglm(gap_ind ~ as.character(year)  + 
                                           tudiaryday + race + eating_ind + (trchildnum>0) +trsppres +
                                           age.c + employment + education.new,
                                         design = model3.fragment.female.svy,
                                         family = binomial)

a = odds.ratio(model3.fragment.female.adjusted)
1/a['eating_indNo Eat',1:3]
print("Female crude")
model3.fragment.female.crude = svyglm(gap_ind ~ eating_ind,
                                      design = model3.fragment.female.svy, family = binomial)

a = odds.ratio(model3.fragment.female.crude)
1/a['eating_indNo Eat',1:3]

###############################################################################
######################## Fragmentation Male ###################################
###############################################################################

print("Male Adjust")
model3.fragment.male.svy = svydesign(ids = ~tucaseid,
                                     weights = ~final.weight.b,
                                     data = model1.data.male)

model3.fragment.male.adjusted = svyglm(gap_ind ~ as.character(year) + 
                                         tudiaryday + race + eating_ind + (trchildnum>0) +trsppres +
                                         age.c + employment + education.new,
                                       design = model3.fragment.male.svy, family = binomial)

a = odds.ratio(model3.fragment.male.adjusted)
1/a['eating_indNo Eat',1:3]
print("Male crude")
model3.fragment.male.crude = svyglm(gap_ind ~ eating_ind,
                                    design = model3.fragment.male.svy, family = binomial)

a = odds.ratio(model3.fragment.male.crude)
1/a['eating_indNo Eat',1:3]

library(diagis)
library(extrafont)
font_import()
loadfonts(device="win")


model1.data %>%
  group_by(tesex, age.c) %>%
  summarise(pop = weighted.mean(gap_ind, final.weight.b),
            pop_se = weighted_se(gap_ind, 
                                 final.weight.b)) %>%
  ggplot(aes(x = age.c, group = tesex, color = tesex)) +
  geom_line(aes(y = pop), size = 1) +
  geom_point(aes(y = pop, shape = tesex), show.legend = F, size = 3) +
  labs(x = "Age Group", y = "Proportion") + 
  scale_color_manual(name = "Gender", 
                     values = c("Male" = "darkgray", 
                                "Female" = "black")) +
  scale_shape_manual(values = c("Male" = 1, "Female" = 16)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                     limits = c(0.001, 0.026),
                     breaks = seq(0.001,0.026,0.005)) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.8),
        # rect = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        # axis.title.x = element_text(size = 13),
        # axis.title.y = element_text(size = 13),
        # legend.text = element_text(size = 10),
        # legend.title = element_text(size = 13),
        text=element_text(family="Times New Roman", face="bold", size=12))




sd(model1.data$final.weight.b)/mean(model1.data$final.weight.b)

###############################################################################
################### Remove Children care/medical care #########################
###############################################################################
model1.data.svy = svydesign(ids = ~tucaseid,
                            weights = ~final.weight.b,
                            data = model1.data)
model1.data.svy.sub = subset(model1.data.svy, primary.sleep=="primary sleep")
model1.data.svy.sub = subset(model1.data.svy.sub, start > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S")))
model1.data.svy.sub = subset(model1.data.svy.sub, days == "Weekday")

# Fragmentation Overall
print("Overall Adjust")
model3.fragment.overall.adjusted = svyglm(gap_remove_child ~ as.character(year)  + 
                                            tudiaryday + race + eating_ind + (trchildnum>0) +trsppres +
                                            age.c + employment + education.new,
                                          design = model1.data.svy.sub, family = binomial)

a = odds.ratio(model3.fragment.overall.adjusted)
1/a['eating_indNo Eat',1:3]
print("Overall crude")
model3.fragment.overall.crude = svyglm(gap_remove_child ~ eating_ind,
                                       design = model1.data.svy.sub, family = binomial)

a = odds.ratio(model3.fragment.overall.crude)
1/a['eating_indNo Eat',1:3]

# Fragmentation Female 
model1.data.svy.female = subset(model1.data.svy.sub, tesex == "Female")
print("Female Adjust")
#model1.data.svy.female
model3.fragment.female.adjusted = svyglm(gap_remove_child ~ as.character(year)  + 
                                           tudiaryday + race + eating_ind + (trchildnum>0) +trsppres +
                                           age.c + employment + education.new,
                                         design = model1.data.svy.female,
                                         family = binomial)

a = odds.ratio(model3.fragment.female.adjusted)
1/a['eating_indNo Eat',1:3]
print("Female crude")
model3.fragment.female.crude = svyglm(gap_remove_child ~ eating_ind,
                                      design = model1.data.svy.female, family = binomial)

a = odds.ratio(model3.fragment.female.crude)
1/a['eating_indNo Eat',1:3]

# Fragmentation Male
model1.data.svy.male = subset(model1.data.svy.sub, tesex == "Male")
print("Male Adjust")
# model1.data.svy.male
model3.fragment.male.adjusted = svyglm(gap_remove_child ~ as.character(year)  + 
                                         tudiaryday + race + eating_ind + (trchildnum>0) +trsppres +
                                         age.c + employment + education.new,
                                       design = model1.data.svy.male, family = binomial)

a = odds.ratio(model3.fragment.male.adjusted)
1/a['eating_indNo Eat',1:3]
print("Male crude")
model3.fragment.male.crude = svyglm(gap_remove_child ~ eating_ind,
                                    design = model1.data.svy.male, family = binomial)

a = odds.ratio(model3.fragment.male.crude)
1/a['eating_indNo Eat',1:3]


