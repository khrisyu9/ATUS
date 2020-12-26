library(dplyr)
library(ggplot2)
library(data.table)
library(GGally)
library(gridExtra)
library(grid)
library(latex2exp)
library(mice)
library(weights)
library(scales)
library(stats)
library(knitr)
library(survey)
library(splines)
library(plotly)
library(quantreg)

setwd("D:/ATUS/Processed_Data/")

other.factor = read.csv("other_factor.csv") %>%
  mutate(education.new = ifelse(education %in% c("College graduate",
                                                 "Master's degree or higher"),
                                "College graduate",
                                "Less than high school"),
         education.new = ifelse(education == "High school graduate",
                                "High school graduate", education.new),
         education.new = as.factor(education.new))

primary.sleep = read.csv("primary_sleep_timing and duration.debug.csv") %>%
  mutate(start = as.POSIXct(start),
         stop = as.POSIXct(stop),
         start = start +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         stop = stop +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         duration = as.numeric(stop - start, units = "mins"),
         starttime = as.numeric(start - (as.POSIXct("2020-12-24 00:00:00")), units = "mins"))

gap_total = read.csv("gap_total.debug.csv") %>%
  select(tucaseid, gap_duration, gap_num)

exer_dat = read.csv("all_exercise.csv")
exer_dat = exer_dat %>%
  mutate(start = as.POSIXct(paste("1970-01-01", tustarttim), format="%Y-%m-%d %H:%M:%S"),
         stop = as.POSIXct(paste("1970-01-01", tustoptime), format="%Y-%m-%d %H:%M:%S"),
         start = start +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         stop = stop +
           as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
         exer_duration = as.numeric(stop - start, units = "mins"))
exer_dat = exer_dat %>%
  filter(exer_duration > 0) %>%
  mutate(exer_duration90 = ifelse(exer_duration > 90, 90, exer_duration))

exer_dat = exer_dat[,c(1:5,8,11,12)]
  
length(exer_dat$tucaseid)

# exercise duration distribution
length(which(exer_dat$exer_duration < 30))
length(which(exer_dat$exer_duration < 60 & exer_dat$exer_duration >= 30))
length(which(exer_dat$exer_duration < 90 & exer_dat$exer_duration >= 60))
length(which(exer_dat$exer_duration < 120 & exer_dat$exer_duration >= 90))
length(which(exer_dat$exer_duration >= 120))

#predictors = as.data.frame(exer_dat %>%
#  select(tucaseid, exer_time))
#################priliminary results####################
#model1=merge(primary.sleep, exer_dat, by = "tucaseid", all.x = TRUE)
#model1$exer_time = as.factor(model1$exer_time)
#model1_dur_mo=glm(duration ~ (exer_time == "morning"),data = model1)
#ci = summary(model1_dur_mo)$coefficient['exer_time == "morning"TRUE',]
#ci[1]
#c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

#model1_dur_af=glm(duration ~ (exer_time == "afternoon"),data = model1)
#ci = summary(model1_dur_af)$coefficient['exer_time == "afternoon"TRUE',]
#ci[1]
#c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

#model1_dur_ev=glm(duration ~ (exer_time == "evening"),data = model1)
#ci = summary(model1_dur_ev)$coefficient['exer_time == "evening"TRUE',]
#ci[1]
#c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

#model1_dur_ev=glm(duration ~ (exer_time == NA),data = model1)
#ci = summary(model1_dur_ev)$coefficient['exer_time == "NA"TRUE',]
#ci[1]
#c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

#model2=merge(primary.sleep, exer_dat, by = "tucaseid", all.x = TRUE)
#model2$exer_time = as.factor(model2$exer_time)
#model2=merge(model2, WASO, by = "tucaseid", all.x = TRUE)
#model2_waso_mo=glm((con_gap_30 > 0) ~ (exer_time == "morning"),
#    data = model2)
#table(model2$exer_time, model2$con_gap_30)
#ci = summary(model1_waso_mo)$coefficient['exer_time == "morning"TRUE',]
#ci[1]
#c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])ci = summary(model1_dur_mo)$coefficient['exer_time == "morning"TRUE',]

#####################

WASO = read.csv("Gap_activity.debug.csv") %>%
  mutate(start = as.POSIXct(start, tryFormats = c("%Y-%m-%d %H:%M:%S")),
         stop = as.POSIXct(stop, tryFormats = c("%Y-%m-%d %H:%M:%S")),
         start_sleep = as.POSIXct(start_sleep, tryFormats = c("%Y-%m-%d %H:%M:%S")),
         stop_sleep = as.POSIXct(stop_sleep, tryFormats = c("%Y-%m-%d %H:%M:%S")),
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
                                     units = "mins")) %>%  
  # mutate(start = as.POSIXct(start, tryFormats = c("%m/%d/%Y %H:%M")),
  #        stop = as.POSIXct(stop, tryFormats = c("%m/%d/%Y %H:%M")),
  #        start_sleep = as.POSIXct(start_sleep, tryFormats = c("%m/%d/%Y %H:%M")),
  #        stop_sleep = as.POSIXct(stop_sleep, tryFormats = c("%m/%d/%Y %H:%M")),
  #        start = start +
  #          as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
  #        stop = stop +
  #          as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
  #        duration = as.numeric(stop - start, units = "mins"),
  #        start_sleep = start_sleep +
  #          as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#        stop_sleep = stop_sleep +
#          as.numeric(as.POSIXct(strptime("19:00:00",format = "%H:%M:%S"))),
#        sleep_duration = as.numeric(stop_sleep - start_sleep,
#                                    units = "mins")) %>%
group_by(tucaseid) %>%
  mutate(con_gap_30 = ifelse(gap_duration >= 30, gap_duration, 0),
         con_gap_30 = max(con_gap_30),
         care_children = ifelse(trcode %in% c(30101, 30301), gap_duration, 0),
         care_children = max(care_children)) %>%
  group_by(tucaseid, con_gap_30, care_children, sleep_duration) %>%
  summarise(gap_max = max(gap_duration),
            gap_duration = sum(gap_duration),
            gap_num = sum(n())) %>%
  select(tucaseid, con_gap_30, care_children, gap_duration, gap_max, 
         gap_num, sleep_duration) %>%
  ungroup()

WASO = as.data.frame(WASO)

##combine data sets together for modeling

model1.data = merge(primary.sleep, exer_dat, by = "tucaseid", all.x = TRUE)
model1.data[is.na(model1.data)]=0

model1.data = model1.data %>% group_by(tucaseid) %>%
  # filter(primary.sleep=="primary sleep") %>%
  summarise(start = start[1],
            starttime = starttime[1],
            duration = duration[1],
            stop = stop[1],
            primary.sleep = primary.sleep[1],
            exer_time = exer_time[1],
            exer_duration = exer_duration[1],
            exer_duration90 = exer_duration90[1]) %>%
  mutate(exer_duration.c = ifelse(exer_duration == 0,
                                  "no_exercise", "others"),
         exer_duration.c = ifelse(exer_duration > 0 & exer_duration < 30,
                                   "less30", exer_duration.c),
         exer_duration.c = ifelse(exer_duration >= 30 & exer_duration < 60,
                                   "between30_60",exer_duration.c),
         exer_duration.c = ifelse(exer_duration >= 60 & exer_duration < 90,
                                  "between60_90",exer_duration.c),
         exer_duration.c = ifelse(exer_duration >= 90 & exer_duration < 120,
                                  "between90_120",exer_duration.c),
         exer_duration.c = ifelse(exer_duration >= 120,
                                  "greater120",exer_duration.c),
         exer_duration.c = as.factor(exer_duration.c)) %>%
  # 1. Primary Sleep after 6 
  # filter(start > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S"))) %>%
  left_join(WASO, by = "tucaseid") %>%
  replace(.,is.na(.),0) %>%
  mutate(sleep_duration = (duration - gap_duration)) %>%
  right_join(other.factor, by = "tucaseid") %>%
  # replace(.,is.na(.),0) %>%
  # 3. People recorded on Weekday
  # filter(days == "Weekday") %>%
  # Generating varaible race
  mutate(race = ifelse(ptdtrace %in% c("White only","White Only"),"White only","Others"),
         race = ifelse(ptdtrace %in% c("Black only","Black Only"),"Black only",race),
         race = ifelse(pehspnon == "Hispanic", "Hispanic", race),
         # race = as.factor(race),
         employment = ifelse(telfs %in% c("Unemployed - on layoff", "Unemployed - looking"),
                             "Unemployed","Employed"),
         employment = ifelse(telfs == "Not in labor force",
                             "Not in labor force",employment),
         # employment = as.factor(employment)
  ) %>%
  ungroup()

model1.data = model1.data %>%
  mutate(final.weight.b = ifelse(final.weight>quantile(final.weight,0.9),
                                 quantile(final.weight,0.9),
                                 final.weight)) %>%
  mutate(sleep_duration.c = ifelse(teage<18 & sleep_duration<8*60,
                                   "short","normal"),
         sleep_duration.c = ifelse(teage<18 & sleep_duration>10*60,
                                   "long",sleep_duration.c),
         sleep_duration.c = ifelse(teage>65 & sleep_duration<7*60,
                                   "short",sleep_duration.c),
         sleep_duration.c = ifelse(teage>65 & sleep_duration>8*60,
                                   "long",sleep_duration.c),
         sleep_duration.c = ifelse(teage>=18 & teage<=64 & sleep_duration<7*60,
                                   "short",sleep_duration.c),
         sleep_duration.c = ifelse(teage>=18 & teage<=64 & sleep_duration>9*60,
                                   "long",sleep_duration.c),
         sleep_duration.c = as.factor(sleep_duration.c)) %>%
  mutate(sleep_efficiency = sleep_duration/duration*100,
         SE = ifelse(sleep_efficiency==100, 1, 0),
         gap = ifelse(sleep_efficiency==100, 0, 1))

model1.data = model1.data %>% 
  mutate(gap_20 = ifelse(gap_duration<20,"less 20","larger and equal 20"),
         gap_20 = ifelse(gap_duration==0,"No gap",gap_20),
         gap_20 = as.factor(gap_20),
         gap_30 = ifelse(gap_duration<30,"less 30","larger and equal 30"),
         gap_30 = ifelse(gap_duration==0,"No gap",gap_30),
         gap_30 = as.factor(gap_30))

model1.data = model1.data %>% 
  mutate(trchildnum = ifelse(teage < 18, trchildnum-1, trchildnum),
         gap_remove_child = ifelse(gap_30 == "larger and equal 30" &
                                     (gap_duration - care_children) >= 30, 1, 0),
         child = ifelse(trchildnum > 0, "Yes", "No"))

model1.data = model1.data %>%
  mutate(start.sleep.c = ifelse(start < as.POSIXct(strptime("19:00:00",format = "%H:%M:%S")),
                                "1: 6 pm - 7 pm", 
                                "2: 7 pm - 8 pm"),
         start.sleep.c = ifelse(start > as.POSIXct(strptime("20:00:00",format = "%H:%M:%S")),
                                "3: 8 pm - 9 pm", start.sleep.c),
         start.sleep.c = ifelse(start > as.POSIXct(strptime("21:00:00",format = "%H:%M:%S")), 
                                "4: 9 pm - 10 pm", start.sleep.c),
         start.sleep.c = ifelse(start > as.POSIXct(strptime("22:00:00",format = "%H:%M:%S")), 
                                "5: 10 pm - 11 pm", start.sleep.c),
         start.sleep.c = ifelse(start > as.POSIXct(strptime("23:00:00",format = "%H:%M:%S")), 
                                "6: 11 pm - 12 am", start.sleep.c),
         start.sleep.c = ifelse(start > as.POSIXct(strptime("24:00:00",format = "%H:%M:%S")), 
                                "7: 12 am - 1 am", start.sleep.c),
         start.sleep.c = ifelse(start > as.POSIXct(strptime("24:00:00",format = "%H:%M:%S"))+60*60, 
                                "8: 1 am - 2 am", start.sleep.c),
         start.sleep.c = ifelse(start > as.POSIXct(strptime("24:00:00",format = "%H:%M:%S"))+2*60*60, 
                                "9: 2 am - 3 am", start.sleep.c),
         start.sleep.c = ifelse(start > as.POSIXct(strptime("24:00:00",format = "%H:%M:%S"))+3*60*60, 
                                "99: 3 am - 4 am", start.sleep.c),
         start.sleep = as.numeric(start - as.POSIXct(strptime("00:00:00",format = "%H:%M:%S")),
                                  units = "hours"),
         start.sleep.scale = scale(start.sleep),
         start.sleep.center = start.sleep-24)


###############################################################################
######################## Quantile Regression Models ########################################
###############################################################################

########################whole dataset#######################################################
ggplot(model1.data, aes(x=exer_duration,y=sleep_duration)) + geom_point() + geom_smooth(method="lm")
# Build the general quantile model
qs <- 1:9/10
quantile.model.general <- rq(sleep_duration ~ exer_duration + tesex*age.c, weight = final.weight.b, data = model1.data, tau = qs)
#summary(quantile.model.general)
coef(quantile.model.general)
plot(summary(quantile.model.general), parm="exer_duration")
#yhat = predict(quantile.model.general, data = model1.data.sub)
#yhat0.1 = yhat[1]
#dp0.1 = data.frame(yhat=yhat0.1, exer_duration=model1.data.sub$exer_duration)
#ggplot(dp0.1, aes(x=exer_duration, y=yhat)) + geom_line() 

# only exercise time as predictor
qs <- 1:9/10
quantile.model.exercise <- rq(sleep_duration ~ exer_duration, weight = final.weight.b, data = model1.data, tau = qs)
#summary(quantile.model.exercise)
coef(quantile.model.exercise)
plot(summary(quantile.model.exercise), parm="exer_duration")

ggplot(model1.data.sub, aes(exer_duration,sleep_duration)) + 
  geom_point() + 
  geom_abline(intercept=coef(quantile.model.exercise)[1,1], slope=coef(quantile.model.exercise)[2,1], color = "blue") +
  geom_abline(intercept=coef(quantile.model.exercise)[1,9], slope=coef(quantile.model.exercise)[2,9], color = "grey")



########################only weekdays data with exercise####################################

# Sleep Duration Overall
model1.data.sub = subset(model1.data, primary.sleep=="primary sleep")
model1.data.sub = subset(model1.data.sub, start > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S")))
model1.data.sub = subset(model1.data.sub, days == "Weekday")
model1.data.sub = subset(model1.data.sub, exer_duration90 > 0 & sleep_duration > 0)

##########################data visualization###############################################
ggplot(model1.data.sub, aes(exer_duration,sleep_duration,color=age.c)) + geom_point()
ggplot(model1.data.sub, aes(exer_duration90,sleep_duration,color=age.c)) + geom_point()

#########################Use exer_duration below as predictor##############################
ggplot(model1.data.sub, aes(x=exer_duration,y=sleep_duration)) + geom_point() + geom_smooth(method="lm")
# Build the general quantile model
qs <- 1:9/10
quantile.model.general <- rq(sleep_duration ~ exer_duration + tesex*age.c, weight = final.weight.b, data = model1.data.sub, tau = qs)
#summary(quantile.model.general)
coef(quantile.model.general)
plot(summary(quantile.model.general), parm="exer_duration")
#yhat = predict(quantile.model.general, data = model1.data.sub)
#yhat0.1 = yhat[1]
#dp0.1 = data.frame(yhat=yhat0.1, exer_duration=model1.data.sub$exer_duration)
#ggplot(dp0.1, aes(x=exer_duration, y=yhat)) + geom_line() 

# only exercise time as predictor
qs <- 1:9/10
quantile.model.exercise <- rq(sleep_duration ~ exer_duration, weight = final.weight.b, data = model1.data.sub, tau = qs)
#summary(quantile.model.exercise)
coef(quantile.model.exercise)
plot(summary(quantile.model.exercise), parm="exer_duration")

ggplot(model1.data.sub, aes(exer_duration,sleep_duration)) + 
  geom_point() + 
  geom_abline(intercept=coef(quantile.model.exercise)[1,1], slope=coef(quantile.model.exercise)[2,1], color = "blue") +
  geom_abline(intercept=coef(quantile.model.exercise)[1,9], slope=coef(quantile.model.exercise)[2,9], color = "grey")

##########################Use exer_duration90 below instead################################
ggplot(model1.data.sub, aes(exer_duration90,sleep_duration)) + geom_point() + geom_smooth(method="lm")
# Build the general quantile model
qs <- 1:9/10
quantile.model90.general <- rq(sleep_duration ~ exer_duration90 + tesex*age.c, weight = final.weight.b, data = model1.data.sub, tau = qs)
#summary(quantile.model90.general)
coef(quantile.model90.general)
plot(summary(quantile.model90.general), parm="exer_duration90")


# only exercise time as predictor
qs <- 1:9/10
quantile.model90.exercise <- rq(sleep_duration ~ exer_duration90, weight = final.weight.b, data = model1.data.sub, tau = qs)
#summary(quantile.model90.exercise)
coef(quantile.model90.exercise)
plot(summary(quantile.model90.exercise), parm="exer_duration90")

ggplot(model1.data.sub, aes(exer_duration90,sleep_duration)) + 
  geom_point() + 
  geom_abline(intercept=coef(quantile.model90.exercise)[1,1], slope=coef(quantile.model90.exercise)[2,1], color = "blue") +
  geom_abline(intercept=coef(quantile.model90.exercise)[1,9], slope=coef(quantile.model90.exercise)[2,9], color = "grey")


###############################################################################
######################## Spline Models ########################################
###############################################################################
# Sleep Duration Overall
model1.data.svy = svydesign(ids = ~tucaseid,
                            weights = ~final.weight.b,
                            data = model1.data%>% mutate(slp = sleep_duration/60))
model1.data.svy.sub = subset(model1.data.svy, primary.sleep=="primary sleep")
model1.data.svy.sub = subset(model1.data.svy.sub, start > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S")))
# model1.data.svy.sub = subset(model1.data.svy.sub, days == "Weekday")
model1.data.svy.sub = subset(model1.data.svy.sub, days == "Weekday")

# Build the model
model.general <- svyglm(sleep_duration ~ exer_duration90 + tesex*age.c, 
                design = model1.data.svy.sub)
summary(model.general)

hist(model1.data.svy.sub$variables$exer_duration90)

# only exercise time
model.spline.exerciseonly <- svyglm(sleep_duration ~ exer_time + bs(exer_duration, knots = 5), 
                       design = model1.data.svy.sub)
summary(model.spline.exerciseonly)

# waso overall
model.overall.crude = svyglm((gap_20=="larger and equal 20") ~ exer_duration90 + tesex*age.c,
                                       design = model1.data.svy.sub, family = binomial)
summary(model.overall.crude)


###############################################################################
######################## Sleep Duration Models ###################################
###############################################################################
#data visualization
ggplot(model1.data.sub, aes(exer_time,sleep_duration)) + geom_boxplot()

# Sleep Duration Overall
model1.data.svy = svydesign(ids = ~tucaseid,
                            weights = ~final.weight.b,
                            data = model1.data%>% mutate(slp = sleep_duration/60))
model1.data.svy.sub = subset(model1.data.svy, primary.sleep=="primary sleep")
model1.data.svy.sub = subset(model1.data.svy.sub, start > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S")))
# model1.data.svy.sub = subset(model1.data.svy.sub, days == "Weekday")
model1.data.svy.sub = subset(model1.data.svy.sub, days == "Weekday")
###model for people who only exercise in the morning
model1.overall.crude.morning = svyglm(sleep_duration ~ (exer_time == "morning"),
                              design = model1.data.svy.sub)
summary(model1.overall.crude.morning)

ci = summary(model1.overall.crude.morning)$coefficient['exer_time == "morning"TRUE',]
ci[1]
c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

###model for people who only exercise in the afternoon
model1.overall.crude.afternoon = svyglm(sleep_duration ~ (exer_time == "afternoon"),
                                      design = model1.data.svy.sub)
summary(model1.overall.crude.afternoon)

ci = summary(model1.overall.crude.afternoon)$coefficient['exer_time == "afternoon"TRUE',]
ci[1]
c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

###model for people who only exercise in the evening
model1.overall.crude.evening = svyglm(sleep_duration ~ (exer_time == "evening"),
                                      design = model1.data.svy.sub)
summary(model1.overall.crude.evening)

ci = summary(model1.overall.crude.evening)$coefficient['exer_time == "evening"TRUE',]
ci[1]
c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

###model for people who exercise multiple times throughout the day
model1.overall.crude.multiple = svyglm(sleep_duration ~ (exer_time == "multiple"),
                                      design = model1.data.svy.sub)
summary(model1.overall.crude.multiple)

ci = summary(model1.overall.crude.multiple)$coefficient['exer_time == "multiple"TRUE',]
ci[1]
c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

##############################################################################
model1.overall.crude.general = svyglm(sleep_duration ~ exer_time,
                                      design = model1.data.svy.sub)
summary(model1.overall.crude.general)

table(model1.data.svy.sub$variables$exer_time)

################## Exercise Model with Only Age and Sex #######################
crude = svyglm(sleep_duration ~ exer_time,
               design = model1.data.svy.sub)
# row = c("tesex", "age.c", "race", "education", "employment", "trsppres", "child")
model1.age_sex = svyglm(sleep_duration ~ exer_time +age.c +tesex,
                       design = model1.data.svy.sub)
summary(model1.age_sex)

################## Exercise Model with Years #######################
crude = svyglm(sleep_duration ~ exer_time,
               design = model1.data.svy.sub)
# row = c("tesex", "age.c", "race", "education", "employment", "trsppres", "child")
model1.year = svyglm(sleep_duration ~ exer_time +year,
                        design = model1.data.svy.sub)
summary(model1.year)
table(model1.data$exer_time, model1.data$age.c)
############ Exercise Breakdown Models with Each Demographic Variable ##################
model1.age = svyglm(sleep_duration ~ exer_time + age.c,
                    design = model1.data.svy.sub)
summary(model1.age)
anova(crude, model1.age, test = "F", method = "LRT")

model1.race = svyglm(sleep_duration ~ exer_time + race,
                     design = model1.data.svy.sub)
summary(model1.race)
anova(crude, model1.race, test = "F", method = "LRT")

model1.education = svyglm(sleep_duration ~ exer_time + education,
                          design = model1.data.svy.sub)
summary(model1.education)
anova(crude, model1.education, test = "F", method = "LRT")

model1.employment = svyglm(sleep_duration ~ exer_time + employment,
                           design = model1.data.svy.sub)
summary(model1.employment)
anova(crude, model1.employment, test = "F", method = "LRT")

model1.trsppres = svyglm(sleep_duration ~ exer_time + trsppres,
                         design = model1.data.svy.sub)
summary(model1.trsppres)
anova(crude, model1.trsppres, test = "F", method = "LRT")

model1.child = svyglm(sleep_duration ~ exer_time + child,
                      design = model1.data.svy.sub)
summary(model1.child)
anova(crude, model1.child, test = "F", method = "LRT")

#################### Sex Distribution with Exercise Time ######################
table(model1.data$exer_time, model1.data$tesex)

#################### Sleep Duration for Female ##########################
model1.data.svy.female = subset(model1.data.svy.sub, tesex == "Female")
model1.female.crude = svyglm(sleep_duration ~ exer_time,
                             design = model1.data.svy.female)
summary(model1.female.crude)

#################### Sleep Duration for Male ############################
model1.data.svy.male = subset(model1.data.svy.sub, tesex == "Male")
model1.male.crude = svyglm(sleep_duration ~ exer_time,
                             design = model1.data.svy.male)
summary(model1.male.crude)

###############################################################################
######################## Fragmentation Models ###################################
###############################################################################
table(model1.data$exer_time, model1.data$gap_20)
library(questionr)
####################### Fragmentation Crude Models #############################
#morning
model2.fragment.overall.crude.morning = svyglm((gap_20=="larger and equal 20") ~ (exer_time=="morning"),
                                       design = model1.data.svy.sub, family = binomial)
summary(model2.fragment.overall.crude.morning)
a = odds.ratio(model2.fragment.overall.crude.morning)
a['exer_time == "morning"TRUE',1:3]

#afternoon
model2.fragment.overall.crude.afternoon = svyglm((gap_20=="larger and equal 20") ~ (exer_time=="afternoon"),
                                               design = model1.data.svy.sub, family = binomial)
summary(model2.fragment.overall.crude.afternoon)
b = odds.ratio(model2.fragment.overall.crude.afternoon)
b['exer_time == "afternoon"TRUE',1:3]

#evening
model2.fragment.overall.crude.evening = svyglm((gap_20=="larger and equal 20") ~ (exer_time=="evening"),
                                                 design = model1.data.svy.sub, family = binomial)
summary(model2.fragment.overall.crude.evening)
c = odds.ratio(model2.fragment.overall.crude.evening)
c['exer_time == "evening"TRUE',1:3]

#multiple
model2.fragment.overall.crude.multiple = svyglm((gap_20=="larger and equal 20") ~ (exer_time=="multiple"),
                                               design = model1.data.svy.sub, family = binomial)
summary(model2.fragment.overall.crude.multiple)
d = odds.ratio(model2.fragment.overall.crude.multiple)
d['exer_time == "multiple"TRUE',1:3]

#overall
model2.fragment.overall.crude = svyglm((gap_20=="larger and equal 20") ~ exer_time,
                                                design = model1.data.svy.sub, family = binomial)
summary(model2.fragment.overall.crude)

# overall for female
model1.data.svy.female = subset(model1.data.svy.sub, tesex == "Female")
model2.female.fragment.overall.crude = svyglm((gap_20=="larger and equal 20") ~ exer_time,
                             design = model1.data.svy.female, family = binomial)
summary(model2.female.fragment.overall.crude)

# Sleep Duration for Male
model1.data.svy.male = subset(model1.data.svy.sub, tesex == "Male")
model2.male.fragment.overall.crude = svyglm((gap_20=="larger and equal 20") ~ exer_time,
                           design = model1.data.svy.male, family = binomial)
summary(model2.male.fragment.overall.crude)

#afternoon for male
model2.male.fragment.crude.afternoon = svyglm((gap_20=="larger and equal 20") ~ (exer_time=="afternoon"),
                                                 design = model1.data.svy.male, family = binomial)
summary(model2.male.fragment.crude.afternoon)
mb = odds.ratio(model2.male.fragment.crude.afternoon)
mb['exer_time == "afternoon"TRUE',1:3]

#evening for male
model2.male.fragment.crude.evening = svyglm((gap_20=="larger and equal 20") ~ (exer_time=="evening"),
                                              design = model1.data.svy.male, family = binomial)
summary(model2.male.fragment.crude.evening)
mc = odds.ratio(model2.male.fragment.crude.evening)
mc['exer_time == "evening"TRUE',1:3]

#multiple for male
model2.male.fragment.crude.multiple = svyglm((gap_20=="larger and equal 20") ~ (exer_time=="multiple"),
                                              design = model1.data.svy.male, family = binomial)
summary(model2.male.fragment.crude.multiple)
md = odds.ratio(model2.male.fragment.crude.multiple)
md['exer_time == "multiple"TRUE',1:3]

#print("Overall Adjust")
#model3.fragment.overall.adjusted = svyglm((gap_30=="larger and equal 30") ~ as.character(year)  + 
#                                            tudiaryday + race + (exer_ind=="Exercise") +
#                                            (trchildnum>0) +trsppres +
#                                            age.c + employment + education.new + tesex,
#                                          design = model1.data.svy.sub, family = binomial)

#a = odds.ratio(model3.fragment.overall.adjusted)
#a['exer_ind == "Exercise"TRUE',1:3]

###############################################################################
######################## WASO Models ##########################################
###############################################################################

linear.WASO = svyglm(gap_max ~ exer_ind,
                     design = model1.data.svy.sub)

summary(linear.WASO)

WASO.sub = subset(model1.data.svy.sub, gap_max > 0)
linear.WASO.sub = svyglm(gap_max ~ exer_ind,
                     design = WASO.sub)

summary(linear.WASO.sub)

WASO.sub.10 = subset(model1.data.svy.sub, gap_max > 10)
linear.WASO.sub.10 = svyglm(gap_max ~ exer_ind,
                         design = WASO.sub.10)

summary(linear.WASO.sub.10)


###############################################################################
######################## Primary Sleep Start Time Models ###################################
###############################################################################
# Primary Sleep Start Time Overall
model3.data.svy = svydesign(ids = ~tucaseid,
                            weights = ~final.weight.b,
                            data = model1.data%>% mutate(slp = sleep_duration/60))
model3.data.svy.sub = subset(model3.data.svy, primary.sleep=="primary sleep")
model3.data.svy.sub = subset(model3.data.svy.sub, start > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S")))
# model1.data.svy.sub = subset(model1.data.svy.sub, days == "Weekday")
model3.data.svy.sub = subset(model3.data.svy.sub, days == "Weekday")
###model for people who only exercise in the morning
model3.overall.crude.morning = svyglm(starttime ~ (exer_time == "morning"),
                                      design = model3.data.svy.sub)
summary(model3.overall.crude.morning)

ci = summary(model3.overall.crude.morning)$coefficient['exer_time == "morning"TRUE',]
ci[1]
c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

###model for people who only exercise in the afternoon
model3.overall.crude.afternoon = svyglm(starttime ~ (exer_time == "afternoon"),
                                        design = model3.data.svy.sub)
summary(model3.overall.crude.afternoon)

ci = summary(model3.overall.crude.afternoon)$coefficient['exer_time == "afternoon"TRUE',]
ci[1]
c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

###model for people who only exercise in the evening
model3.overall.crude.evening = svyglm(starttime ~ (exer_time == "evening"),
                                      design = model3.data.svy.sub)
summary(model3.overall.crude.evening)

ci = summary(model3.overall.crude.evening)$coefficient['exer_time == "evening"TRUE',]
ci[1]
c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

###model for people who exercise multiple times throughout the day
model3.overall.crude.multiple = svyglm(starttime ~ (exer_time == "multiple"),
                                       design = model3.data.svy.sub)
summary(model3.overall.crude.multiple)

ci = summary(model3.overall.crude.multiple)$coefficient['exer_time == "multiple"TRUE',]
ci[1]
c(ci[1]-1.96*ci[2],ci[1]+1.96*ci[2])

###general model for people
model3.overall.crude.general = svyglm(starttime ~ exer_time,
                                      design = model3.data.svy.sub)
summary(model3.overall.crude.general)

################## Exercise Model with Only Age and Sex #######################
crude = svyglm(starttime ~ exer_time,
               design = model3.data.svy.sub)
# row = c("tesex", "age.c", "race", "education", "employment", "trsppres", "child")
model3.age_sex = svyglm(starttime ~ exer_time +age.c +tesex,
                        design = model3.data.svy.sub)
summary(model3.age_sex)

#################### Primary Sleep Start Time for Female ##########################
model3.data.svy.female = subset(model3.data.svy.sub, tesex == "Female")
model3.female.crude = svyglm(starttime ~ exer_time,
                             design = model3.data.svy.female)
summary(model3.female.crude)

#################### Primary Sleep Start Time for Male ############################
model3.data.svy.male = subset(model3.data.svy.sub, tesex == "Male")
model3.male.crude = svyglm(starttime ~ exer_time,
                           design = model3.data.svy.male)
summary(model3.male.crude)


###############################################################################
###################### Long/Short Sleep Duration ##############################
###############################################################################
library(questionr)
library(nnet)
print("Multinomial Logistic Regression")
# model1.data = read.csv("D:/muli.sas.csv")

model1.data.multi = model1.data %>%
  filter(primary.sleep == "primary sleep") %>%
  filter(start > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S"))) %>%
  filter(days == "Weekday")
# Long/Short Sleep Duration Overall
model1.data.multi$sleep_duration.c.r = relevel(model1.data.multi$sleep_duration.c, 
                                               ref = "normal")


model2.mult.overall.crude = multinom(sleep_duration.c.r ~ exer_ind,
                                     weights = final.weight.b, data = model1.data.multi, 
                                     trace=F)

print("Overall crude")
b = odds.ratio(model2.mult.overall.crude)
print(1/b['long/exer_indNo Exercise',1:3])
print(1/b['short/exer_indNo Exercise',1:3])
# long 1.539579 1.539609 1.539549
# short 1/c(1.07924, 1.07921, 1.0793)

model2.mult.overall.adjusted = multinom(sleep_duration.c.r ~ as.character(year) +  
                                          trsppres + race + tudiaryday + 
                                          education.new + employment +
                                          age.c*tesex + eating_ind,
                                        weights = final.weight.b, data = model1.data.multi, 
                                        trace=F)
print("Overall adjusted")
a = odds.ratio(model2.mult.overall.adjusted)
svymean(model1.data.multi$sleep_duration, design = model1.data.svy.sub)
ggplot(model1.data.multi, aes(x=sleep_duration)) + geom_histogram(binwidth = 20)
print(1/a['long/eating_indNo Eat',1:3])
print(1/a['short/eating_indNo Eat',1:3])


