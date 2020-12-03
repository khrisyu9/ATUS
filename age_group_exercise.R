# Sleep Duration Overall
model1.data.svy = svydesign(ids = ~tucaseid,
                            weights = ~final.weight.b,
                            data = model1.data%>% mutate(slp = sleep_duration/60))
model1.data.svy.sub = subset(model1.data.svy, primary.sleep=="primary sleep")
model1.data.svy.sub = subset(model1.data.svy.sub, start > as.POSIXct(strptime("18:00:00",format = "%H:%M:%S")))
# model1.data.svy.sub = subset(model1.data.svy.sub, days == "Weekday")
model1.data.svy.sub = subset(model1.data.svy.sub, days == "Weekday")
model1.data.svy.sub = subset(model1.data.svy.sub, age.c == "15 - 22")
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