# load packages
library(tidyverse)

actlog2003=read.csv("D:/ATUS/ATUS_Originial_Data/2003atus/actlog.csv")
exer_2003=filter(actlog2003, tutier1code == 13 & tutier2code == 1)
exer_2003=exer_2003[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2004=read.csv("D:/ATUS/ATUS_Originial_Data/2004atus/actlog.csv")
exer_2004=filter(actlog2004, tutier1code == 13 & tutier2code == 1)
exer_2004=exer_2004[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2005=read.csv("D:/ATUS/ATUS_Originial_Data/2005atus/actlog.csv")
exer_2005=filter(actlog2005, tutier1code == 13 & tutier2code == 1)
exer_2005=exer_2005[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2006=read.csv("D:/ATUS/ATUS_Originial_Data/2006atus/actlog.csv")
exer_2006=filter(actlog2006, tutier1code == 13 & tutier2code == 1)
exer_2006=exer_2006[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2007=read.csv("D:/ATUS/ATUS_Originial_Data/2007atus/actlog.csv")
exer_2007=filter(actlog2007, tutier1code == 13 & tutier2code == 1)
exer_2007=exer_2007[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2008=read.csv("D:/ATUS/ATUS_Originial_Data/2008atus/actlog.csv")
exer_2008=filter(actlog2008, tutier1code == 13 & tutier2code == 1)
exer_2008=exer_2008[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2009=read.csv("D:/ATUS/ATUS_Originial_Data/2009atus/actlog.csv")
exer_2009=filter(actlog2009, tutier1code == 13 & tutier2code == 1)
exer_2009=exer_2009[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2010=read.csv("D:/ATUS/ATUS_Originial_Data/2010atus/actlog.csv")
exer_2010=filter(actlog2010, tutier1code == 13 & tutier2code == 1)
exer_2010=exer_2010[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2011=read.csv("D:/ATUS/ATUS_Originial_Data/2011atus/actlog.csv")
exer_2011=filter(actlog2011, tutier1code == 13 & tutier2code == 1)
exer_2011=exer_2011[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2012=read.csv("D:/ATUS/ATUS_Originial_Data/2012atus/actlog.csv")
exer_2012=filter(actlog2012, tutier1code == 13 & tutier2code == 1)
exer_2012=exer_2012[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2013=read.csv("D:/ATUS/ATUS_Originial_Data/2013atus/actlog.csv")
exer_2013=filter(actlog2013, tutier1code == 13 & tutier2code == 1)
exer_2013=exer_2013[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2014=read.csv("D:/ATUS/ATUS_Originial_Data/2014atus/actlog.csv")
exer_2014=filter(actlog2014, tutier1code == 13 & tutier2code == 1)
exer_2014=exer_2014[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2015=read.csv("D:/ATUS/ATUS_Originial_Data/2015atus/actlog.csv")
exer_2015=filter(actlog2015, tutier1code == 13 & tutier2code == 1)
exer_2015=exer_2015[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2016=read.csv("D:/ATUS/ATUS_Originial_Data/2016atus/actlog.csv")
exer_2016=filter(actlog2016, tutier1code == 13 & tutier2code == 1)
exer_2016=exer_2016[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2017=read.csv("D:/ATUS/ATUS_Originial_Data/2017atus/actlog.csv")
exer_2017=filter(actlog2017, tutier1code == 13 & tutier2code == 1)
exer_2017=exer_2017[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

actlog2018=read.csv("D:/ATUS/ATUS_Originial_Data/2018atus/actlog.csv")
exer_2018=filter(actlog2018, tutier1code == 13 & tutier2code == 1)
exer_2018=exer_2018[,c("tucaseid", "tuactdur24", "tutier1code", "tutier2code", "tutier3code", "tustarttim", "tustoptime")]

exer_2019=read.csv("D:/ATUS/ATUS_Originial_Data/2019atus/actlog.csv")

exer=exer_2003
exer=rbind(exer,exer_2004)
exer=rbind(exer,exer_2005)
exer=rbind(exer,exer_2006)
exer=rbind(exer,exer_2007)
exer=rbind(exer,exer_2008)
exer=rbind(exer,exer_2009)
exer=rbind(exer,exer_2010)
exer=rbind(exer,exer_2011)
exer=rbind(exer,exer_2012)
exer=rbind(exer,exer_2013)
exer=rbind(exer,exer_2014)
exer=rbind(exer,exer_2015)
exer=rbind(exer,exer_2016)
exer=rbind(exer,exer_2017)
exer=rbind(exer,exer_2018)

library(data.table)

exer_dt=as.data.table(exer)

counts <- exer_dt[, .(rowCount = .N), by = "tucaseid"]
counts_2=filter(counts,rowCount>=2) 


#exer_wide=reshape(exer, idvar = "tucaseid", timevar = "tustarttim", direction = "wide")

#length(unique(exer$tucaseid))

exer_1=filter(exer, !(tucaseid %in% counts_2$tucaseid))
exer_2=filter(exer, (tucaseid %in% counts_2$tucaseid))


library(hms)
exer_1$tustarttim <- as_hms(exer_1$tustarttim)
exer_time=rep("evening",nrow(exer_1))

library(lubridate)

for (i in 1:nrow(exer_1)) {
  if (hour(exer_1$tustarttim[i]) >= 12 & hour(exer_1$tustarttim[i]) < 18){
    exer_time[i] = "afternoon"
  }
}

for (i in 1:nrow(exer_1)) {
  if (hour(exer_1$tustarttim[i]) >= 4 & hour(exer_1$tustarttim[i]) < 12){
    exer_time[i] = "morning"
  }
}

exer_1=cbind(exer_1,exer_time)

exer_2=exer_2[!duplicated(exer_2$tucaseid),]
exer_time=rep("multiple",nrow(exer_2))
exer_2=cbind(exer_2,exer_time)
exer_all=rbind(exer_1,exer_2)

exer_all$exer_time=as.factor(exer_all$exer_time)

table(exer_all$exer_time)

write.csv(exer_all, "D:/ATUS/Processed_Data/all_exercise.csv", row.names=FALSE)

