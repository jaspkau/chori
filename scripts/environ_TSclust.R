#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

library(readxl)
library(vegan)
library(lubridate)#workwithdates
library(dplyr)#datamanipulation(filter,summarize,mutate)
library(ggplot2)#graphics
library(gridExtra)#tileseveralplotsnexttoeachother
#install.packages("tseries")
library(tseries)
#install.packages("forecast")
library(forecast)
library(lmtest)
library(gdata)
library(dendextend)
library(ggdendro)

library(TSclust)
library(gdata)
library(imputeTS)

data<-read.xls("data/chor_env_mon_germ_analysis_file.xlsx",
               sheet=1,verbose=TRUE,na.strings="N/A")

data$date<-as.POSIXct(as.character(data$date),format="%m/%d/%Y",
                      tz="America/Los_Angeles")

data$year = year(data$date)
data$month = month(data$date)
data2=subset(data,month == 1| month == 2 |month == 3)
data2=subset(data2, year == 2015 | year== 2016 | year == 2017 | year == 2018)
grangertest(EO12.at ~ EO14.at, order = 1, data = data2)
####make length of time series same

#drop = c("2015-03-18", "2016-02-09", "2017-02-11", "2017-02-12")
#drop = as.Date(drop)
#data2 <- subset(data2, !as.Date(data2$date) %in% drop)

x=subset(data2,year==2015)
names(x) = paste(names(x),".15")
names(x)=gsub(" ","",names(x))
#x=x[1:540,]

y=subset(data2,year==2016)
names(y)=paste(names(y),".16")
names(y)=gsub(" ","",names(y))

z=subset(data2,year==2017)
names(z)=paste(names(z),".17")
names(z)=gsub(" ","",names(z))

z2=subset(data2,year==2018)
names(z2)=paste(names(z2),".18")
names(z2)=gsub(" ","",names(z2))

v=cbind(x,y,z,z2)

keep = c("EO12.at.15","EO14.at.15","EO16.at.15",
         "EO12.at.16","EO14.at.16","EO16.at.16", "NPS1.at.16",
         "EO12.at.17","EO14.at.17","EO16.at.17", "NPS2.at.17",
         "EO12.at.18","EO14.at.18", "NPS1.at.18", "NPS2.at.18")

data3 = v[,keep]
dat = data3[!rowSums((is.na(data3))),]
names(dat) = paste(names(dat),"_",round(colMeans(dat),0))
names(dat) = gsub(" ", "", names(dat))

dis <- diss(dat,"DTW")

avg = summarise_each(dat, funs(mean(., na.rm = TRUE)))   # calculate the annual mean of airt
avg = as.matrix(avg)
posthoc.friedman.nemenyi.test(avg)

dist_w = vegdist(dat, method = "bray")

met = as.data.frame(names(dis))
row.names(met) = met[,1]
ccdata = cSplit(met, "names(dis)", ".")
ccdata = plyr::rename(ccdata, c("names(dis)_1"="Site", "names(dis)_2"="Year", "names(dis)_3"="month"))
ccdata = as.data.frame(ccdata)
row.names(ccdata) = row.names(met)
ccdata$code = row.names(ccdata)

a = adonis(dis ~ ccdata$Site, permutations = 999)
a

a = mrpp(dis ~ ccdata$Site, permutations = 999)
a

h=hclust(dis)
dhc <- as.dendrogram(h) %>% set("labels_cex", 0.5)
ggd1 <- as.ggdend(dhc)
p1 = ggplot(ggd1, horiz = TRUE,theme = theme_minimal())
p1

keep = c("EO12.st.15","EO14.st.15","EO16.st.15",
         "EO12.st.16","EO14.st.16","EO16.st.16", "NPS1.st.16", "NPS2.st.16",
         "EO12.st.17","EO14.st.17","EO16.st.17", "NPS2.st.17",
         "EO12.st.18","EO14.st.18", "NPS1.st.18", "NPS2.st.18")

data3=v[,keep]
dat=data3[!rowSums((is.na(data3))),]
names(dat) = paste(names(dat),"_",round(colMeans(dat),0))
names(dat) = gsub(" ", "", names(dat))

dis<-diss(dat,"DTW")
h=hclust(dis)
dhc <- as.dendrogram(h) %>% set("labels_cex", 0.5)
ggd1 <- as.ggdend(dhc)
p2 = ggplot(ggd1, horiz = TRUE,theme = theme_minimal())
p2

keep = c("EO12.rf.15","EO14.rf.15","EO16.rf.15",
         "EO12.rf.16","EO14.rf.16","EO16.rf.16", "NPS1.rf.16",
         "EO12.rf.17","EO14.rf.17","EO16.rf.17",
         "EO12.rf.18","EO14.rf.18", "NPS1.rf.18")

data3=v[,keep]
dat=data3[!rowSums((is.na(data3))),]
names(dat) = paste(names(dat),"_",round(colMeans(dat),2))
names(dat) = gsub(" ", "", names(dat))

dis<-diss(dat,"DTW")
h=hclust(dis)
dhc <- as.dendrogram(h) %>% set("labels_cex", 0.5)
ggd1 <- as.ggdend(dhc)
p3 = ggplot(ggd1, horiz = TRUE,theme = theme_minimal())
p3

keep = c("EO12.sm2.15","EO14.sm2.15","EO16.sm2.15",
         "EO12.sm2.16","EO14.sm2.16","EO16.sm2.16",
         "EO12.sm2.17","EO14.sm2.17","EO16.sm2.17", "NPS1.sm2.17", "NPS2.sm2.17",
         "EO12.sm2.18","EO14.sm2.18", "NPS1.sm2.18", "NPS2.sm2.18")

data3=v[,keep]
dat=data3[!rowSums((is.na(data3))),]
names(dat) = paste(names(dat),"_",round(colMeans(dat),2))
names(dat) = gsub(" ", "", names(dat))

dis<-diss(dat,"DTW")
h=hclust(dis)
dhc <- as.dendrogram(h) %>% set("labels_cex", 0.5)
ggd1 <- as.ggdend(dhc)
p4 = ggplot(ggd1, horiz = TRUE,theme = theme_minimal())
p4

keep = c("EO12.rh.15","EO14.rh.15","EO16.rh.15",
         "EO12.rh.16","EO14.rh.16","EO16.rh.16", "NPS1.rh.16",
         "EO12.rh.17","EO14.rh.17","EO16.rh.17", "NPS2.rh.17",
         "EO14.rh.18", "NPS1.rh.18", "NPS2.rh.18")

data3=v[,keep]
dat=data3[!rowSums((is.na(data3))),]
names(dat) = paste(names(dat),"_",round(colMeans(dat),0))
names(dat) = gsub(" ", "", names(dat))

dis<-diss(dat,"DTW")
h=hclust(dis)
dhc <- as.dendrogram(h) %>% set("labels_cex", 0.5)
ggd1 <- as.ggdend(dhc)
p5 = ggplot(ggd1, horiz = TRUE,theme = theme_minimal())
p5

keep = c("EO12.par.15","EO14.par.15","EO16.par.15",
         "EO14.par.16","EO16.par.16",
         "EO12.par.17","EO14.par.17","EO16.par.17",
         "EO12.par.18","EO14.par.18")

data3=v[,keep]
dat=data3[!rowSums((is.na(data3))),]
names(dat) = paste(names(dat),"_",round(colMeans(dat),0))
names(dat) = gsub(" ", "", names(dat))

dis<-diss(dat,"DTW")
h=hclust(dis)
dhc <- as.dendrogram(h) %>% set("labels_cex", 0.5)
ggd1 <- as.ggdend(dhc)
p6 = ggplot(ggd1, horiz = TRUE,theme = theme_minimal())
p6

ggpubr::ggarrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
