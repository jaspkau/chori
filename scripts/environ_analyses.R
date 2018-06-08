#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

library(readxl)
library(vegan)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
#install.packages("tseries")
library(tseries)
#install.packages("forecast")
library(forecast)
library(lmtest)

# Environmnetal data ------------------------------------------------------

#Resources
#http://neondataskills.org/R/time-series-subset-dplyr/
data <- read_excel("data/PG_env.xlsx",
                   sheet = 1)

data$date <- as.POSIXct(data$date, format= "%y-%m-%d %H:%M",
                        tz = "GMT")

data$year = year(data$date)
data$month = month(data$date)
data.avg = group_by(data, Site, year, month)
tally(data.avg)
pg_env_avg = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt

####make avergaes of soil data

data <- read_excel("data/growth_germ.xlsx",
                   sheet = 5)

data.avg = group_by(data, Site, Year)
tally(data.avg)
pg_env_avg = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt
write.csv(pg_env_avg, file = "results/soil_germ_avg.csv")

####
aov = aov(pg_env_avg2$atemp ~ pg_env_avg2$pop.year)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(pg_env_avg2$atemp, pg_env_avg2$pop.year, DFerror, MSerror)
l

###Do ANOVA with environmnetal monitoring data
pg_env_avg2 = subset(pg_env_avg, Site != "NPS1")
pg_env_avg2 = subset(pg_env_avg2, Site != "NPS2")
pg_env_avg2$par.inphase = gsub("NaN", "", pg_env_avg2$par.inphase)
require(partykit)
pg_env_avg2$pop.year = interaction(pg_env_avg2$Site, pg_env_avg2$year)

pg_env_avg2$Site = as.factor(pg_env_avg2$Site)

##final will store final results

final = c()
for(i in 4:9){
  column = names(pg_env_avg2[i])
  f = anova(aov(pg_env_avg2[,i]~ Site, data=pg_env_avg2))$"F value"
  av = anova(aov(pg_env_avg2[,i]~ Site , data=pg_env_avg2))$"Pr(>F)"
  results = data.frame(otu = paste(column), F.value = paste(f), Pr..F. = paste(av))
  final = rbind(final, results)
} 

write.csv(final, file='soil_aov.csv')

####check for autocorrelation

ts.df = ts(data)

crosscorrelation<-ccf(data$atemp,data$sm2, na.action = na.pass)
g.mod = tslm(atemp ~ sm2, data = ts.df, na.rm = TRUE)
summary(g.mod)

library(Hmisc)

rcorr(data.pg$EO12.atemp,data.pg$EO12.rh)

####weekly averages

d = xts(data, order.by=as.Date(data$date,"%y-%m-%d %H:%M", tz = "GMT"))
ts.weeks <- apply.weekly(d, mean)

####time series analyses

grangertest(EO12.atemp ~ EO16.atemp, order = 6, data = data)

library(Kendall)
Kendall(data$EO12.atemp, data$EO16.atemp)

library(TSclust)

data <- read_excel("data/chor_env_mon_germ_analysis_file.xlsx",
                   sheet = 1)

data$date <- as.POSIXct(data$date, format= "%y-%m-%d %H:%M",
                        tz = "GMT")

data$year = year(data$date)
data$month = month(data$date)
data2 = subset(data, month == 1|month == 2|month == 3)
data2 = subset(data2, year == 2015| year == 2016| year == 2017)

keep = c("EO12.atemp", "EO14.atemp", "EO16.atemp")
data3 = data2[,keep]
dat = data3[!rowSums((is.na(data3))),]

dis <- diss(dat, "DTW")
h = hclust(dis)
plot(h)

keep = c("EO12.soiltemp", "EO14.soiltemp", "EO16.soiltemp")
data3 = data2[,keep]
dat = data3[!rowSums((is.na(data3))),]

dis <- diss(dat, "DTW")
h = hclust(dis)
plot(h)

keep = c("EO12.rf", "EO14.rf", "EO16.rf")
data3 = data2[,keep]
dat = data3[!rowSums((is.na(data3))),]

dis <- diss(dat, "DTW")
h = hclust(dis)
plot(h)

keep = c("EO12.sm2", "EO14.sm2", "EO16.sm2")
data3 = data2[,keep]
dat = data3[!rowSums((is.na(data3))),]

dis <- diss(dat, "DTW")
h = hclust(dis)
plot(h)
