#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

library(gdata)
library(lubridate)#workwithdates
library(dplyr)#datamanipulation(filter,summarize,mutate)
library(TSclust)
library(gdata)
library(imputeTS)
library(lsmeans)
library(multcomp)

#####Import environmnetal data time series
data <- read.xls("data/PCA_env.xlsx",
               sheet=2,verbose=TRUE,na.strings="N/A")

data$date <- as.POSIXct(as.character(data$date),format="%Y-%m-%d",
                      tz="America/Los_Angeles")

data$year = year(data$date)
data$month = month(data$date)
data$day = day(data$date)

#######divide monthy data into four weeks

data$day2 = ifelse(data$day <8, "a", "")
data$day2 = ifelse(data$day >7, "b", paste(data$day2))
data$day2 = ifelse(data$day >16, "c", paste(data$day2))
data$day2 = ifelse(data$day >24, "d", paste(data$day2))

#######compute the weekly averages

data.avg = group_by(data, Site, year, month, day2)
tally(data.avg)
pg_env_avg = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt
pg_env_avg$int = paste(pg_env_avg$Site,".",pg_env_avg$year,".",pg_env_avg$month)
pg_env_avg$int = gsub(" ", "", pg_env_avg$int)

###select the varible you want to test

keep = c("Site", "year", "month", "day2", "par")
#pg_env_avg[is.na(pg_env_avg)] <- " "
avg = pg_env_avg[ ,(names(pg_env_avg) %in% keep)]
avg = na.omit(avg)
avg$year = as.factor(avg$year)
avg$month = as.factor(avg$month)

###Regression model to compare environmnetal variables across space and time

model = lmerTest::lmer(as.numeric(par) ~ Site*year + (1|month/day2), data = avg)
anova(model)

############################### Site
posthoc <- glht(model, linfct = mcp(Site = "Tukey"))
summary(posthoc)

cld(posthoc,
    alpha   = 0.05, 
    Letters = letters,     
    adjust  = "tukey")

#############################Year
posthoc <- glht(model, linfct = mcp(year = "Tukey"))
summary(posthoc)

cld(posthoc,
    alpha   = 0.05, 
    Letters = letters,     
    adjust  = "tukey")
    
#NOTES
#random = ~ 1 | rep/subject
#Indicates that each subject-within-rep unit will have its own intercept
