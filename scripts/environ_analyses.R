#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

library(gdata)
library(lubridate)#workwithdates
library(dplyr)#datamanipulation(filter,summarize,mutate)
library(TSclust)
library(readxl)
library(imputeTS)
library(lsmeans)
library(multcomp)

#####Import environmnetal data time series
data <- read_excel("data/PCA_env.xlsx",
               sheet=1)

data$date <- as.POSIXct(data$date,format="%Y-%m-%d",
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
pg_env_avg$year = as.factor(pg_env_avg$year)
pg_env_avg$month = as.factor(pg_env_avg$month)
pg_env_avg$east = pg_env_avg$Site=="EO12"|pg_env_avg$Site=="EO16"|pg_env_avg$Site=="NPS2"

####correlation between microclimatic variables

x = cor.test(pg_env_avg$at, pg_env_avg$st)
x
x = cor.test(pg_env_avg$at, pg_env_avg$sm2)
x
x = cor.test(pg_env_avg$at, pg_env_avg$rh)
x
x = cor.test(pg_env_avg$at, pg_env_avg$par)
x
x = cor.test(pg_env_avg$at, pg_env_avg$rf)
x

###Regression model to compare environmnetal variables across space and time

model = lmerTest::lmer(as.numeric(at) ~ east + (1|year/month/day2), data = pg_env_avg)
anova(model)

model = lmerTest::lmer(as.numeric(st) ~ east + (1|year/month/day2), data = pg_env_avg)
anova(model)

model = lmerTest::lmer(as.numeric(sm2) ~ east + (1|year/month/day2), data = pg_env_avg)
anova(model)

model = lmerTest::lmer(as.numeric(rh) ~ east + (1|year/month/day2), data = pg_env_avg)
anova(model)

model = lmerTest::lmer(as.numeric(par) ~ east + (1|year/month/day2), data = pg_env_avg)
anova(model)

###seprate df for rainfall by summing the values to get total precipitation
pg_env_rf <- data.avg %>% 
  summarise(rf = sum(rf, na.rm = TRUE))
pg_env_rf$east = pg_env_rf$Site=="EO12"|pg_env_rf$Site=="EO16"|pg_env_rf$Site=="NPS2"

model = lmerTest::lmer(as.numeric(rf) ~ east + (1|year/month/day2), data = pg_env_rf)
anova(model)

############################### Site
posthoc <- glht(model, linfct = mcp(east = "Tukey"))
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
