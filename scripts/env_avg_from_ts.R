setwd("C:/Users/jas/Google Drive/data_analysis/chorizanthe/chori")

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
library(ggpubr)

data <- read_excel("data/chor_env_mon_germ_analysis_file.xlsx",
                   sheet = 1)

data$date <- as.POSIXct(data$date, format= "%y-%m-%d %H:%M",
                        tz = "GMT")

data$year = year(data$date)
data$month = month(data$date)
data = subset(data, month == 10| month == 11|month == 12)
data.avg = group_by(data, year)
tally(data.avg)

pg_env_avg = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt
#write.csv(pg_env_avg, file = "results/DR_env.csv")
