setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

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

data <- read_excel("data/PCA_env.xlsx",
                   sheet = 1)

data$date <- as.POSIXct(data$date, format= "%y-%m-%d %H:%M",
                        tz = "GMT")

data$year = year(data$date)
data$month = month(data$date)
data.avg = group_by(data, year, month)
tally(data.avg)
pg_env_avg = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt
pg_env_avg$int = paste(pg_env_avg$month,".",pg_env_avg$year)
pg_env_avg$int = gsub(" ", "", pg_env_avg$int)
pg_env_avg$int2 = factor(pg_env_avg$int, levels = c("1.2015", "2.2015", "3.2015", "1.2016", "2.2016", 
                                                    "3.2016", "1.2017", "2.2017", "3.2017",
                                                    "1.2018", "2.2018", "3.2018"))
#write.csv(pg_env_avg, file = "results/PG_env_avg_with_2018.csv")

p = ggplot(pg_env_avg, aes(int2, atemp, group = Site, shape= Site)) + 
  geom_point(size = 3) + geom_line(linetype="twodash") 

p

####facet by year

pg_env_avg$month2 = factor(pg_env_avg$month, levels = c("1", "2", "3"))
#write.csv(pg_env_avg, file = "results/PG_env_avg_with_2018.csv")

p1 = ggplot(pg_env_avg, aes(month2, atemp, group = Site, shape= Site)) + 
  geom_point(size = 1.5) + geom_line(linetype="twodash") + facet_grid(. ~ year)

p2 = ggplot(pg_env_avg, aes(month2, stemp, group = Site, shape= Site)) + 
  geom_point(size = 1.5) + geom_line(linetype="twodash") + facet_grid(. ~ year)

p3 = ggplot(pg_env_avg, aes(month2, rh, group = Site, shape= Site)) + 
  geom_point(size = 1.5) + geom_line(linetype="twodash") + facet_grid(. ~ year)

p4 = ggplot(pg_env_avg, aes(month2, sm2, group = Site, shape= Site)) + 
  geom_point(size = 1.5) + geom_line(linetype="twodash") + facet_grid(. ~ year)

p5 = ggplot(pg_env_avg, aes(month2, rf, group = Site, shape= Site)) + 
  geom_point(size = 1.5) + geom_line(linetype="twodash") + facet_grid(. ~ year)

p6 = ggplot(pg_env_avg, aes(month2, par, group = Site, shape= Site)) + 
  geom_point(size = 1.5) + geom_line(linetype="twodash") + facet_grid(. ~ year)

ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

######################## Type 2

p1 = ggplot(data = pg_env_avg, aes(month2, atemp, group = Site, color = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year) 

p2 = ggplot(pg_env_avg, aes(month2, stemp, group = Site, color = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

p3 = ggplot(pg_env_avg, aes(month2, rh, group = Site, color = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

p4 = ggplot(pg_env_avg, aes(month2, sm2, group = Site, color = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

p5 = ggplot(pg_env_avg, aes(month2, rf, group = Site, color = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

p6 = ggplot(pg_env_avg, aes(month2, par, group = Site, color = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 1, nrow = 6, common.legend = TRUE, legend = "bottom")

##############Type 3

p1 = ggplot(data = pg_env_avg, aes(month2, atemp, group = Site, shape = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year) 

p2 = ggplot(pg_env_avg, aes(month2, stemp, group = Site, shape = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

p3 = ggplot(pg_env_avg, aes(month2, rh, group = Site, shape = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

p4 = ggplot(pg_env_avg, aes(month2, sm2, group = Site, shape = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

p5 = ggplot(pg_env_avg, aes(month2, rf, group = Site, shape = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

p6 = ggplot(pg_env_avg, aes(month2, par, group = Site, shape = Site)) + theme_classic() +
  geom_point(size = 2) + state_col_ord + geom_line(linetype="solid") + facet_grid(. ~ year)

ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 1, nrow = 6, common.legend = TRUE, legend = "bottom")
