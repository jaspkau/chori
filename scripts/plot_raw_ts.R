#setwd("C:/Users/jaspkaur/Google Drive/data_analysis/chorizanthe/chori")

library(readxl)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(randomForest) 
library(adespatial)
library(magrittr)
library(ggpubr)
library(crosstalk)
library(reshape2)
#devtools::install_github("ropensci/plotly")
library(plotly)
library(scales)
library(gdata)

# Environmnetal data ------------------------------------------------------

#Resources
#http://neondataskills.org/R/time-series-subset-dplyr/

data <- read.xls("data/chor_env_mon_germ_analysis_file.xlsx",
                 sheet = 1, verbose = TRUE, na.strings="N/A", perl="C:/Perl64/bin/perL")

data$date <- as.POSIXct(data$date, format= "%Y-%m-%d %H:%M",
                        tz = "GMT")

data$year = year(data$date)
data$month = month(data$date)
#data = subset(data, month = c(1,2,3))

# Ploting of environmnetal averages ---------------------------------------
#Useful link = https://plot.ly/ggplot2/geom_line/

####raw data

data = group_by(data, date, year, month)
data$date = as.Date(data$date)
tally(data)
data = as.data.frame(data)
m_raw = melt(data, id = c("year", "month", "date"))

m_raw_atemp = subset(m_raw, variable == "EO12.atemp" | 
                       variable == "EO14.atemp" | 
                       variable == "EO16.atemp")

m_raw_atemp$value = as.numeric(m_raw_atemp$value)

p <- ggplot(data=m_raw_atemp, aes(date, value, group=variable, colour=variable)) +
  geom_smooth() + scale_x_date(breaks = date_breaks("months"),
                               labels = date_format("%b %y")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("red", "yellow", 
                                "black", "blue", "green"))

p = ggplotly(p)
