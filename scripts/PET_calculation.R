#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")

# import growth data##########

library(readxl)
library(lubridate) #works with date functions
library(xts)
library(Evapotranspiration)

###load file with contants
constants = read_excel("data/ET_constants.xlsx",
                       sheet = 2)
constants = as.list(constants)

###load climatic data

data = read_excel("data/chor_env_mon_germ_analysis_file.xlsx",
                           sheet = 5)

###fragment dates
data$date <- as.POSIXct(data$date, format= "%Y-%m-%d %H:%M",
                        tz = "GMT")

data$Year = year(data$date)
data$Month = month(data$date)
data$Day = day(data$date)
data$Hour = hour(data$date)

###format climatic data
data.et <- ReadInputs(varnames = c("Temp", "RH", "n"),
                   data, 
                   constants, 
                   stopmissing=c(10,10,3),
                   timestep = "subdaily",
                   interp_missing_days = TRUE, 
                   interp_missing_entries = TRUE, 
                   interp_abnormal = TRUE, 
                   missing_method = "neighbouring average", 
                   abnormal_method = "neighbouring average")

####generate index
results <- ET.Turc(data.et, constants, ts="daily", solar="sunshine hours", humid= TRUE,
                               message="yes", save.csv="yes")

results <- ET.HargreavesSamani(data.et, constants, ts="daily", humid= TRUE,
              message="yes", save.csv="yes")

results <- ET.McGuinnessBordne(data.et, constants, ts="daily", humid= TRUE,
                               message="yes", save.csv="yes")




