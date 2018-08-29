library(imputeTS)

data<-read.xls("data/Book1.xlsx",
               sheet=2,verbose=TRUE,na.strings="N/A")

data$date<-as.POSIXct(as.character(data$date),format="%m/%d/%Y",
                      tz="America/Los_Angeles")

data$NPS2.at2 = na.interpolation(data$NPS2.at, option = "stine")

data$year=year(data$date)
data$month=month(data$date)
data2=subset(data, month==1|month==2|month==3)
data2=subset(data2,year==2016)
mean(data2$NPS2.at2)

write.csv(data2, file = "x.csv")
