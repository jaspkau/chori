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
library(ggpubr)
library(tidyr)

data <- read_excel("data/PCA_env.xlsx",
                   sheet=1)

data$date <- as.POSIXct(data$date,format="%Y-%m-%d",
                        tz="America/Los_Angeles")

data$year = year(data$date)
data$month = month(data$date)
data$day = day(data$date)
data$east = data$Site=="EO12"|data$Site=="EO16"|data$Site=="NPS2"

#######divide monthy data into four weeks

data$day2 = ifelse(data$day <8, "1", "")
data$day2 = ifelse(data$day >7, "2", paste(data$day2))
data$day2 = ifelse(data$day >16, "3", paste(data$day2))
data$day2 = ifelse(data$day >24, "4", paste(data$day2))

#convert the data from wide format to long format
long_data = data %>% gather(Variable, Value, at:rf)
#

####make list of dfs by variable
datalist = split(long_data, long_data$Variable)

###apply functions to generate means, sd and and confidence intervals for all dfs in the list

func = function(x) { ###here x refers to the dataframes within a list
  groupx = group_by(x, east, month, day2)
  summax = summarise(groupx, avg = mean(Value, na.rm = TRUE),
                     sum = sum(Value, na.rm = TRUE),
                     sd = sd(Value, na.rm = TRUE),
                     n = n(),
                     se = sd/ sqrt(n),
                     lci = avg-1.96*se,
                     uci = avg+1.96*se)
}

x = lapply(datalist, func)
x = lapply(x, function(x) {mutate(x, int = paste0(month, ".", day2))})
datasummary = lapply(x, function(x) {mutate(x, int2 =  factor(int, 
                                                              levels = c("10.1", "10.2", "10.3", "10.4",
                                                                         "11.1", "11.2", "11.3", "11.4",
                                                                         "12.1", "12.2", "12.3", "12.4",
                                                                         "1.1", "1.2", "1.3", "1.4",
                                                                         "2.1", "2.2", "2.3", "2.4",
                                                                         "3.1", "3.2", "3.3", "3.4")))})
plotlist = list()# new empty list
for(i in 1:length(datasummary)) {
  df1 = as.data.frame(datasummary[[i]])
  plotdf1 <- ggplot(df1, aes(x = int2, y = avg)) + 
    geom_line(data = df1, aes(group = east, linetype = east), size = 0.3) + 
    scale_linetype_manual(values=c("solid", "dashed")) +
    geom_errorbar(data=df1, aes(ymin=lci, ymax=uci, linetype = east), group = 2,  width = 0.25, size = 0.1, alpha = 0.5) +
    theme(axis.text.x = element_text(face="plain", color="black", 
                                     size=9, angle=0, vjust = 0),
          axis.text.y = element_text(face="plain", color="black", 
                                     size=9, angle=0)) +
    theme_bw() +
    ylab(paste(names(datasummary[i])))
  plotlist[[i]] <- plotdf1  # add each plot into plot list
}

g = do.call(grid.arrange, plotlist)

ggsave(file="results/envion_panel.pdf", g, width = 8, height = 6, units = "in")

####make separate plot for total precipitation (instead of average)
rf = as.data.frame(datasummary$rf)
p6 = ggplot(rf, aes(x = int2, y = sum)) + 
  geom_line(data = rf, aes(group = east, linetype = east), size = 0.5) + 
  scale_linetype_manual(values=c("solid", "twodash")) +
  geom_errorbar(data= rf, aes(ymin=lci, ymax=uci, linetype = east), group = 2,  width = 0.25, size = 0.1, alpha = 0.5) +
  scale_linetype_manual(values=c("solid", "twodash")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 10, color = "black")) + theme_bw()
  
ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

###alternate ploting method that uses two dataframes

data.avg = group_by(data, Site, month, day2)
tally(data.avg)
pg_env_avg = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt
pg_env_avg$int = paste0(pg_env_avg$month,".",pg_env_avg$day2)
pg_env_avg$int2 = factor(pg_env_avg$int, levels = c("10.1", "10.2", "10.3", "10.4",
                                                    "11.1", "11.2", "11.3", "11.4",
                                                    "12.1", "12.2", "12.3", "12.4",
                                                    "1.1", "1.2", "1.3", "1.4",
                                                    "2.1", "2.2", "2.3", "2.4",
                                                    "3.1", "3.2", "3.3", "3.4"))

p1 = ggplot(pg_env_avg, aes(x = int2, y = at)) + geom_point(aes(shape=Site), size = 1, color = "gray") + 
  scale_shape_manual(values=c(15, 16, 17, 3, 4)) +
  geom_line(aes(group = Site), color = "grey80") + 
  geom_line(data = pg_env_avg2, aes(group = east, linetype = east), size = 1) + scale_linetype_manual(values=c("solid", "twodash")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 10, color = "black")) + theme_bw()
