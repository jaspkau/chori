#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")

library(readxl)
library(ggplot2)
library(lme4)
library(Hmisc)

# import growth data##########

data<-read.xls("data/Book1.xlsx",
               sheet=3,verbose=TRUE,na.strings="N/A")

data$date<-as.POSIXct(as.character(data$date),format="%m/%d/%Y",
                      tz="America/Los_Angeles")
data$year = year(data$date)
data$month = month(data$date)
##subset dataframe for fecundity data which is only avaibale for 2016 and 2017 study years
data2 = subset(data, year == 2015 | year == 2016 | year == 2017)
#correlation between plant diameter and branches

gavg <- data2 %>% 
  group_by(variable, month, year) %>% 
  summarise(at = mean(at), st = mean(st), rh = mean(rh), sm2 = mean(sm2), rf = mean(rf), par = mean(par))
gavg

keep = c("at", "rh")
keep = c("at", "st", "sm2", "rh", "par", "rf")
df1 = data[,keep]
df1 = as.matrix(df1)
r = rcorr(df1, type = c("spearman"))
df1 = as.data.frame(df1)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(r$r, r$P)

qplot(at, 
      st, 
      data = df1, 
      geom = c("point", "smooth"), 
      method = "lm", 
      alpha = I(1 / 5), 
      se = TRUE)

qplot(Involucres, 
      Diameter, 
      data = df1, 
      geom = c("point", "smooth"), 
      method = "lm", 
      alpha = I(1 / 5), 
      se = TRUE)

require(ggpubr)
require(tidyverse)
require(Hmisc)
require(corrplot)

##check the normality before deciding on correlation method
#if normal, use pearson correlation otherwise use spearman or kendall

shapiro.test(df1$Diameter)
shapiro.test(df1$Branches)

ggscatter(df1, x = "at", y = "st",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(df1, x = "at", y = "sm2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(df1, x = "at", y = "rf",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(df1, x = "at", y = "",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")