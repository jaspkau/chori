#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")

library(readxl)
library(ggplot2)
library(lme4)
library(Hmisc)

# import growth data##########

growth <- read_excel("data/growth_germ.xlsx", sheet = 1)
growth$Year = as.factor(growth$Year) #convert year values into factor 

##subset dataframe for fecundity data which is only avaibale for 2016 and 2017 study years
growth_inv = subset(growth, Year == 2016 | Year == 2017)
#correlation between plant diameter and branches

keep = c("Diameter", "Branches", "Involucres")
df1 = growth[,keep]
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

qplot(Branches, 
     Diameter, 
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

ggscatter(df1, x = "Branches", y = "Diameter",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(df1, x = "Involucres", y = "Diameter",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman")