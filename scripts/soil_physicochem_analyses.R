#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)
library(dplyr)
library(devtools)
#install_github("vqv/ggbiplot")
#install.packages("factoextra")
library(factoextra)

# Pricipal component with soil variables ---------------------------------

germ_avg <- read_excel("data/growth_germ.xlsx", sheet = 6)
row.names(germ_avg) = germ_avg$Code
X = germ_avg[,-1]
X = X[,4:23]
row.names(X) = row.names(germ_avg)
X = scale(X, center = TRUE, scale = TRUE)
X = na.omit(X)  

pca.results = prcomp(X, scale=F)               
pca.results

s = summary(pca.results)

site.scores = predict(pca.results)

site.scores

screeplot(pca.results, type="lines", main="PCA Scree Plot")  

####different kind of plot
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

####plot data (Figure. 7a)

p = fviz_pca_biplot(pca.results, geom.var = "text", col.var = "black",
                geom.ind = "point", pointsize = 2, habillage = germ_avg$Site, mean.point = FALSE,
                repel=TRUE, addEllipses = TRUE, ellipse.type = "confidence",
                ggtheme = theme_classic()) + 
  scale_shape_manual(values=c(18,17,16,15,1,0,5))
