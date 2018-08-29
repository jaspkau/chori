#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)
library(dplyr)

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

library(ggbiplot)
site = germ_avg$Site
year = as.factor(germ_avg$Year)
g <- ggbiplot(pca.results, size = 3)+ geom_point(aes(shape = site), size = 3.5) +
  theme_bw(base_size = 15) + scale_shape_manual(values=c(16,1,17,2,15,0,5)) + 
  labs(x = paste("PC1 Scores (", as.character(round(100*s$importance[2,1])),"%)",sep=''),
       y = paste("PC2 Scores (", as.character(round(100*s$importance[2,2])),"%)",sep=''))
g

####different kind of plot
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#install.packages("factoextra")
library(factoextra)

p = fviz_pca_biplot(pca.results, geom.var = "text", col.var = "black",
                geom.ind = "point", pointsize = 2, habillage = germ_avg$Site, mean.point = FALSE,
                repel=TRUE, addEllipses = TRUE, ellipse.type = "confidence") + scale_shape_manual(values=c(18,17,16,15,1,0,5))

p1 = fviz_contrib(pca.results, choice="var", axes = 1)
p2 = fviz_contrib(pca.results, choice="var", axes = 2)
p3 = fviz_contrib(pca.results, choice="ind", axes = 1)
p4 = fviz_contrib(pca.results, choice="ind", axes = 2)

ggpubr::ggarrange(p1, p2, p3, p4,
          ncol = 2, nrow = 2)

#########Krsukal wallis test

require(partykit)

##final will store final results
soil <- as.data.frame(read_excel("data/growth_germ.xlsx", sheet = 6))
soil$int = paste(soil$Site,".",soil$Year)
soil$int = gsub(" ", "", soil$int)

final = c()
for(i in 5:24){
  column = names(soil[i])
  k.year = kruskal.test(soil[,i]~ as.factor(soil$Year), data = soil)$"p.value"
  k.pop = kruskal.test(soil[,i]~ as.factor(soil$Site), data = soil)$"p.value"
  k.py = kruskal.test(soil[,i]~ as.factor(soil$int), data = soil)$"p.value"
  results = data.frame(otu = paste(column), pval.year = as.numeric(paste(k.year)), pval.pop = as.numeric(paste(k.pop)), pval.py = as.numeric(paste(k.py)))
  final = rbind(final, results)
} 

final$pad.year = p.adjust(final$pval.year, method = "bonferroni")
final$pad.pop = p.adjust(final$pval.pop, method = "bonferroni")
final$pad.py = p.adjust(final$pval.py, method = "bonferroni")
write.csv(final, file='results/soil_kw.csv')

####Mean sepration with Dunn test
library(FSA) ###for multiple pairwise comparisons after kw
DT = dunnTest(soil[,12] ~ as.factor(soil$Site), data=soil, method="bonferroni")

library(rcompanion) ##for compact letter display after dunn
DT = DT$res
p.ad = cldList(P.adj ~ Comparison, data = DT, threshold = 0.05)
p.ad

###site average for variables
data.avg = group_by(soil, Site)
tally(data.avg)
soil.tab = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt
