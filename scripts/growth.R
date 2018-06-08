#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")

library(readxl)
library(ggplot2)
library(lme4)
library(dplyr)

# import growth data##########

growth <- read_excel("data/growth_germ.xlsx", sheet = 1)
growth$Year = as.factor(growth$Year) #convert year values into factor 

# Determine the spatio-temporal variation in Diameter and Fecundity along with their potential predictors------------------------------------------------------------------------

#Spatiotemporal variation in plant diameter
gavg <- growth %>% 
  group_by(Site, Year) %>% 
  summarise(Diameter = mean(Diameter))
gavg

ggplot(growth, aes(Site, Diameter)) + geom_point() + 
  facet_grid(. ~ Year) + geom_bar(data = gavg, stat = "identity", alpha = .3)

g.mod <- lm(Diameter ~ Site*Year, data=growth)
summary(g.mod)
anova(g.mod)
qqnorm(resid(g.mod))

#Hypothesis for spatiotemporal variation in plant growth: 
#Vegetative and reproductive fitness of plants is better at low air temperature
#Spatiotemporal variation in air temperature

growth$pop.year = paste(growth$Site,".", growth$Year)
growth2 = subset(growth, pop.year != "EO16 . 2018")

gavg2 <- growth %>% 
  group_by(Site, Year) %>% 
  summarise(Diameter = mean(Diameter), atemp = mean(pg.at))
gavg2

ggplot(gavg2, aes(atemp, Diameter, group = Site, color=Site, shape=Year)) + geom_point() + 
  geom_line() +
  geom_jitter(width=0.02, height=0.001)

#ggplot(growth2, aes(pg.ppt, Diameter, color=Site, shape=Year)) + geom_jitter(width=0.01, height=0)
#Regression of Diameter with air temp

g.mod <- lmer(Diameter ~ pg.at + (1 | Site) + (1 | Year), data=growth2)
g.mod.null <- lmer(Diameter ~ 1 + (1 | Site) + (1 | Year), data=growth2)
anova(g.mod.null, g.mod)

g.mod <- lm(Diameter ~ pg.at, data=growth2)
summary(g.mod)
anova(g.mod)
qqnorm(resid(g.mod))

# Pricipal component with soil variables and regression of PCs with plant diameter ---------------------------------

growth_avg <- read_excel("data/growth_germ.xlsx", sheet = 2)
row.names(growth_avg) = growth_avg$Code
X = growth_avg[,-1]
X = X[,3:22]
row.names(X) = row.names(growth_avg)
X = scale(X, center = TRUE, scale = TRUE)
X = na.omit(X)                                     

pca.results = prcomp(X, scale=F)               
pca.results

s = summary(pca.results)

site.scores = predict(pca.results) #or s$x also gives site scores

site.scores

screeplot(pca.results, type="lines", main="PCA Scree Plot")  

library(ggbiplot)
site = growth_avg$Site
year = as.factor(growth_avg$Year)
g <- ggbiplot(pca.results, groups = site, size = 3)+ geom_point(aes(color = site, shape = year ), size = 3.5) +
  theme_bw(base_size = 15) +
  labs(x = paste("PC1 Scores (", as.character(round(100*s$importance[2,1])),"%)",sep=''),
       y = paste("PC2 Scores (", as.character(round(100*s$importance[2,2])),"%)",sep=''))
g

####different kind of plot
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#install.packages("factoextra")
library(factoextra)
site = growth_avg$Site
year = as.factor(growth_avg$Year)
fviz_pca_ind(pca.results,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca.results, repel = TRUE,
                col.var = "red", # Variables color
                col.ind = "black"  # Individuals color
)

