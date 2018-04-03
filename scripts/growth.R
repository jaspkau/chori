#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\mar2018/")

# import growth data##########

library(readxl)
growth <- read_excel("data/growth_germ.xlsx", sheet = 1)
growth$Year = as.factor(growth$Year) #convert year values into factor 

##subset dataframe for fecundity data which is only avaibale for 2016 and 2017 study years
growth_inv = subset(growth, Year == 2016 | Year == 2017)

# Determine the spatio-temporal variation in Diameter and Fecundity along with their potential predictors------------------------------------------------------------------------

library(ggplot2)

#Spatiotemporal variation in plant diameter
ggplot(growth, aes(Site, Diameter)) + geom_point() + facet_grid(. ~ Year)

g.mod <- glm(Diameter ~ Site*Year, data=growth)
summary(g.mod)
anova(g.mod)

#Spatiotemporal variation in fecundity

ggplot(growth_inv, aes( Site, Involucres)) + geom_point() + facet_grid(. ~ Year)

g.mod <- glm(Involucres ~ Site*Year, family = poisson, data=growth_inv)
summary(g.mod)
anova(g.mod)

#Hypothesis for spatiotemporal variation in plant growth: 

#Vegetative and reproductive fitness of plants is better at low air temperature

#Spatiotemporal variation in air temperature
ggplot(growth, aes(Site, pg.at)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

#Regression of Diameter with air temp
g.mod <- glm(Diameter ~ pg.at, data=growth)
summary(g.mod)
anova(g.mod)

#Regression of Fecundity with air temp
g.mod <- glm(Involucres ~ pg.at, data=growth_inv)
summary(g.mod)
anova(g.mod)

# Pricipal component with soil variables and regression of PCs with plant diameter ---------------------------------

growth_avg <- read_excel("data/growth_germ.xlsx", sheet = 2)
X = cbind(growth_avg[,6:25])
X = scale(X, center = TRUE, scale = TRUE)
X = na.omit(X)                                    

pca.results = prcomp(X, scale=F)               
pca.results

s = summary(pca.results)

screeplot(pca.results, type="lines", main="PCA Scree Plot")  

library(ggbiplot)
site = growth_avg$Site
year = as.factor(growth_avg$Year)
g <- ggbiplot(pca.results, groups = site, size = 3)+ geom_point(aes(color = site, shape = year ), size = 3.5) +
  theme_bw(base_size = 15) +
  labs(x = paste("PC1 Scores (", as.character(round(100*s$importance[2,1])),"%)",sep=''),
       y = paste("PC2 Scores (", as.character(round(100*s$importance[2,2])),"%)",sep=''))
g


#Hypotheses for spatiotemporal variation in edaphic characteristic based on PCA

#Vegetative and reproductive fitness of plants is better on soil with high Ca, P and PH

g.mod <- glm(Diameter ~ CA + P2 + PH, data=growth)
summary(g.mod)
anova(g.mod)
