#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")

library(readxl)
library(ggplot2)
library(lme4)
library(dplyr)

# import growth data##########

growth <- read_excel("data/growth_germ.xlsx", sheet = 1)
growth$Year = as.factor(growth$Year) #convert year values into factor 

# Determine the spatio-temporal variation in Diameter------------------------------------------------------------------------

#Spatiotemporal variation in plant diameter (Figure 2b)
gavg <- growth %>% 
  group_by(Site, Year) %>% 
  summarise(Diameter = mean(Diameter))
gavg

ggplot(growth, aes(Site, Diameter)) + geom_point() + 
  facet_grid(. ~ Year) + geom_bar(data = gavg, stat = "identity", alpha = .3)

####Regression of plant diameter with sites and years

g.mod <- lm(Diameter ~ Site + Year + Site*Year, data=growth)
summary(g.mod)
anova(g.mod)
qqnorm(resid(g.mod))

#Spatiotemporal variation in air temperature (Figure 2c)

growth$pop.year = paste(growth$Site,".", growth$Year)
growth2 = subset(growth, pop.year != "EO16 . 2018")

gavg2 <- growth %>% 
  group_by(Site, Year) %>% 
  summarise(Diameter = mean(Diameter), atemp = mean(pg.at))
gavg2

ggplot(gavg2, aes(atemp, Diameter, group = Site, color=Site, shape=Year)) + geom_point() + 
  geom_line() +
  geom_jitter(width=0.02, height=0.001)

####Regression of plant diameter with air temperature

g.mod <- lm(Diameter ~ pg.at, data=growth)
summary(g.mod)
anova(g.mod)
qqnorm(resid(g.mod))


