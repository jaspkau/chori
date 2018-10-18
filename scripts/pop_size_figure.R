#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")

library(readxl)
library(ggplot2)
library(lme4)
library(dplyr)

# import growth data##########

growth <- read_excel("data/growth_germ.xlsx", sheet = 7)
growth$Year = as.factor(growth$Year) #convert year values into factor 

growth$pop.year = paste(growth$Site,".", growth$Year)
growth$pop.year = gsub(" ", "", growth$pop.year)

ggplot(growth, aes(pop.year, pop_size)) + geom_bar(stat = "identity") +
   theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 10, color = "black"))

# Determine the spatio-temporal variation in Diameter and Fecundity along with their potential predictors------------------------------------------------------------------------

#Spatiotemporal variation in plant diameter
gavg <- growth %>% 
  group_by(Site) %>% 
  summarise(pop_size = mean(pop_size))
gavg

ggplot(growth, aes(Site, pop_size)) + geom_point() + 
  geom_bar(data = gavg, stat = "identity", alpha = .3)