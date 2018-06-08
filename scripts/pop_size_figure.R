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
  group_by(Site, Year) %>% 
  summarise(mean(Diameter), mean(pg.at))
gavg

gavg = as.data.frame(gavg)

gavg$Diameter = gavg$`mean(Diameter)`
gavg$atemp = gavg$`mean(pg.at)`


g1 = ggplot(gavg, aes(x = Site)) + 
  facet_grid(. ~ Year) 

g2 = g1 + geom_point(aes(y = Diameter))

g3 = g2 + geom_point(aes(y = atemp))


gavg2 <- growth2 %>% 
  group_by(Site, Year) %>% 
  summarise(pg.at = mean(pg.at))
gavg2

g2 = g1 + geom_point(data = gavg2, stat = "identity", alpha = .3)

gavg <- growth %>% 
  group_by(pop.year) %>% 
  summarise(Diameter = mean(Diameter))
gavg

g1 = ggplot(growth, aes(pop.year, Diameter)) + geom_line(data = gavg, stat = "identity", alpha = .3)

g2 = 

growth2 <- read_excel("data/growth_germ.xlsx", sheet = 7)
growth2$Year = as.factor(growth2$Year) #convert year values into factor 

g2 = ggplot(growth2, aes(Site, pop_size)) + geom_point() + facet_grid(. ~ Year)


gx = gavg %>% ggplot() + 
  geom_bar(mapping = aes(x = Site, y = Diameter), stat = "identity", colour = gray(0.5), fill = gray(0.5)) + 
  geom_line(mapping = aes(x = Site, y = atemp)) + 
  geom_point(mapping = aes(x = Site, y = atemp), size = 3, shape = 21, fill = "white") +
  facet_grid(. ~ Year) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 20 / 15), limits = c(10, 20))

# https://whatalnk.github.io/r-tips/ggplot2-secondary-y-axis.nb.html

gx = gavg %>% ggplot() + 
  geom_bar(mapping = aes(x = Site, y = Diameter, colour = "Diameter"), stat = "identity", colour = gray(0.5), fill = gray(0.5)) + 
  geom_point(mapping = aes(x = Site, y = atemp), size = 3, shape = 21, fill = "black") +
  facet_grid(. ~ Year) + theme_classic()
  #scale_y_continuous(sec.axis = sec_axis(~ . * 20/15), limits = c(0, 15))


