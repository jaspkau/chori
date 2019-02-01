#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")

library(readxl)
library(ggplot2)
library(lme4)
library(dplyr)

#########Imprt data for Population size figure (Fig. 2a)

growth <- read_excel("data/growth_germ.xlsx", sheet = 7)
growth$Year = as.factor(growth$Year) #convert year values into factor 

growth$pop.year = paste(growth$Site,".", growth$Year)
growth$pop.year = gsub(" ", "", growth$pop.year)

ggplot(growth, aes(pop.year, pop_size)) + geom_bar(stat = "identity") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, 
                                   size = 9, color = "black")) +
  ylab("Population size") +
  xlab("Site")

# import growth data##########

growth <- read_excel("data/growth_germ.xlsx", sheet = 1)
growth$Year = as.factor(growth$Year) #convert year values into factor 

# Determine the spatio-temporal variation in Width------------------------------------------------------------------------

#Spatiotemporal variation in plant Width (Figure 2b)
gavg <- growth %>% 
  group_by(Site) %>% 
  summarise(Width = mean(Width))
gavg

ggplot(growth, aes(Site, Width)) + geom_point() + 
   geom_bar(data = gavg, stat = "identity", alpha = .3) + 
  facet_grid(.~ Year) +
  geom_jitter(width = 0.2, height = 0) +
  theme_classic() +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust = 0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ylab("Plant Width (cm)") +
  xlab("Site")

####Regression of plant Width with sites and years

g.mod <- lm(Width ~ Site, data=growth)

anova.tab <- anova(g.mod)
anova.tab

summary(g.mod)

qqnorm(resid(g.mod))
