#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")
#setwd("C:\\Users\\jas\\Google Drive\\data_analysis\\chorizanthe\\chori")

library(readxl)
library(ggplot2)
library(lme4)
library(dplyr)

# import growth data##########

growth <- read_excel("data/growth_germ.xlsx", sheet = 1)
growth$Year = as.factor(growth$Year) #convert year values into factor 

# Determine the spatio-temporal variation in Width------------------------------------------------------------------------

#Spatiotemporal variation in plant Width (Figure 2b)
gavg <- growth %>% 
  group_by(Site) %>% 
  summarise(Width = mean(Width))
gavg

###plot data (Figure 2)

width = ggplot(growth, aes(Site, Width)) + 
   geom_bar(data = gavg, stat = "identity", alpha = .3) + 
  facet_grid(.~ Year) +
  geom_jitter(width = 0.2, height = 0, size = 1) +
  theme_classic() +
  ggtitle("Figure 2") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust = 0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ylab("Plant Width (cm)") +
  xlab("Site")

ggsave(width, file="results/width.pdf", 
       width = 6, height = 4, units = "in")

####Linear regression of plant Width with sites as predictor

g.mod <- lm(Width ~ Site, data=growth)

anova.tab <- anova(g.mod)
anova.tab

summary(g.mod)

qqnorm(resid(g.mod))
