#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori")
#setwd("C:\\Users\\jas\\Google Drive\\data_analysis\\chorizanthe\\chori")

library(readxl)
library(ggplot2)
library(lme4)
library(dplyr)

# import growth data##########

growth <- read_excel("data/growth_germ.xlsx", sheet = 1)
growth$Year = as.factor(growth$Year) 
growth$aspect = ifelse(growth$Site %in% c("EO12", "EO16"), "Easterly", "Westerly")

# Determine the spatio-temporal variation in Width------------------------------------------------------------------------

#Spatial variation in plant Width (between two aspects) (Figure 2b)
gavg <- growth %>% 
  group_by(aspect) %>%
summarise(avg = mean(Width, na.rm = TRUE),
          sd = sd(Width, na.rm = TRUE),
          n = n(),
          se = sd/ sqrt(n),
          lci = avg-1.96*se,
          uci = avg+1.96*se) %>%
  rename(Width = avg)

gavg

###plot data (Figure 2)

width = ggplot(growth, aes(aspect, Width)) + 
   #geom_bar(data = gavg, stat = "identity", width = 0.25, alpha = .3) + 
  #geom_crossbar(data=gavg, aes(ymin=lci, ymax=uci), group = 2,  
                #width = 0.2, size = 0.6, alpha = 0.8, color = "blue") +
  geom_jitter(width = 0.1, height = 0, size = 0.2) +
  theme_classic() +
  ggtitle("Figure 2a") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust = 0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ylab("Plant Width (cm)") +
  xlab("Aspect") +
  #geom_errorbar(data=gavg, aes(ymin=lci, ymax=uci), group = 2,  width = 0.25, size = 0.1, alpha = 0.5) #+
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", color = "black", width = 0.2, size = 0.6) #fun.args = list(mult = 1)

width

ggsave(width, file="results/width.pdf", 
       width = 4, height = 4, units = "in")

####Linear regression of plant Width with sites as predictor

g.mod <- lm(Width ~ aspect, data=growth)

anova.tab <- anova(g.mod)
anova.tab

summary(g.mod)

qqnorm(resid(g.mod))

#Hypothesis for spatiotemporal variation in plant growth: 
#Vegetative and reproductive fitness of plants is better at low air temperature
#Spatiotemporal variation in air temperature

growth$pop.year = paste0(growth$Site, growth$Year)
remove = c("EO162018", "EO132017", "EO132018")
growth2 = subset(growth, !(pop.year %in% remove))

gavg2 <- growth2 %>% 
  group_by(aspect, Year) %>% 
  summarise(Width = mean(Width), atemp = mean(pg.at))
gavg2

width.at = ggplot(gavg2, aes(atemp, Width, group = aspect, shape=Year)) + geom_point() + 
  geom_line(aes(linetype = aspect)) + 
  theme_classic() +
  ggtitle("Figure 2b") +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0, vjust = 0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ylab("Plant Width (cm)") +
  xlab("Air temp (°C)")
width.at

Fig2 = ggpubr::ggarrange(width, width.at,
          ncol = 2, nrow = 1, common.legend = FALSE, legend = "none")

ggsave(Fig2, file="results/width.at.pdf", 
       width = 8, height = 3, units = "in")

#ggplot(growth2, aes(pg.ppt, Width, color=Site, shape=Year)) + geom_jitter(width=0.01, height=0)
#Regression of Width with air temp

g.mod <- lm(Width ~ pg.at, data=growth2)
summary(g.mod)
anova(g.mod)
qqnorm(resid(g.mod))

#####Population size analyses

popsize <- read_excel("data/growth_germ.xlsx", sheet = 7)
popsize$Year = as.factor(popsize$Year) 
popsize$aspect = ifelse(popsize$Site %in% c("EO12", "EO16"), "Easterly", "Westerly")

k = kruskal.test(popsize$pop_size ~ popsize$aspect)
k
