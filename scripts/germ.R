#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")
setwd("C:/Users/Administrator/Documents/jaspreet_kaur/chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)
library(dplyr)
library(pbkrtest)
library(parallel)
library(devtools) 
#install_github("lme4/lme4",dependencies=TRUE)

# import after-ripening germination data -----------------------------------------

germ.sto <- read_excel("data/growth_germ.xlsx", sheet = 4)
germ.sto$Year <- factor(germ.sto$Year)
germ.sto$Diameter <- round(germ.sto$Diameter, digits = 0)
germ.sto$east <- germ.sto$Site =="EO12g"|germ.sto$Site=="NPS2"

#Visual comparisons and regression ------------------------------------------------------------------------

#Comparison of germination across native/foreign sites, storage treatmnets and soil temperature

#Hypothesis: Germination will be hihger for seeds germination will be highest among seeds 
#after-ripened in ambient conditions in native soil in comparison to seeds 
#after-ripened under modified controlled environments during the period of dormancy

#Hypothesis: seeds introduced at foreign sites will yield similar germination 
#ant fitness as the native donor sites with the primary regulation 
#by microenvironment of the experimental site

ggplot(germ.sto, aes(aspect, Germ, color=Year)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto, aes(trt, Germ, color=Year)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)
germ.sto$int = interaction(germ.sto$trt, germ.sto$east)

gavg <- germ.sto %>% 
  group_by(trt) %>% 
  summarise(Germ = (sum(Germ)/1440)*100)
gavg

gavg <- germ.sto %>% 
  group_by(east) %>% 
  summarise(Germ = (sum(Germ)/2160)*100)
gavg

gavg <- germ.sto %>% 
  group_by(int) %>% 
  summarise(Germ = (sum(Germ)/720)*100)
gavg

g1 = ggplot(gavg, aes(int, Germ)) + facet_grid(.~ east)
  geom_bar(data = gavg, stat = "identity", alpha = .9)

cl = makeCluster(35)

gm1 <- afex::mixed(Germ ~ trt*aspect + (1 | Site) + (1 | Year), family = binomial, nAGQ = 1L, 
                   control = glmerControl(optimizer="bobyqa",
                                          boundary.tol = 1e-2,
                                          check.conv.singular = .makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2),
                   method = "PB", data = germ.sto, args_test = list(nsim = 10), cl = cl)

anova.tab <- anova(gm1)
anova.tab

coef.tab <- summary(gm1)$coefficients
coef.tab

summary(gm1)

# (1 + open|school/class) tells R to fit a varying slope and varying intercept model for schools and classes nested within schools
# (0 + open|school/class) tells R to fit a varying slope model for schools and classes nested within schools
#the random slope model allows the explanatory variable to have a different effect for each group
# the coefficients associated with a categorical factor are usually expressed as an
#intercept and a set of k-1 "contrasts", where k is the number of
l#evels in the factor.  There are alternatives, however, and it can be
#interesting to explore them.


#Comparison of plant diameter across sites and stoarge treatmnets....

germ.sto.dia = subset(germ.sto, Germ > 0)
germ.sto.dia$int = interaction(germ.sto.dia$trt, germ.sto.dia$Native)

gavg2 <- germ.sto.dia %>% 
  group_by(trt) %>% 
  summarise(Diameter = mean(Diameter))
gavg2

gavg2 <- germ.sto.dia %>% 
  group_by(east) %>% 
  summarise(Diameter = mean(Diameter))
gavg2

gavg2 <- germ.sto.dia %>% 
  group_by(int) %>% 
  summarise(Diameter = mean(Diameter))
gavg2

g2 = ggplot(germ.sto.dia, aes(int, Diameter)) + geom_point() + 
  geom_bar(data = gavg2, stat = "identity", alpha = .3)

ggplot(germ.sto.dia, aes(Site, Diameter, color=Year)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto.dia, aes(trt, Diameter, color=Year)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)

gm1 <- afex::mixed(Diameter ~ trt*Native + (1 | Site) + (1 | Year), method = "KR", data = germ.sto)

anova.tab <- anova(gm1)
anova.tab

coef.tab <- summary(gm1)$coefficients
coef.tab

# import micorenvironment germination data --------------------------------------------

germ.env <- read_excel("data/growth_germ.xlsx", sheet = 3)
germ.env$Year = factor(germ.env$Year)
germ.env$Site = factor(germ.env$Site)
germ.env$shade = factor(germ.env$shade)
germ.env$water = factor(germ.env$water)
germ.env.17 <- subset(germ.env, Year == "2017")

# Visual comparisons and regression ------------------------------------------------------------------------

#Hypothesis: Germination will be hihger for seeds suplied with supplemental moisture and shade.

germ.env$int = interaction(germ.env$shade, germ.env$water)
ggplot(germ.env, aes(int, Germ)) + geom_jitter(width=0.2, height=0.1)

gavg <- germ.env %>% 
  group_by(int) %>% 
  summarise(Germ = (sum(Germ)/480)*100)
gavg

gavg <- germ.env %>% 
  group_by(Year) %>% 
  summarise(Germ = mean(pg.sm2))
gavg

g1 = ggplot(gavg, aes(int, Germ)) +
  geom_bar(data = gavg, stat = "identity", alpha = .9)

gm1 <- afex::mixed(Germ ~ shade*water + (1 | Site) + (1 | Year), family = binomial, nAGQ = 1L, 
                   control = glmerControl(optimizer="bobyqa",
                                          boundary.tol = 1e-2,
                                          check.conv.singular = .makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2),
                   method = "PB", data = germ.env, args_test = list(nsim = 1000), cl = cl)

anova.tab <- anova(gm1)
anova.tab

coef.tab <- summary(gm1)$coefficients
coef.tab

summary(gm1)

###Comparison of plant diameter

#Hypothesis: Diameter will be hihger for plants suplied with supplemental moisture and shade.

germ.env.dia = subset(germ.env.17, Germ > 0)
germ.env.dia$int = interaction(germ.env.dia$shade, germ.env.dia$water)

gavg2 <- germ.env.dia %>% 
  group_by(int) %>% 
  summarise(Diameter = mean(Diameter))
gavg2

g2 = ggplot(germ.env.dia, aes(int, Diameter)) + geom_point() + 
   geom_bar(data = gavg2, stat = "identity", alpha = .3)

gm1 <- afex::mixed(Diameter ~ shade*water + (1 | Site), method = "KR", data = germ.env.17)

anova.tab <- anova(gm1)
anova.tab

coef.tab <- summary(gm1)$coefficients
coef.tab
