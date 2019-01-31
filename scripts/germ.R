#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")
#setwd("C:/Users/Administrator/Documents/jaspreet/chori/")

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
germ.sto$block = as.factor(germ.sto$block)

#Calculation of means, figures and regression ------------------------------------------------------------------------

#Comparison of germination across microhabitats and after-ripening treatments
#Hypothesis: Germination will be hihger for combination of easterly aspect and
# driest after-ripening 

##germination % across after-ripening treatements
gavg <- germ.sto %>% 
  group_by(trt) %>% 
  summarise(Germ = (sum(Germ)/1440)*100)
gavg

##germination % across micorhabitats
gavg <- germ.sto %>% 
  group_by(east) %>% 
  summarise(Germ = (sum(Germ)/2160)*100)
gavg

##germination % across after-ripening and micorhabitat interaction

germ.sto$int = interaction(germ.sto$trt, germ.sto$east)

gavg <- germ.sto %>% 
  group_by(int) %>% 
  summarise(Germ = (sum(Germ)/720)*100)
gavg

library(splitstackshape)
gavg = cSplit(gavg, "int", ".")
gavg = plyr::rename(gavg, c("int_1"= "trt", "int_2"= "east"))
  
g1 = ggplot(gavg, aes(trt, Germ)) +
    geom_bar(data = gavg, stat = "identity", alpha = .9) + facet_grid(.~ east)
g1

#cl = makeCluster(35) ###for running parallel jobs

gm1 <- afex::mixed(Germ ~ trt*east + (1 | Site/block), family = binomial, nAGQ = 1L, 
                   control = glmerControl(optimizer="bobyqa",
                                          boundary.tol = 1e-2,
                                          check.conv.singular = .makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2),
                   method = "PB", data = germ.sto, args_test = list(nsim = 10000), cl = cl)

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
#levels in the factor.  There are alternatives, however, and it can be
#interesting to explore them.

#Comparison of plant diameter across sites and stoarge treatmnets....

germ.sto.dia = subset(germ.sto, Germ > 0)
germ.sto.dia$int = interaction(germ.sto.dia$trt, germ.sto.dia$east)

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

gavg2 = cSplit(gavg2, "int", ".")
gavg2 = plyr::rename(gavg2, c("int_1"= "trt", "int_2"= "east"))

g2 = ggplot(gavg2, aes(trt, Diameter)) +
  geom_bar(data = gavg2, stat = "identity", alpha = .9) + facet_grid(.~ east)

gm1 <- afex::mixed(Diameter ~ trt*east + (1 | Site/block), method = "KR", data = germ.sto.dia)

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
germ.env$block = as.factor(germ.env$block)
germ.env$replicate = as.factor(germ.env$replicate)

# Visual comparisons and regression ------------------------------------------------------------------------

#Hypothesis: Germination will be hihger for seeds suplied with supplemental moisture and shade.

germ.env$int = interaction(germ.env$Site, germ.env$shade, germ.env$water)

gavg <- germ.env %>% 
  group_by(shade) %>% 
  summarise(Germ = (sum(Germ)/960)*100)
gavg

gavg <- germ.env %>% 
  group_by(water) %>% 
  summarise(Germ = (sum(Germ)/960)*100)
gavg

gavg <- germ.env %>% 
  group_by(Site) %>% 
  summarise(Germ = (sum(Germ)/960)*100)
gavg

gavg <- germ.env %>% 
  group_by(int) %>% 
  summarise(Germ = (sum(Germ)/240)*100)
gavg

#install.packages("splitstackshape")
library(splitstackshape)
gavg = cSplit(gavg, "int", ".")
gavg = plyr::rename(gavg, c("int_1"= "site", "int_2"= "shade", "int_3" = "water"))
gavg$int = interaction(gavg$shade, gavg$water)

g1 = ggplot(gavg, aes(int, Germ)) +
  geom_bar(data = gavg, stat = "identity", alpha = .9) + facet_grid(.~ site)

gm1 <- afex::mixed(Germ ~ shade*water*Site + (1 | block/replicate), family = binomial, nAGQ = 1L, 
                   control = glmerControl(optimizer="bobyqa",
                                          boundary.tol = 1e-2,
                                          check.conv.singular = .makeCC(action="ignore",tol=1e-2), tolPwrss=1e-2),
                   method = "PB", data = germ.env, args_test = list(nsim = 10000), cl = cl)

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
  group_by(shade) %>% 
  summarise(Diameter = mean(Diameter))
gavg2

gavg2 <- germ.env.dia %>% 
  group_by(water) %>% 
  summarise(Diameter = mean(Diameter))
gavg2

gavg2 <- germ.env.dia %>% 
  group_by(int) %>% 
  summarise(Diameter = mean(Diameter))
gavg2

g2 = ggplot(germ.env.dia, aes(int, Diameter)) + geom_point() + 
   geom_bar(data = gavg2, stat = "identity", alpha = .3)

gm1 <- afex::mixed(Diameter ~ shade*water + (1 | block/replicate), method = "KR", data = germ.env.dia)

anova.tab <- anova(gm1)
anova.tab

coef.tab <- summary(gm1)$coefficients
coef.tab
