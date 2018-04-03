#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\mar2018/")

# import AR germination data -----------------------------------------

library(readxl)
germ.sto <- read_excel("data/growth_germ.xlsx", sheet = 4)

germ.sto$Diameter = round(germ.sto$Diameter, digits = 0)

germ.sto.16 <- subset(germ.sto, Year == 2016)
germ.sto.17 <- subset(germ.sto, Year == 2017)

#Visual comparisons of and regression ------------------------------------------------------------------------

library(ggplot2)
library(pscl)

#Comparison of germination across sites and storage treatmnets in 2016

#Hypothesis: Germination will be higher at site with extant
#population of Chorizanthe (E012) in comparion to new introduction
#sites (EO14, NPS1, NPS2).Germination will be higher among seeds stored in
#ambient conditions in native soil (SS) in comparison to seeds stored 
#at a constant temperature and relative humidity (RTSI and RTNSI). Seeds stored at high relative
#humidity (RTNSI) and for longer period (2013) of time will have lower germination.

ggplot(germ.sto.16, aes(Site, Germ)) + geom_jitter(width=0.2, height=0.1)

ggplot(germ.sto.16, aes(trt, Germ)) + geom_jitter(width=0.2, height=0.1)

g.mod <- glm(Germ ~ Site + trt, family = poisson, data=germ.sto.16)
summary(g.mod)
anova(g.mod)

#Comparison of plant diameter across sites and stoarge treatmnets in 2016

#Hypothesis: Plant Diameter will be higher at site with extant
#population of Chorizanthe (E012) in comparions to new introduction
#sites (EO14, NPS1, NPS2), whereas plant Diameter will be similar across storage treatments

ggplot(germ.sto.16, aes(Site, Diameter)) + geom_jitter(width=0.2, height=0.1)

ggplot(germ.sto.16, aes(trt, Diameter)) + geom_jitter(width=0.2, height=0.1)

g.mod <- glm(Diameter ~ Site + trt, data=germ.sto.16)
summary(g.mod)
anova(g.mod)

##Comparison of germination across sites and storage treatments in 2017

#Hypotheses: Germination will be higher at site with extant
#population of Chorizanthe (EO12) in comparions to new introduction
#sites (EO14, NPS1, NPS2).Germination will be higher among seeds stored in
#ambient conditions in native soil (SS12 and SS14) in comparison to seeds stored 
#at a constant temperature and relative humidity (RTSI and RTNSI). Seeds stored at high relative
#humidity or soil moisture will have lower germination.

ggplot(germ.sto.17, aes(Site, Germ)) + geom_jitter(width=0.2, height=0.1)

ggplot(germ.sto.17, aes(trt, Germ)) + geom_jitter(width=0.2, height=0.1)

g.mod <- glm(Germ ~ Site + trt, family = poisson, data=germ.sto.17)
summary(g.mod)
anova(g.mod)

#Comparison of plant Diameter across sites and storage treatmnets in 2017

#Hypothesis: Plant Diameter will be higher at site with extant
#population of Chorizanthe (E012) in comparions to new introduction
#sites (EO14, NPS1, NPS2), whereas plant Diameter will be similar across storage treatments

ggplot(germ.sto.17, aes(Site, Diameter)) + geom_jitter(width=0.2, height=0.1)

ggplot(germ.sto.17, aes(trt, Diameter)) + geom_jitter(width=0.2, height=0.1)

g.mod <- glm(Diameter ~ Site + trt, data=germ.sto.17)
summary(g.mod)
anova(g.mod)

#Hypothesis for spatial variation in germination: germination is better 
# at sites with low soil temeprature during dormancy release period

ggplot(germ.sto, aes(Site, dr.st)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

g.mod <- glm(Germ ~ dr.st, data=germ.sto.16)
summary(g.mod)
anova(g.mod)

g.mod <- glm(Germ ~ dr.st, data=germ.sto.17)
summary(g.mod)
anova(g.mod)

#Hypothesis for spatial variation in plant diameter: germination is better 
# at sites with low air temeprature during plant growth period

ggplot(germ.sto, aes(Site, pg.at)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

g.mod <- glm(Diameter ~ pg.at, data=germ.sto.16)
summary(g.mod)
anova(g.mod)

g.mod <- glm(Diameter ~ pg.at, data=germ.sto.17)
summary(g.mod)
anova(g.mod)

# import micorenvironment data --------------------------------------------

germ.env <- read_excel("data/growth_germ.xlsx", sheet = 3)

germ.env.15 <- subset(germ.env, Year == 2015)
germ.env.17 <- subset(germ.env, Year == 2017)

# Visual comparisons and regression ------------------------------------------------------------------------

#2015 germination

#Hypothesis: Germination will be hihger at site with extant
#population of Chorizanthe (E012) in comparions to control site and 
#for seeds suplied with supplemental moisture and shade.Germination will be hihger for seeds with 
#scarified involucres

ggplot(germ.env.15, aes(Site, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

ggplot(germ.env.15, aes(water, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

ggplot(germ.env.15, aes(shade, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

ggplot(germ.env.15, aes(involucre, Germ)) + geom_jitter(width=0.2, height=0.1)

g.mod <- glm(Germ ~ Site + water + shade + involucre, family = poisson, data=germ.env.15)
summary(g.mod)
anova(g.mod)


#2017 germination

#Hypothesis: Germination will be higher at site with extant
#population of Chorizanthe (E012) in comparions to control site and 
#for seeds suplied with supplemental moisture and shade

ggplot(germ.env.17, aes(Site, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

ggplot(germ.env.17, aes(water, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

ggplot(germ.env.17, aes(shade, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

g.mod <- glm(Germ ~ Site + water + shade, family = poisson, data=germ.env.17)
summary(g.mod)
anova(g.mod)

