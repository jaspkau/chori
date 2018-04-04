#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\mar2018/")

# import AR germination data -----------------------------------------

library(readxl)
# DWS: you should document that code expects working directory to be root of
# the git repo. This is fine (less convenient for me than if working dir is
# scripts dir, but that is fine as long as documented.
germ.sto <- read_excel("data/growth_germ.xlsx", sheet = 4)
germ.sto$Year <- factor(germ.sto$Year)
germ.sto$Diameter <- round(germ.sto$Diameter, digits = 0)
germ.sto$Native <- germ.sto$Site=="EO12"
germ.sto <- subset(germ.sto, trt != "2013")

germ.sto <- subset(germ.sto, trt == "RTSI" | trt== "RTNSI")

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

ggplot(germ.sto, aes(Site, Germ)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto, aes(Native, Germ, color=Year)) + geom_jitter(width=0.2, height=0.1)

ggplot(germ.sto, aes(trt, Germ, color=Year)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)

ggplot(germ.sto.16, aes(trt, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)

g.mod <- glmer(Germ ~ trt + (trt | Site) + (1 | Year), family = poisson, data=germ.sto)
g.mod.null <- glmer(Germ ~ 1 + (trt | Site) + (1 | Year), family = poisson, data=germ.sto)

summary(g.mod)
anova(g.mod.null, g.mod, test="Chisq")

#Comparison of plant diameter across sites and stoarge treatmnets in 2016

#Hypothesis: Plant Diameter will be higher at site with extant
#population of Chorizanthe (E012) in comparions to new introduction
#sites (EO14, NPS1, NPS2), whereas plant Diameter will be similar across storage treatments

ggplot(germ.sto.16, aes(Site, Diameter)) + geom_jitter(width=0.2, height=0.1)

ggplot(germ.sto.16, aes(trt, Diameter)) + geom_jitter(width=0.2, height=0.1)  +
  facet_grid(. ~ Site)

g.mod <- glm(Diameter ~ Site * trt, data=germ.sto.16)
g.mod.null <- glm(Diameter ~ Site, data=germ.sto.16)
summary(g.mod)
anova(g.mod, g.mod.null, test="Chisq")

## DWS: so where is the hypothesis test? These model does not represent the
## hypothesis listed (compare E012 with others)

##Comparison of germination across sites and storage treatments in 2017

#Hypotheses: Germination will be higher at site with extant
#population of Chorizanthe (EO12) in comparions to new introduction
#sites (EO14, NPS1, NPS2).


## DWS: so no support for this hypothesis at all.

#Germination will be higher among seeds stored in
#ambient conditions in native soil (SS12 and SS14) in comparison to seeds stored 
#at a constant temperature and relative humidity (RTSI and RTNSI). Seeds stored at high relative
#humidity or soil moisture will have lower germination.

ggplot(germ.sto.17, aes(Site, Germ)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto.17, aes(trt, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)

g.mod <- glm(Germ ~ Site + trt, family = poisson, data=germ.sto.17)
summary(g.mod)
anova(g.mod)

## DWS: why that model (no interaction), why no test?  What is question?

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

g.mod <- glm(, data=germ.sto.16)
summary(g.mod)
anova(g.mod, test="Chisq")

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
anova(g.mod, test="Chisq")


#2017 germination

#Hypothesis: Germination will be higher at site with extant
#population of Chorizanthe (E012) in comparions to control site and 
#for seeds suplied with supplemental moisture and shade

ggplot(germ.env.17, aes(Site, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

ggplot(germ.env.17, aes(water, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

ggplot(germ.env.17, aes(shade, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

g.mod <- glm(Germ ~ Site + water + shade, family = poisson, data=germ.env.17)
summary(g.mod)
anova(g.mod, test="Chisq")

