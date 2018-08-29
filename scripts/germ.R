#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)
library(dplyr)
library(pbkrtest)

# import after-ripening germination data -----------------------------------------

germ.sto <- read_excel("data/growth_germ.xlsx", sheet = 4)
germ.sto$Year <- factor(germ.sto$Year)
germ.sto$Diameter <- round(germ.sto$Diameter, digits = 0)
germ.sto$Native <- germ.sto$Site=="EO12g"|germ.sto$Site=="EO14g"

#Visual comparisons and regression ------------------------------------------------------------------------

#Comparison of germination across native/foreign sites, storage treatmnets and soil temperature

#Hypothesis: Germination will be hihger for seeds germination will be highest among seeds 
#after-ripened in ambient conditions in native soil in comparison to seeds 
#after-ripened under modified controlled environments during the period of dormancy

#Hypothesis: seeds introduced at foreign sites will yield similar germination 
#ant fitness as the native donor sites with the primary regulation 
#by microenvironment of the experimental site

ggplot(germ.sto, aes(Site, Germ, color=Year)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto, aes(trt, Germ, color=Year)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)

g.mod <- glmer(Germ ~ trt + (1| Site) + (1 | Year), family = binomial, data=germ.sto)
g.mod2 <- glmer(Germ ~ Native + (1|Site) + (1 | Year), family = binomial, data=germ.sto)
g.mod3 <- glmer(Germ ~ dr.st + (1 | Site) + (1 | Year), family = binomial, data=germ.sto)
g.mod.null <- glmer(Germ ~ 1 + (1| Site) + (1 | Year), family = binomial, data=germ.sto)

#Question: Shall I use anova here or parametric bootstrap test? KRmodcomp is not 
#possible to use with glmer models.
#How many number of simulations?

anova(g.mod, g.mod2, g.mod3, g.mod.null, test="Chisq")

PBmodcomp(g.mod, g.mod.null, nsim=10)

summary(g.mod)
summary(g.mod2)
summary(g.mod3)

#Comparison of plant diameter across sites and stoarge treatmnets....

germ.sto.dia = subset(germ.sto, Germ > 0)

ggplot(germ.sto.dia, aes(Site, Diameter, color=Year)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto.dia, aes(trt, Diameter, color=Year)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)

g.mod <- lmerTest::lmer(Diameter ~ trt + (1| Site) + (1 | Year), data=germ.sto.dia)
g.mod2 <- lmerTest::lmer(Diameter ~ Native + (1| Site) + (1 | Year), data=germ.sto.dia)
g.mod3 <- lmerTest::lmer(Diameter ~ pg.at + (1 | Site) + (1 | Year), data=germ.sto.dia)
g.mod.null <- lmerTest::lmer(Diameter ~ 1 + (1| Site) + (1 | Year), data=germ.sto.dia)

anova(g.mod.null, g.mod, g.mod2, g.mod3, test="Chisq")

KRmodcomp(g.mod, g.mod.null)
##Question, I should report results as (F(2,83.03) = 0.61, P = 0.54)?
KRmodcomp(g.mod2, g.mod.null)
KRmodcomp(g.mod3, g.mod.null)

# import micorenvironment germination data --------------------------------------------

germ.env <- read_excel("data/growth_germ.xlsx", sheet = 3)
germ.env.17 <- subset(germ.env, Year == "2017")

# Visual comparisons and regression ------------------------------------------------------------------------

#Hypothesis: Germination will be hihger for seeds suplied with supplemental moisture and shade.

ggplot(germ.env, aes(water, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)
ggplot(germ.env, aes(shade, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

g.mod <- glmer(Germ ~ shade*water + (1|Site) + (1 | Year), family = binomial, data=germ.env)
g.mod.null <- glmer(Germ ~ 1 + (1|Site) + (1 | Year), family = binomial, data=germ.env)

anova(g.mod.null, g.mod, test="Chisq")
summary(g.mod)

###Comparison of plant diameter

#Hypothesis: Diameter will be hihger for plants suplied with supplemental moisture and shade.

germ.env.dia = subset(germ.env.17, Germ > 0)

ggplot(germ.env.dia, aes(water, Diameter)) + geom_jitter(width=0.2, height=0.1) 
ggplot(germ.env.dia, aes(shade, Diameter)) + geom_jitter(width=0.2, height=0.1) 

g.mod <- lmerTest::lmer(Diameter ~ shade*water + (1| Site), data=germ.env.dia)
g.mod.null <- lmerTest::lmer(Diameter ~ 1 + (1| Site), data=germ.env.dia)

anova(g.mod.null, g.mod, test="Chisq")
summary(g.mod)