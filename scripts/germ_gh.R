#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)

# import after-ripening germination data -----------------------------------------

germ.sto <- read_excel("data/chorizanthe_germination_gh.xlsx", sheet = 1)
germ.sto$site <- as.factor(germ.sto$site)
germ.sto$Treatment <- as.factor(germ.sto$Treatment)

#Visual comparisons of and regression ------------------------------------------------------------------------

ggplot(germ.sto, aes(site, germ)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto, aes(Treatment, germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ site)

g.mod <- glmer(germ ~ site + (1| Treatment), family = poisson, data=germ.sto)
g.mod.null <- glmer(germ ~ 1 + (1| Treatment), family = poisson, data=germ.sto)

anova(g.mod.null, g.mod, test="Chisq")

summary(g.mod)

