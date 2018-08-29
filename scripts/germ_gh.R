#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)

# import after-ripening germination data -----------------------------------------

germ.sto <- read_excel("data/chorizanthe_germination_gh.xlsx", sheet = 2)
germ.sto$site <- as.factor(germ.sto$Site)
germ.sto$trt <- as.factor(germ.sto$trt)

germ.sto = subset(germ.sto, trt == "SS"|
                    trt == "RTSI"|
                    trt == "RTNSI")

#Visual comparisons of and regression ------------------------------------------------------------------------

ggplot(germ.sto, aes(site, germ)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto, aes(trt, germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ site)

g.mod <- glmer(germ ~ site*trt (1| replicate), family = poisson, data=germ.sto)
#g.mod2 <- glmer(germ ~ trt + (1| replicate), family = poisson, data=germ.sto)
g.mod.null <- glmer(germ ~ 1 + (1| replicate), family = poisson, data=germ.sto)

anova(g.mod.null, g.mod, test="Chisq")

summary(g.mod2)

data.avg = group_by(germ.sto, trt)
pg_env_avg = data.frame(summarise_each(data.avg, funs(mean(., na.rm = TRUE))))   # calculate the annual mean of airt


