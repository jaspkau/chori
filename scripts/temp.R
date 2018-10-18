#setwd("C:\\Users\\jas\\Google Drive\\data_analysis\\chorizanthe\\chori/")
#setwd("C:/Users/Administrator/Documents/jaspreet/chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)
library(dplyr)
library(pbkrtest)
#library(parallel)
#library(devtools) 
#install_github("lme4/lme4",dependencies=TRUE)

########################Import shade and soil moisture experiment data...........

#Sites = 2
#Shade treatments = 2
#Soil moisture treatments = 2
#Years = 2, but we have decided to not include it in any of the models below
#Response: seed germination and plant fitness

germ.env <- read_excel("data/growth_germ.xlsx", sheet = 3)
germ.env$Year = factor(germ.env$Year)
germ.env$Site = factor(germ.env$Site)
germ.env$shade = factor(germ.env$shade)
germ.env$water = factor(germ.env$water)
germ.env.17 <- subset(germ.env, Year == "2017")
germ.env$block = as.factor(germ.env$block) ###factor for three blocks in the experimnetal area
germ.env$replicate = as.factor(germ.env$replicate)#####factor for two replicate seed packets (with 20 seeds in each) of each shade and soil moisture treatment combination in each block

####All models given below have fixed effect of Site*Shade*soil moisture intercation, whereas random effects vary
##Additional comments: We talked about inclluding two way interactions of fixed effects in the model if it is overspecified 
#with three way interaction. Since I was able to run model with three way interctaion, I did not use two way interctaions
#of site, shade and soil moisture in any of the models

#Without accounting for the random effect of blocks or replicates

gm <- glm(Germ ~ Site*shade*water, family = binomial,
          data = germ.env)

####random intercept for block

gm1 <- glmer(Germ ~ Site*shade*water + (1 | block), family = binomial,
             data = germ.env)

####random intercept for replicates nested within block

gm2 <- glmer(Germ ~ Site*shade*water + (1 | block/replicate), family = binomial,
             data = germ.env)

##Comparison of AIC values of three models
AIC(gm, gm1, gm2)

#Question: Which model I should select given that AIC values are not showing big difference among three models? Although
#AIC value is slightly hihger for model gm2, but it is more representative of the experimnetal design, so I think its
#better to use it.

# import after-ripening germination data -----------------------------------------

#Sites = 4
#after-ripening treatments = 3
#microhabitats  = 2
#Years = 2, but we have decided to not include it in any of the models below
#Response: seed germination and plant fitness

germ.sto <- read_excel("data/growth_germ.xlsx", sheet = 4)
germ.sto$Year <- factor(germ.sto$Year)
germ.sto$Diameter <- round(germ.sto$Diameter, digits = 0)
germ.sto$east <- germ.sto$Site =="EO12g"|germ.sto$Site=="NPS2"
germ.sto$block = as.factor(germ.sto$block) ###factor for the nine blocks

#####models to test the effect of predictors on seed germination
####Fixed effect = interaction of three after-ripening treatmnets (trt) and two microhabitats with east/west aspects (east)

##Random effects = block nested within four experimnetal sites

gm <- glmer(Germ ~ trt*east + (1 | Site/block), family = binomial,
            data = germ.sto)

####Random effects = four experimnetal sites
gm1 <- glmer(Germ ~ trt*east + (1 | Site), family = binomial,
             data = germ.sto)

AIC(gm, gm1)

##Comments: AIC value is lower with first model (gm) and it taked into account the random effects of both, experimnetal site
#and blocks. According to me, its better to use model gm. 

# Determine the spatio variation in Diameter in descriptive study------------------------------------------------------------------------

##Sites = 3
##Years = 4, not used in the model
#Response = plant fitness

growth <- read_excel("data/growth_germ.xlsx", sheet = 1)
growth$Year = as.factor(growth$Year) #convert year values into factor 

####Regression of plant diameter with sites 

gm <- lm(Diameter ~ Site, data = growth)

anova.tab <- anova(gm)
anova.tab





