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
germ.env$Site = factor(germ.env$Site)  # Why
germ.env$shade = factor(germ.env$shade)
germ.env$water = factor(germ.env$water)
germ.env.17 <- subset(germ.env, Year == "2017")  ## DWS: why?
germ.env$block = as.factor(germ.env$block) ###factor for three blocks in the experimental area
germ.env$replicate = as.factor(germ.env$replicate)#####factor for two replicate seed packets (with 20 seeds in each) of each shade and soil moisture treatment combination in each block

## OK, so "replicate" only has 2 values and therefore is not explicitly nested.
## You must be careful below.

## DWS: that is a lot of repeated code. Perhaps mutate_all() would help? And
## why is this necessary for some of these?

## All models given below have fixed effect of Site*Shade*soil moisture
## intercation, whereas random effects vary Additional comments: We talked
## about including two way interactions of fixed effects in the model if it is
## overspecified with three way interaction. Since I was able to run model with
## three way interactaion, I did not use two way interactions of site, shade and
## soil moisture in any of the models.

## DWS: this does not make sense, you can't include a 3-way and leave out the
## nested 2-ways. This will wreck interpretations and bias coefficients.

## DWS: Work on cleaner formatting of your comments. Check a style guide such
## as mine: http://r-research-tool.schwilk.org/handouts/style-guide.html

# Without accounting for the random effect of blocks or replicates

gm <- glm(Germ ~ Site*shade*water, family = binomial, data = germ.env)

## DWS this kmodel does include 2-way interactions. You use the "*" shorthand
## so that expands to 2 and 3-way interactions. But this model deos not make
## sense -- treatments were not applied individiually to seeds.


####random intercept for block

gm1 <- glmer(Germ ~ Site*shade*water + (1 | block), family = binomial,
             data = germ.env)
## MODEL FAILS TO CONVERGE!  You can't go on

## DWS: Again NO, treatments are not applied to individual seeds!

####random intercept for replicates nested within block

gm2 <- glmer(Germ ~ Site*shade*water + (1 | block/replicate), family = binomial,
             data = germ.env)

## gm3 <- glmer(Germ ~ Site*shade*water + Year + (1 | block/replicate), family = binomial,
##              data = germ.env)

##Comparison of AIC values of three models

## You can't use AIC to tell you the nested structure, Only the third one gets
## the replication close but still wrong. Check the numbers! You have 8 seed
## packets per block, not 2. You must either identify the subblocks as well
## (and recode to replicates have identifies 1,2,3,4) or explicitly nest the
## replicates completely. Look:

## > gm2
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Germ ~ Site * shade * water + (1 | block/replicate)
##    Data: germ.env
##       AIC       BIC    logLik  deviance  df.resid 
##  565.3874  620.9882 -272.6937  545.3874      1910 
## Random effects:
##  Groups          Name        Std.Dev. 
##  replicate:block (Intercept) 2.481e-05
##  block           (Intercept) 3.125e-01
## Number of obs: 1920, groups:  replicate:block, 6; block, 3
## Fixed Effects:
##             (Intercept)                SiteEO14g                   shades  
##                 -2.9031                  -1.2211                  -0.1766  
##                  waterw         SiteEO14g:shades         SiteEO14g:waterw  
##                  0.2876                 -15.2965                  -1.6877  
##           shades:waterw  SiteEO14g:shades:waterw  
##                  0.2977                  16.8034  
## > 

## You have 1920 observations, that is correct, but other numbers are wrong
## which means you have the model wrong.

## DWS: I /think/ you want:

germ.env <- germ.env %>% mutate(subblock = paste(block, water, sep=""),
                                packet = paste(shade, replicate, sep="_"))
# OR, you could explicitly nest `packet` eg paste(block, water, shade, replicate...)
gm2 <- glmer(Germ ~ Site*shade*water + (1 | subblock/packet), family = binomial,
             data = germ.env)

## DWS: Really it should be block/subblock/packet to capture how you applied
## water and allow for an elevation effect along block, but I think you must
## ignore that so that the model is not overspecified ("degenerate" matrix
## warning)

gm2


## DWS: Where are your figures for this?  Can you show logistic curves/surfaces?


#Question: Which model I should select given that AIC values are not showing big difference among three models? Although
#AIC value is slightly higher for model gm2, but it is more representative of the experimnetal design, so I think its
#better to use it.

## DWS: It is the only one not psuedoreplciated. Of course its AIC is higher.
## Year is still excluded which is a bit iffy but perhaps justified.
                                                                

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





