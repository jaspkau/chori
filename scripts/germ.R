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
library(splitstackshape)
library(afex)

#install_github("lme4/lme4",dependencies=TRUE)

# import after-ripening germination data -----------------------------------------

#Predictors: microhabitat (easterly vs. westerly) and 3 after-ripening treatment
#Response: seed germination and plant fitness

#Hypothesis: Germination will be hihger for combination of easterly aspect and
# driest after-ripening 

germ.sto <- read_excel("data/growth_germ.xlsx", sheet = 4)
germ.sto$Year <- factor(germ.sto$Year)
germ.sto$Width <- round(germ.sto$Width, digits = 0)
germ.sto$east <- germ.sto$Site =="EO12g"|germ.sto$Site=="NPS2"
germ.sto$block = as.factor(germ.sto$block)

# Calculation of means across treatments ------------------------------------------------------------------------

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

####### prepare data for plotting---------------------------------------------------

#show seed germination % variability and averages for micorohabitat and 
#after-ripening interctaion

gavg = cSplit(gavg, "int", ".")
gavg = plyr::rename(gavg, c("int_1"= "trt", "int_2"= "east"))

###calculate germination % for each seed packet 

germ.sto$sp = interaction(germ.sto$Site, germ.sto$Year, germ.sto$trt, germ.sto$block)

gavg2 <- germ.sto %>% 
  group_by(sp) %>% 
  summarise(Germ = (sum(Germ)/20)*100)
gavg2

gavg2 = cSplit(gavg2, "sp", ".")
gavg2 = plyr::rename(gavg2, c("sp_1"= "Site", "sp_2"= "Year", "sp_3"= "trt", "sp_4"= "block"))

####plot data

g1 = ggplot(gavg2, aes(trt, Germ)) + geom_point() + 
  geom_bar(data = gavg, stat = "identity", alpha = .4) + facet_grid(.~ east) +
  scale_y_continuous(limits = c(0, 23)) + 
  geom_jitter(width = 0.2, height = 0.8) +
  theme_classic() +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=30, vjust = 0.6),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ylab("Germination (%)") + 
  scale_x_discrete("After-ripening",
                   labels=c("22°C, 45% RH","22°C, 17% RH","Soil",
                            "22°C, 45% RH","22°C, 17% RH","Soil"))

g1

####### Run mixed effect model -----------------------------------------------------------------------

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

###############Comparison of plant Width across 
######after-ripening and micorhabitat interaction

germ.sto.wid = subset(germ.sto, Germ > 0)
germ.sto.wid$int = interaction(germ.sto.wid$trt, germ.sto.wid$east)

# Calculation of means across treatments ------------------------------------------------------------------------

##average Width across after-ripening treatements

gavg.wid <- germ.sto.wid %>% 
  group_by(trt) %>% 
  summarise(Width = mean(Width))
gavg.wid

##average Width across microhabitats

gavg.wid <- germ.sto.wid %>% 
  group_by(east) %>% 
  summarise(Width = mean(Width))
gavg.wid

##average Width across microhabitat and after-ripening interactions

gavg.wid <- germ.sto.wid %>% 
  group_by(int) %>% 
  summarise(Width = mean(Width))
gavg.wid

###prepare data for plotting

#show plant Width variability and averages for micorohabitat and 
#after-ripening interaction

gavg.wid = cSplit(gavg.wid, "int", ".")
gavg.wid = plyr::rename(gavg.wid, c("int_1"= "trt", "int_2"= "east"))

####### data plotting---------------------------------------------------

g2 = ggplot(germ.sto.wid, aes(trt, Width)) + geom_point() + 
  geom_bar(data = gavg.wid, stat = "identity", alpha = .4) + facet_grid(.~ east) +
  scale_y_continuous(limits = c(0, 25)) + 
  geom_jitter(width = 0.2, height = 0.7) +
  theme_classic() +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=30, vjust = 0.6),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ylab("Plant Width (cm)") + 
  scale_x_discrete("After-ripening",
                   labels=c("22°C, 45% RH","22°C, 17% RH","Soil",
                            "22°C, 45% RH","22°C, 17% RH","Soil"))
g2

####### Run mixed effect model -----------------------------------------------------------------------

gm1 <- afex::mixed(Width ~ trt*east + (1 | Site/block), method = "KR", data = germ.sto.wid)

anova.tab <- anova(gm1)
anova.tab

coef.tab <- summary(gm1)$coefficients
coef.tab

###############Microenvironment data--------------------------------------------
# import micorenvironment germination data----------------------------------

#Predictors: microhabitat (easterly vs. westerly), 2 light, and 2 soil moisture treatment
#Response: seed germination and plant fitness

#Hypothesis:  seeds exposed to a combination of supplemental soil moisture, 
#supplemental shade and eastern aspect microhabitat will exhibit highest seed germination

germ.env <- read_excel("data/growth_germ.xlsx", sheet = 3)
germ.env$Year = factor(germ.env$Year)
germ.env$Site = factor(germ.env$Site)
germ.env$shade = factor(germ.env$shade)
germ.env$water = factor(germ.env$water)
germ.env.17 <- subset(germ.env, Year == "2017")
germ.env$block = as.factor(germ.env$block)
germ.env$replicate = as.factor(germ.env$replicate)

# Calculation of means across treatments ------------------------------------------------------------------------

##germination % across light treatments

gavg <- germ.env %>% 
  group_by(shade) %>% 
  summarise(Germ = (sum(Germ)/960)*100)
gavg

##germination % across soil moisture treatements

gavg <- germ.env %>% 
  group_by(water) %>% 
  summarise(Germ = (sum(Germ)/960)*100)
gavg

##germination % across microhabitats

gavg <- germ.env %>% 
  group_by(Site) %>% 
  summarise(Germ = (sum(Germ)/960)*100)
gavg

##germination % across microhabitat, light and soil moisture interactions

germ.env$int = interaction(germ.env$Site, germ.env$shade, germ.env$water)

gavg <- germ.env %>% 
  group_by(int) %>% 
  summarise(Germ = (sum(Germ)/240)*100)
gavg

####### prepare data for plotting----------------------------------------------------

#show seed germination % variability and averages for micorohabitat and 
#after-ripening interctaion

gavg = cSplit(gavg, "int", ".")
gavg = plyr::rename(gavg, c("int_1"= "Site", "int_2"= "shade", "int_3" = "water"))
gavg$int = interaction(gavg$shade, gavg$water)

###calculate germination % for each seed packet 

germ.env$sp = interaction(germ.env$Site, germ.env$Year, germ.env$block, germ.env$replicate, germ.env$shade, germ.env$water)

gavg2 <- germ.env %>% 
  group_by(sp) %>% 
  summarise(Germ = (sum(Germ)/20)*100)
gavg2

gavg2 = cSplit(gavg2, "sp", ".")
gavg2 = plyr::rename(gavg2, c("sp_1"= "Site", "sp_2"= "Year", "sp_3"= "block", "sp_4"= "replicate",
                              "sp_5"= "shade", "sp_6"= "water"))
gavg2$int = interaction(gavg2$shade, gavg2$water)

g1 = ggplot(gavg2, aes(int, Germ)) + geom_point() + 
  geom_bar(data = gavg, stat = "identity", alpha = .4) + 
  facet_grid(.~ Site, labeller = labeller(fac_lab)) +
  scale_y_continuous(limits = c(0, 25)) + 
  geom_jitter(width = 0.2, height = 0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ylab("Germination (%)") + 
  scale_x_discrete("Light and soil moisture interaction",
                   labels=c("AL:AM","ML:AM","AL:MM","ML:MM","AL:AM","ML:AM","AL:MM","ML:MM"))

g1

####### Run mixed effect model -----------------------------------------------------------------------

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

########Comparison of plant Width------------------------------------------------

#Hypothesis: Width will be higher for plants exposed to a combination of supplemental soil moisture, 
#supplemental shade and eastern aspect microhabitat will exhibit highest seed germination

germ.env.wid = subset(germ.env.17, Germ > 0) ###remove data from ungerminated seeds
germ.env.wid$int = interaction(germ.env.wid$shade, germ.env.wid$water)

# Calculation of means across treatments ------------------------------------------------------------------------

##mean plant width across light treatements

gavg.wid2 <- germ.env.wid %>% 
  group_by(shade) %>% 
  summarise(Width = mean(Width))
gavg.wid2

##mean plant width across soil moisture treatements

gavg.wid2 <- germ.env.wid %>% 
  group_by(water) %>% 
  summarise(Width = mean(Width))
gavg.wid2

##mean plant width across light and soil moisture interactions

gavg.wid2 <- germ.env.wid %>% 
  group_by(int) %>% 
  summarise(Width = mean(Width))
gavg.wid2


####### data plotting---------------------------------------------------

g2 = ggplot(germ.env.wid, aes(int, Width)) + geom_point() + 
   geom_bar(data = gavg.wid2, stat = "identity", alpha = .3) +
  geom_jitter(width = 0.1, height = 0.4) + 
  theme_classic() +
  theme(axis.text.x = element_text(face="plain", color="black", 
                                   size=9, angle=0),
        axis.text.y = element_text(face="plain", color="black", 
                                   size=9, angle=0)) +
  ylab(" Plant Width (cm)") +  
  scale_x_discrete("Light and soil moisture interaction",
                   labels=c("AL:AM","ML:AM","AL:MM","ML:MM"))

g2

####### Run mixed effect model -----------------------------------------------------------------------

gm1 <- afex::mixed(Width ~ shade*water + (1 | block/replicate), method = "KR", data = germ.env.wid)

anova.tab <- anova(gm1)
anova.tab

coef.tab <- summary(gm1)$coefficients
coef.tab
