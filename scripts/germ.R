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
germ.sto$block = as.factor(germ.sto$block)
germ.sto$aspect = ifelse(germ.sto$Site %in% c("EO12g", "NPS2"), "Easterly", "Westerly")


# Calculation of means across treatments ------------------------------------------------------------------------

##germination % across after-ripening treatements
gavg <- germ.sto %>% 
  group_by(trt) %>% 
  summarise(Germ = (sum(Germ)/1440)*100)
gavg

##germination % across micorhabitats
gavg <- germ.sto %>% 
  group_by(aspect) %>% 
  summarise(Germ = (sum(Germ)/2160)*100)
gavg

##germination % across after-ripening and micorhabitat interaction

gavg <- germ.sto %>% 
  group_by(trt, aspect) %>% 
  summarise(avg = (sum(Germ)/720)*100)
gavg

####### prepare data for plotting---------------------------------------------------

###calculate germination % for each seed packet 

gavg2 <- germ.sto %>% 
  group_by(Site, Year, trt, block) %>% 
  summarise(Germ = (sum(Germ)/20*100))
gavg2

gavg2$aspect = ifelse(gavg2$Site %in% c("EO12g", "NPS2"), "Easterly", "Westerly")

###calculate mean and other statitics for treatmnet and microhabitat interctaion

gavg3 <- gavg2 %>% 
  group_by(trt, aspect) %>% 
  summarise(avg = mean(Germ),
            sd = sd(Germ, na.rm = TRUE),
            n = n(),
            se = sd/ sqrt(n),
            lci = avg-1.96*se,
            uci = avg+1.96*se) %>%
  rename(Germ = avg)
gavg3

####plot data (Fig. 3)

g.ar.germ = ggplot(gavg2, aes(trt, Germ)) +
  #geom_bar(data = gavg, stat = "identity", alpha = .4) + 
  #geom_crossbar(data=gavg3, aes(ymin=lci, ymax=uci), #group = 2,  
             # width = 0.2, size = 0.6, alpha = 0.8, color = "blue") +
  facet_grid(.~ aspect) +
    scale_y_continuous(limits = c(0, 23)) + 
  geom_jitter(width = 0.2, height = 0.4, size = 0.4) +
  theme_classic() +
  ggtitle("Figure 3") + 
  theme(axis.text.x = element_text(face="plain",
                                   color="black", 
                                   size=9, angle=30, vjust = 0.6),
        axis.text.y = element_text(face="plain",
                                   color="black", 
                                   size=9, angle=0)) +
  ylab("Germination (%)") + 
  scale_x_discrete("After-ripening",
                   labels=c("22°C, 45% RH","22°C, 17% RH","Soil",
                            "22°C, 45% RH","22°C, 17% RH","Soil")) +
  stat_summary(fun.data = mean_cl_boot, geom = "crossbar", color = "black", width = 0.2, size = 0.6) #fun.args = list(mult = 1)

g.ar.germ

ggsave(g.ar.germ, file="results/AR_germ.pdf", 
       width = 6, height = 5, units = "in")

####### Run mixed effect model -----------------------------------------------------------------------

cl = makeCluster(35) ###for running parallel jobs

gm1 <- afex::mixed(Germ ~ trt*aspect + (1 | Site:block), family = binomial, nAGQ = 1L, 
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
  group_by(trt, aspect) %>% 
  summarise(Width = mean(Width))
gavg.wid

####### data plotting (Fig. 4)---------------------------------------------------

g.ar.wid = ggplot(germ.sto.wid, aes(trt, Width)) +  
  #geom_bar(data = gavg.wid, stat = "identity", alpha = .4) + 
  facet_grid(.~ aspect) +
  scale_y_continuous(limits = c(0, 25)) + 
  geom_jitter(width = 0.1, height = 0.1, size = 0.4) +
  theme_classic() +
  ggtitle("Figure 4") + 
  theme(axis.text.x = element_text(face="plain", color="black",
                                   size=9, angle=30, vjust = 0.6),
        axis.text.y = element_text(face="plain", color="black",
                                   size=9, angle=0)) +
  ylab("Plant Width (cm)") + 
  scale_x_discrete("After-ripening",
                   labels=c("22°C, 45% RH","22°C, 17% RH","Soil",
                            "22°C, 45% RH","22°C, 17% RH","Soil")) +
  stat_summary(fun.data = mean_cl_boot, geom = "crossbar", color = "black", width = 0.2, size = 0.6) #fun.args = list(mult = 1)

g.ar.wid

ggsave(g.ar.wid, file="results/AR_wid.pdf", 
       width = 5, height = 4, units = "in")

###############Microenvironment experiment data--------------------------------------------
#import data-

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

gavg <- germ.env %>% 
  group_by(Site, shade, water) %>% 
  summarise(Germ = mean(Germ)*100)
gavg


####### prepare data for plotting----------------------------------------------------

###calculate germination % for each seed packet 

gavg2 <- germ.env %>% 
  group_by(Site, Year, block, replicate, shade, water) %>% 
  summarise(Germ = (sum(Germ)/20)*100)
gavg2

gavg2$int = interaction(gavg2$shade, gavg2$water)
gavg2$aspect = ifelse(gavg2$Site %in% c("EO12g"), "Easterly", "Westerly")

####plot data (Figure 5)

g.env.germ = ggplot(gavg2, aes(int, Germ)) + 
  #geom_bar(data = gavg, stat = "identity", alpha = .4) + 
  facet_grid(.~ aspect) +
  scale_y_continuous(limits = c(0, 23)) + 
  geom_jitter(width = 0.2, height = 0.4, size = 0.4) +
  theme_classic() +
  ggtitle("Figure 5") + 
  theme(axis.text.x = element_text(face="plain", color="black",
                                   size=9, angle=0),
        axis.text.y = element_text(face="plain", color="black",
                                   size=9, angle=0)) +
  ylab("Germination (%)") + 
  scale_x_discrete("Light and soil moisture interaction",
                   labels=c("AL:AM","ML:AM","AL:MM","ML:MM","AL:AM","ML:AM","AL:MM","ML:MM")) +
  stat_summary(fun.data = mean_cl_boot, geom = "crossbar", color = "black", width = 0.2, size = 0.6) #fun.args = list(mult = 1)

g.env.germ

ggsave(g.env.germ, file="results/env_germ.pdf", 
       width = 6, height = 4, units = "in")

####### Run mixed effect model -----------------------------------------------------------------------

gm1 <- afex::mixed(Germ ~ shade*water*Site + (1 | block:replicate), family = binomial, nAGQ = 1L, 
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


####### data plotting (Fig. 6) ---------------------------------------------------

g.env.wid = ggplot(germ.env.wid, aes(int, Width)) +  
   #geom_bar(data = gavg.wid2, stat = "identity", alpha = .3) +
  geom_jitter(width = 0.1, height = 0.1, size = 0.4) + 
  theme_classic() +
  ggtitle("Figure 6") + 
  theme(axis.text.x = element_text(face="plain", color="black",
                                   size=9, angle=0),
        axis.text.y = element_text(face="plain", color="black",
                                   size=9, angle=0)) +
  ylab(" Plant Width (cm)") +  
  scale_x_discrete("Light and soil moisture interaction",
                   labels=c("AL:AM","ML:AM","AL:MM","ML:MM")) + 
  stat_summary(fun.data = mean_cl_boot, geom = "crossbar", color = "black", width = 0.2, size = 0.6) #fun.args = list(mult = 1)

g.env.wid

ggsave(g.env.wid, file="results/env_wid.pdf", 
       width = 5, height = 4, units = "in")
