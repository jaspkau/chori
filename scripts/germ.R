#setwd("C:\\Users\\jaspkaur\\Google Drive\\data_analysis\\chorizanthe\\chori/")

library(readxl)
library(ggplot2)
library(pscl)
library(lme4)

# import after-ripening germination data -----------------------------------------

germ.sto <- read_excel("data/growth_germ.xlsx", sheet = 4)
germ.sto$Year <- factor(germ.sto$Year)
germ.sto$Diameter <- round(germ.sto$Diameter, digits = 0)
germ.sto$Native <- germ.sto$Site=="EO12"|germ.sto$Site=="EO14"

#Visual comparisons of and regression ------------------------------------------------------------------------

#Comparison of germination across sites and storage treatmnets

#Hypothesis: Native site will have hihger germination
#in comparison to foreign sites. Germination will be higher among seeds stored in
#ambient conditions in native soil (SS) in comparison to seeds stored 
#at a constant temperature and relative humidity (RTSI and RTNSI).Among the seeds
#stored at room temp, RTSI will have higher germination in comparison to RTNSI.

ggplot(germ.sto, aes(Site, Germ, color=Year)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto, aes(trt, Germ, color=Year)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)

g.mod <- glmer(Germ ~ trt + (1| Site) + (1 | Year), family = binomial, data=germ.sto)
g.mod2 <- glmer(Germ ~ Native + (1|Site) + (1 | Year), family = binomial, data=germ.sto)
g.mod3 <- glmer(Germ ~ dr.st + (1 | Site) + (1 | Year), family = binomial, data=germ.sto)
g.mod.null <- glmer(Germ ~ 1 + (1| Site) + (1 | Year), family = binomial, data=germ.sto)

anova(g.mod.null, g.mod, g.mod2, g.mod3, test="Chisq")

summary(g.mod3)

#Comparison of plant diameter across sites and stoarge treatmnets in 2016

#Hypothesis: Plant Diameter will be higher at site with extant
#population of Chorizanthe (E012) in comparions to new introduction
#sites (EO14, NPS1, NPS2), whereas plant Diameter will be similar across storage treatments

ggplot(germ.sto, aes(Site, Diameter, color=Year)) + geom_jitter(width=0.2, height=0.1)
ggplot(germ.sto, aes(trt, Diameter, color=Year)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Site)

g.mod <- lmerTest::lmer(Diameter ~ trt + (1| Site) + (1 | Year), data=germ.sto)
g.mod2 <- lmerTest::lmer(Diameter ~ Native + (1| Site) + (1 | Year), data=germ.sto)
g.mod3 <- lmerTest::lmer(Diameter ~ pg.at + (1 | Site) + (1 | Year), data=germ.sto)
g.mod.null <- lmerTest::lmer(Diameter ~ 1 + (1| Site) + (1 | Year), data=germ.sto)

anova(g.mod.null, g.mod, g.mod2, g.mod3, test="Chisq")

summary(g.mod3)

# import micorenvironment data --------------------------------------------

germ.env <- read_excel("data/growth_germ.xlsx", sheet = 3)
germ.env.17 <- subset(germ.env, Year == "2017")

# Visual comparisons and regression ------------------------------------------------------------------------

#Comparison of germination

#Hypothesis: Germination will be hihger at site with extant
#population of Chorizanthe (E012) in comparions to control site and 
#for seeds suplied with supplemental moisture and shade.

ggplot(germ.env, aes(water, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)
ggplot(germ.env, aes(shade, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)
ggplot(germ.env, aes(Site, Germ)) + geom_jitter(width=0.2, height=0.1) + facet_grid(. ~ Year)

g.mod <- glmer(Germ ~ shade*water + (1|Site) + (1 | Year), family = binomial, data=germ.env)
g.mod.null <- glmer(Germ ~ 1 + (1|Site) + (1 | Year), family = binomial, data=germ.env)

anova(g.mod.null, g.mod, test="Chisq")
summary(g.mod)


###Comparison of plant diameter

ggplot(germ.env.17, aes(water, Diameter)) + geom_jitter(width=0.2, height=0.1) 
ggplot(germ.env.17, aes(shade, Diameter)) + geom_jitter(width=0.2, height=0.1) 
ggplot(germ.env.17, aes(Site, Diameter)) + geom_jitter(width=0.2, height=0.1) 

g.mod <- lmerTest::lmer(Diameter ~ shade*water + (1| Site), data=germ.env.17)
g.mod.null <- lmerTest::lmer(Diameter ~ 1 + (1| Site), data=germ.env.17)

anova(g.mod.null, g.mod, test="Chisq")
summary(g.mod)

# Pricipal component with soil variables ---------------------------------

germ_avg <- read_excel("data/growth_germ.xlsx", sheet = 5)
row.names(germ_avg) = germ_avg$Code
X = germ_avg[,-1]
X = X[,3:22]
row.names(X) = row.names(germ_avg)
X = scale(X, center = TRUE, scale = TRUE)
X = na.omit(X)  

pca.results = prcomp(X, scale=F)               
pca.results

s = summary(pca.results)

site.scores = predict(pca.results)

site.scores

screeplot(pca.results, type="lines", main="PCA Scree Plot")  

library(ggbiplot)
site = germ_avg$Site
year = as.factor(germ_avg$Year)
g <- ggbiplot(pca.results, groups = site, size = 3)+ geom_point(aes(color = site, shape = year ), size = 3.5) +
  theme_bw(base_size = 15) +
  labs(x = paste("PC1 Scores (", as.character(round(100*s$importance[2,1])),"%)",sep=''),
       y = paste("PC2 Scores (", as.character(round(100*s$importance[2,2])),"%)",sep=''))
g

####different kind of plot
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
#install.packages("factoextra")
library(factoextra)

fviz_pca_ind(pca.results,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca.results, repel = TRUE,
                col.var = "red", # Variables color
                col.ind = "black"  # Individuals color
)
