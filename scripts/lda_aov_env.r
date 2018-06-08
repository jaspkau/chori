setwd("C:\\Users\\jaspkaur\\Dropbox\\7april\\july_2016\\")
data <- read.xls("envt_july.xlsx",
                 sheet = 1, verbose = TRUE, perl="C:/Perl64/bin/perL")

######
#LDA with random sampling from raw data from JAN to May
###
eo12atemp = sample(data[1015:1914,]$EO12.atemp, 400, replace = FALSE, prob = NULL)
eo12rh = sample(data[1015:1914,]$EO12.rh, 400, replace = FALSE, prob = NULL)
eo12stemp = sample(data[1015:1914,]$EO12.soiltemp, 400, replace = FALSE, prob = NULL)
eo12sm1 = sample(data[1015:1914,]$EO12.sm1, 400, replace = FALSE, prob = NULL)
eo12sm2 = sample(data[1015:1914,]$EO12.sm2, 400, replace = FALSE, prob = NULL)
eo12par = as.numeric(as.character(sample(data[1015:1914,]$EO12.par.inphase, 400, replace = FALSE, prob = NULL)))
eo12rf = sample(data[1015:1914,]$EO12.rf, 400, replace = FALSE, prob = NULL)

eo14atemp = sample(data[1015:1914,]$EO14.atemp, 400, replace = FALSE, prob = NULL)
eo14rh = sample(data[1015:1914,]$EO14.rh, 400, replace = FALSE, prob = NULL)
eo14stemp = sample(data[1015:1914,]$EO14.soiltemp, 400, replace = FALSE, prob = NULL)
eo14sm1 = sample(data[1015:1914,]$EO14.sm1, 400, replace = FALSE, prob = NULL)
eo14sm2 = as.numeric(as.character(sample(data[1015:1914,]$EO14.sm2, 400, replace = FALSE, prob = NULL)))
eo14par = sample(data[1015:1914,]$EO14.par.inph, 400, replace = FALSE, prob = NULL)
eo14rf = sample(data[1015:1914,]$EO14.rf, 400, replace = FALSE, prob = NULL)

eo16atemp = sample(data[1015:1914,]$EO16.atemp, 400, replace = FALSE, prob = NULL)
eo16rh = sample(data[1015:1914,]$EO16.rh, 400, replace = FALSE, prob = NULL)
eo16stemp = sample(data[1015:1914,]$EO16.soiltemp, 400, replace = FALSE, prob = NULL)
eo16sm1 = sample(data[1015:1914,]$EO16.sm1, 400, replace = FALSE, prob = NULL)
eo16sm2 = sample(data[1015:1914,]$EO16.sm2, 400, replace = FALSE, prob = NULL)
eo16par = sample(data[1015:1914,]$EO16.par.inph, 400, replace = FALSE, prob = NULL)
eo16rf = sample(data[1015:1914,]$EO16.rf, 400, replace = FALSE, prob = NULL)

####2016 data

eo12atemp.2 = sample(data[3200:4042,]$EO12.atemp, 400, replace = FALSE, prob = NULL)
eo12rh.2 = sample(data[3200:4042,]$EO12.rh, 400, replace = FALSE, prob = NULL)
eo12stemp.2 = sample(data[3200:4042,]$EO12.soiltemp, 400, replace = FALSE, prob = NULL)
eo12sm1.2 = sample(data[3200:4042,]$EO12.sm1, 400, replace = FALSE, prob = NULL)
eo12sm2.2 = sample(data[3200:4042,]$EO12.sm2, 400, replace = FALSE, prob = NULL)
eo12par.2 = as.numeric(as.character(sample(data[3200:4042,]$EO12.par.inphase, 400, replace = FALSE, prob = NULL)))
eo12rf.2 = sample(data[3200:4042,]$EO12.rf, 400, replace = FALSE, prob = NULL)

eo14atemp.2 = sample(data[3200:4042,]$EO14.atemp, 400, replace = FALSE, prob = NULL)
eo14rh.2 = sample(data[3200:4042,]$EO14.rh, 400, replace = FALSE, prob = NULL)
eo14stemp.2 = sample(data[3200:4042,]$EO14.soiltemp, 400, replace = FALSE, prob = NULL)
eo14sm1.2 = sample(data[3200:4042,]$EO14.sm1, 400, replace = FALSE, prob = NULL)
eo14sm2.2 = as.numeric(as.character(sample(data[3200:4042,]$EO14.sm2, 400, replace = FALSE, prob = NULL)))
eo14par.2 = sample(data[3200:4042,]$EO14.par.inph, 400, replace = FALSE, prob = NULL)
eo14rf.2 = sample(data[3200:4042,]$EO14.rf, 400, replace = FALSE, prob = NULL)

eo16atemp.2 = sample(data[3200:4042,]$EO16.atemp, 400, replace = FALSE, prob = NULL)
eo16rh.2 = sample(data[3200:4042,]$EO16.rh, 400, replace = FALSE, prob = NULL)
eo16stemp.2 = sample(data[3200:4042,]$EO16.soiltemp, 400, replace = FALSE, prob = NULL)
eo16sm1.2 = sample(data[3200:4042,]$EO16.sm1, 400, replace = FALSE, prob = NULL)
eo16sm2.2 = sample(data[3200:4042,]$EO16.sm2, 400, replace = FALSE, prob = NULL)
eo16par.2 = sample(data[3200:4042,]$EO16.par.inph, 400, replace = FALSE, prob = NULL)
eo16rf.2 = sample(data[3200:4042,]$EO16.rf, 400, replace = FALSE, prob = NULL)

EO12.15 = as.character(factor(sample(1:1, 400, replace = TRUE), labels = c("EO12.15")))
EO14.15 = as.character(factor(sample(1:1, 400, replace = TRUE), labels = c("EO14.15")))
EO16.15 = as.character(factor(sample(1:1, 400, replace = TRUE), labels = c("EO16.15")))
EO12.16 = as.character(factor(sample(1:1, 400, replace = TRUE), labels = c("EO12.16")))
EO14.16 = as.character(factor(sample(1:1, 400, replace = TRUE), labels = c("EO14.16")))
EO16.16 = as.character(factor(sample(1:1, 400, replace = TRUE), labels = c("EO16.16")))

####if combine doesn't work then detach("package:randomForest", unload=TRUE)
atemp = combine(eo12atemp, eo14atemp, eo16atemp, eo12atemp.2, eo14atemp.2, eo16atemp.2)
atemp = atemp[,-2]
rh = combine(eo12rh, eo14rh, eo16rh, eo12rh.2, eo14rh.2, eo16rh.2)
rh = rh[,-2]
stemp = combine(eo12stemp, eo14stemp, eo16stemp, eo12stemp.2, eo14stemp.2, eo16stemp.2)
stemp = stemp[,-2]
sm1 = combine(eo12sm1, eo14sm1, eo16sm1, eo12sm1.2, eo14sm1.2, eo16sm1.2)
sm1 = sm1[,-2]
sm2 = combine(eo12sm2, eo14sm2, eo16sm2, eo12sm2.2, eo14sm2.2, eo16sm2.2)
sm2 = sm2[,-2]
par = combine(eo12par, eo14par, eo16par, eo12par.2, eo14par.2, eo16par.2)
par = par[,-2]
rf = combine(eo12rf, eo14rf, eo16rf, eo12rf.2, eo14rf.2, eo16rf.2)
rf = rf[,-2]

site = combine(EO12.15, EO14.15, EO16.15, EO12.16, EO14.16, EO16.16)
site = site[,-2]
sample = cbind(atemp, rh, stemp, sm1, sm2, par, rf)
sample = data.frame(sample)
sample$site = site

qqnorm(sample$atemp)
qqnorm(sample$logatemp)
qqnorm(sample$sqatemp)###it works
sample$logatemp = log(sample$atemp)
sample$sqatemp = sqrt(sample$atemp)

qqnorm(sample$rh) ##use raw data
sample$rh2 = sample$rh + 1
sample$logrh2 = log(sample$rh2)
sample$sqrh2 = sqrt(sample$rh2)
qqnorm(sample$logrh2)
qqnorm(sample$sqrh2)
qqnorm(sample$rh2)

qqnorm(sample$stemp)
sample$sqstemp = sqrt(sample$stemp)
qqnorm(sample$sqstemp) ###it works

qqnorm(sample$sm1)
sample$sqsm1 = sqrt(sample$sm1)
qqnorm(sample$sqsm1) ###it works

qqnorm(sample$sm2)
sample$sqsm2 = sqrt(sample$sm2)
qqnorm(sample$sqsm2) ###it works

qqnorm(sample$par) ####not at all normal, use raw data
sample$par2 = sample$par - 2
sample$sqpar2 = sqrt(sample$par2)
qqnorm(sample$sqpar2)
sample$logpar2 = log(sample$par2)
qqnorm(sample$par2)

qqnorm(sample$rf) ####not at all normal, use raw data
sample$rf2 = sample$rf - 5
sample$sqrf2 = sqrt(sample$rf2)
qqnorm(sample$sqrf2)
sample$logrf2 = log(sample$rf2)
qqnorm(sample$logrf2)


#p1 <- ggplot(sample, aes(atemp)) + facet_grid(~site) +
#  geom_histogram(alpha=0.3, fill='white', colour='black', binwidth=.04)


r3 <- lda(formula = site ~ sm + stemp + atemp + rh, 
          data = sample, na.action="na.omit")
r3$prior
r3$means
r3$scaling
r3$svd
prop = r3$svd^2/sum(r3$svd^2)
prop

acf(sheet5$stemp, lag.max = 100)

p = ggord(r3, site)

####ANOVA of randomly smapled values if Soil moisture

aov = aov(sample$atemp ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$atemp, sample$site, DFerror, MSerror)
l

aov = aov(sample$sqatemp ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$sqatemp, sample$site, DFerror, MSerror)
l

aov = aov(sample$rh ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$rh, sample$site, DFerror, MSerror)
l

aov = aov(sample$stemp ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$stemp, sample$site, DFerror, MSerror)
l

aov = aov(sample$sqstemp ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$sqstemp, sample$site, DFerror, MSerror)
l

aov = aov(sample$sm1 ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$sm1, sample$site, DFerror, MSerror)
l

aov = aov(sample$sqsm1 ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$sqsm1, sample$site, DFerror, MSerror)
l

aov = aov(sample$sm2 ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$sm2, sample$site, DFerror, MSerror)
l

aov = aov(sample$sqsm2 ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$sqsm2, sample$site, DFerror, MSerror)
l

aov = aov(sample$rf ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$rf, sample$site, DFerror, MSerror)
l

aov = aov(sample$par ~ sample$site)
summary.aov(aov)
DFerror = aov$df.residual
MSerror = sum(aov$residuals^2)/aov$df.residual
l = LSD.test(sample$par, sample$site, DFerror, MSerror)
l

###LDA

r <- lda(formula = site ~ atemp + rh + stemp + sm1+ sm2 + rf, 
         data = X, na.action="na.omit")
r$prior
r$means
r$scaling
r$svd
prop = r$svd^2/sum(r$svd^2)
prop

r$na.action
site = X$site[-c(2116, 2213, 2309, 2363)]
p = ggord(r, site)
p

###PCA

library(lattice)

sample.data = sample

sample.data

attach(sample.data)                               
X = cbind(sample.data[, c(1:5, 7)])
X = scale(X, center = TRUE, scale = TRUE)
X = data.frame(X)
X$site = sample$site
X = na.omit(X)

trt = X$site

X = cbind(X[1:6])

#X = log(X)                                       


## Examine pairwise relationships among variables
X.cov = cov(X)                                 
X.cov

X.cor = cor(X)                                    
X.cor

x.totVar = sum(diag(X.cov))                     
x.totVar

splom(X)                                        

## Basic eigenanalysis

eigen.results.cov = eigen(X.cov)
eigen.results.cov

eigen.results.cor = eigen(X.cor)
eigen.results.cor


## Principal components analysis via prcomp()

pca.results = prcomp(X, scale=F)               
pca.results

summary(pca.results)                              

#windows()
screeplot(pca.results, type="lines", main="PCA Scree Plot")  

pca.eigenvalues = pca.results$sdev^2
pca.eigenvalues

pca.eigenvalues.sum = sum(pca.eigenvalues)
pca.eigenvalues.sum

pca.percentVar = 100*(pca.eigenvalues / sum(pca.eigenvalues))
pca.cumVar = cumsum(pca.percentVar)
pca.percentVar
pca.cumVar

pca.loadings = pca.results$rotation
pca.loadings
pca.loadings.sscp = t(pca.loadings) %*% pca.loadings
pca.loadings.sscp

pca.scores = predict(pca.results)
pca.scores

pca.vectorCorrs = cor(X,pca.scores)
pca.vectorCorrs

#windows()
splom(pca.scores)

#windows()
trt = sample.data$site
#year = as.factor(sample.data$year)
g <- ggbiplot(pca.results, xlab=xlab.str, ylab=ylab.str,
              groups = trt, circle = TRUE, size = 3)+ geom_point(aes(color = trt), size = 3.5) +
  theme_bw(base_size = 15) +
  labs(x = paste("PC1 Scores (", as.character(round(pca.percentVar[1],1)),"%)",sep=''),
       y = paste("PC2 Scores (", as.character(round(pca.percentVar[2],1)),"%)",sep=''))
g


#windows()
xyplot(pca.scores[,2]~pca.scores[,1], pch=19,col="black", xlab=xlab.str,ylab=ylab.str)

#windows()



cloud(pca.scores[,3]~pca.scores[,1]*pca.scores[,2], pch=19,col="black",
      xlab='PC1 Scores',ylab='PC2 scores',zlab='PC3 scores')


## Principal components analysis via princomp()

pca.results = princomp(X, cor=F, scores=T)         # PCA    
pca.results

summary(pca.results, loadings=T)

#windows()
biplot(pca.results, col=c("red","blue"), xlab=xlab.str, ylab=ylab.str, main='PCA biplot')

#windows()
screeplot(pca.results, type="line", main="PCA Scree Plot")






