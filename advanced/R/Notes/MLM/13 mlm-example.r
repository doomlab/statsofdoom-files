##set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/13 MLM")

#import the data
example = read.csv("mlm_example3.csv")

####data screening####
##if your data are in wide format, data screen this way first
##accuracy
summary(example)

##missing data
summary(example)

##outliers
mahal = mahalanobis(example[ , -c(1,2)],
                    colMeans(example[ , -c(1,2)], na.rm = TRUE),
                    cov(example[ , -c(1,2)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(example[ , -c(1,2)]))
ncol(example[ , -c(1,2)]) ##df
cutoff ##cutoff
summary(mahal < cutoff)
noout = example[mahal < cutoff , ]
summary(noout)

##multicollinearity/additivity
correlation = cor(noout[ , -c(1,2)], use = "pairwise.complete.obs")
symnum(correlation)
correlation

##assumptions
random = rchisq(nrow(example), 7)
fake = lm(random ~ ., data = example[ , -c(1,2)])
fitted = scale(fake$fitted.values)
standardized = rstudent(fake)

##linearity
qqnorm(standardized)
abline(0,1)

##normality
hist(standardized)

##homog / homos
plot(fitted, standardized)
abline(0,0)
abline(v=0)

##run the analysis

##set up the analysis
library(nlme)

#####intercept only model####
##gls = generalized least squares
##ML = maximum likelihood
model1 = gls(latency ~ 1, 
             data = noout, 
             method = "ML", 
             na.action = "na.omit")
summary(model1)

####random intercept only model####
##note we switched to LME function
model2 = lme(latency ~ 1, 
             data = noout, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model2)
anova(model1, model2)

####second level####
model2.1 = lme(latency ~ 1, 
             data = noout, 
             method = "ML", 
             na.action = "na.omit",
             random = list(~1|partno, ~1|trialcode))
summary(model2.1)
anova(model1, model2, model2.1)

####predictor model####
model3 = lme(latency ~ response, 
             data = noout, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model3)
anova(model1, model2, model3)

####random slopes####
model4 = lme(latency ~ response,
             data = noout, 
             method = "ML", 
             na.action = "na.omit",
             random = ~ response|partno,
             control = lmeControl(msMaxIter = 200))
summary(model4)
anova(model1, model2, model3, model4)

####assumptions from mlm####
screen = lme(latency ~ response,
             data = noout, 
             method = "ML", 
             na.action = "na.omit",
             random = ~ response|partno,
             control = lmeControl(msMaxIter = 200))

standardized = as.data.frame(scale(screen$residuals))
standardized = standardized$fixed
fitted = scale(fitted.values(screen))

##Linearity
qqnorm(standardized)
abline(0,1)

##normality
hist(standardized)

##homog and s
plot(fitted,standardized)
abline(0,0)
abline(v=0)
