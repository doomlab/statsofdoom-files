##dr b's working directory
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/11 Med Mod")

##import the file
master = read.csv("data 6.csv")

##accuracy
summary(master)

##run the FINAL!! model to get assumption checks
output = lm(budget ~ league + innings, data = master)

##outliers
##mahal
mahal = mahalanobis(master[ , -1],
                    colMeans(master[ , -1]),
                    cov(master[ , -1]))
cutoff = qchisq(1-.001, ncol(master[ , -1]))
cutoff ##cutoff score
ncol(master[ , -1]) ##df
badmahal = as.numeric(mahal > cutoff) ##notice the direction of > 
table(badmahal)

##leverage
k = 2 ##number of IVs in the final step
leverage = hatvalues(output)
cutleverage = (2*k+2) / nrow(master)
cutleverage ##cut off
badleverage = as.numeric(leverage > cutleverage)
table(badleverage)

##cooks
cooks = cooks.distance(output)
cutcooks = 4 / (nrow(master) - k - 1)
cutcooks ##get the cut off
badcooks = as.numeric(cooks > cutcooks)
table(badcooks)

##overall outliers
##add them up!
totalout = badmahal + badleverage + badcooks
table(totalout)

##get rid of them!
noout = subset(master, totalout < 2)

##run a no outlier analysis otherwise these graphs
##will include the outliers
##be sure to run the final model
output = lm(budget ~ league + innings, data = noout)

##assumption set up
standardized = rstudent(output)
fitted = scale(output$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity and homoscedasticity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##mediation analysis
modelc = lm(innings ~ league, data = noout)
summary(modelc)

modela = lm(budget ~ league, data = noout)
summary(modela)

modelb = lm(innings ~ league + budget, data = noout)
summary(modelb)

##sobel
library(multilevel)
sobel(noout$league, noout$budget, noout$innings)
save = sobel(noout$league, noout$budget, noout$innings)
pnorm(abs(save$z.value), lower.tail = F)*2

##interpretation for dummy coded variables
tapply(noout$innings, list(noout$league), mean)
tapply(noout$budget, list(noout$league), mean)
