##set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/14 EFA")

##import the file
master = read.csv("14 EFA data.csv")

##accuracy
summary(master)

##recode
table(master$q6)
master[ , c(6,9,17,27)] = 8 - master[ , c(6,9,17,27)]
table(master$q6)

##missing
percentmissing = function (x){ sum(is.na(x))/length(x) * 100}
missing = apply(master, 1, percentmissing)
table(missing)

##exclude the participant missing too much data
replacepeople = subset(master, missing <= 5)

##make sure the columns aren't missing too much
apply(replacepeople, 2, percentmissing)

##replace away!
library(mice)
tempnomiss = mice(replacepeople)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##outliers
cutoff = qchisq(1-.001, ncol(nomiss))
mahal = mahalanobis(nomiss,
                    colMeans(nomiss),
                    cov(nomiss))
cutoff ##cutoff score
ncol(nomiss) ##df
summary(mahal < cutoff)

##exclude outliers
noout = subset(nomiss, mahal < cutoff)

##additivity
correl = cor(noout, use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
random = rchisq(nrow(noout), 7)
fake = lm(random~., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##running the efa analysis
library(psych)
library(GPArotation)

##correlation adequacy Bartlett's test
cortest.bartlett(correl, n = nrow(noout))

##sampling adequacy KMO test
KMO(correl)

##how many factors?
nofactors = fa.parallel(noout, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a three factor model
round1 = fa(noout, nfactors=3, rotate = "oblimin", fm = "ml")
round1

round2 = fa(noout[ , -c(4,15)], nfactors=3, rotate = "oblimin", fm = "ml")
round2

##get cfi
finalmodel = fa(noout[ , -c(4,15)], nfactors=3, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

##reliability
factor1 = c(1, 3, 7, 8, 10:12, 14, 16, 18, 20:25, 29, 31, 32)
factor2 = c(2, 5, 13, 19, 26, 28, 30)
factor3 = c(6, 9, 17, 27)
psych::alpha(noout[ , factor1])
psych::alpha(noout[ , factor2])
psych::alpha(noout[ , factor3])

##create new factor scores
noout$f1 = apply(noout[ , factor1], 1, mean) ##creates average scores
noout$f2 = apply(noout[ , factor2], 1, mean) ##creates average scores
noout$f3 = apply(noout[ , factor3], 1, mean) ##creates average scores

summary(noout)
sd(noout$f1)
sd(noout$f2)
sd(noout$f3)
