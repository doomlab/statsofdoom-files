##for Dr. B to install on the computer 
install.packages("memisc")
install.packages("QuantPsyc")

###set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 15")
options(scipen = 999)

##import the datafile
library(memisc)
regdata = as.data.set(spss.system.file("c7 regression.sav"))
regdata = as.data.frame(regdata)

##get only the variables we want to use
##IVs = pil_total, audit_total_new
#DV = cesd_total
master = regdata[ , c(8:10)]

##check for missing and accuracy as with regular data screening
summary(master)

##before we can finish data screening, you actually have to run the regression
##normally you do mahal scores first, but we are going to run the regression
##to look at all the outlier stuff at the same time

####multiple regression####
##run the regression
model1 = lm(cesd_total ~ pil_total + audit_total_new, data = master)

##outliers
##mahal - don't forget to drop categorical columns if you have them
mahal = mahalanobis(master, 
                    colMeans(master), 
                    cov(master))
cutmahal = qchisq(1-.001, ncol(master))
badmahal = as.numeric(mahal > cutmahal) ##note the direction of the > 
table(badmahal)

##leverage
k = 2 ##number of IVs
leverage = hatvalues(model1)
cutleverage = (2*k+2) / nrow(master)
badleverage = as.numeric(leverage > cutleverage)
table(badleverage)
  
##cooks
cooks = cooks.distance(model1)
cutcooks = 4 / (nrow(master) - k - 1)
badcooks = as.numeric(cooks > cutcooks)
table(badcooks)

##overall outliers
##add them up!
totalout = badmahal + badleverage + badcooks
table(totalout)

##get rid of them!
noout = subset(master, totalout < 2)

##ok now that we got rid of outliers, we need to run that model again
##otherwise the next set of assumption tests is wrong and includes those people
model2 = lm(cesd_total ~ pil_total + audit_total_new, data = noout)

##multicollinearity
##you could run this the old way but remember you WANT IV-DV correlations
##or use this new output! Ignore the intercept part
summary(model2, correlation = TRUE)

##assumptions
standardized = rstudent(model2)
fitted = scale(model2$fitted.values)

##linearity
qqnorm(standardized)
abline(0,1)

##normality
hist(standardized)

##homog and s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

##regression statistics
summary(model2)

##get betas
library(QuantPsyc)
lm.beta(model2)

##get pr
##you can just run the correlations but this code will help you remember to square it
library(ppcor)
partials = pcor(noout)
partials$estimate^2 

####hierarhical regression####
library(memisc)
hdata = as.data.set(spss.system.file("c7 dummy code.sav"))
hdata = as.data.frame(hdata)

##do the data screening as above

##model 1 control for family history
model1 = lm(after ~ familyhistory, data = hdata)
summary(model1)
##since there's only one predictor b = beta = R, pr2 = sr2 = R2

##model 2 test treatment types
model2 = lm(after ~ familyhistory + treat, data = hdata)
summary(model2)
##note if everything is continuous, you can use lm.beta()
##but with categorical, lm.beta is wonk.

##compare models
anova(model1, model2)

##figure out what those categories mean
##use these numbers to get cohen's d (independent t)
with(hdata, tapply(after, treat, mean))
with(hdata, tapply(after, treat, sd))
with(hdata, tapply(after, treat, length))

##get pr squared for this type of data
t = 4.052
dfresidual = 44

t^2 / (t^2+dfresidual)
