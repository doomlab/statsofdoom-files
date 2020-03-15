##moderated mediation script example
##Dr. Erin M. Buchanan
##June 1, 2017

####set working directory and import files####
library(haven)
setwd("~/OneDrive - Missouri State University/RESEARCH/8 stat help/statstools doc")
master = read_sav("model 7 moderate mediate.sav")

####data screening####
##accuracy
summary(master)
notypos = master[ , -5]
summary(notypos)

##missing
##none seen in the summary

##outliers
##mahalanobis across entire dataset minus any categorical variables
mahal = mahalanobis(notypos, 
                    colMeans(notypos),
                    cov(notypos))
cutoff = qchisq(1-.001, ncol(notypos))
table(mahal < cutoff)
noout = subset(notypos, mahal < cutoff)

##correlations additivity
correl = cor(notypos)
correl
symnum(correl)

##fake regression style assumptions check
random = rchisq(nrow(noout), 7)
fake = lm(random ~ ., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##linearity
qqnorm(standardized)
abline(0,1)

##normality
hist(standardized)

##Homog/s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

####mediation####
##c path, x predicts y
cpath = lm(y ~ x, data = noout)
summary(cpath)

##a path, x predicts m
apath = lm(m ~ x, data = noout)
summary(apath)

##b path, m predicts y with x
##c' path x is diminished with m predicting y
bpath = lm(y ~ x + m, data = noout)
summary(bpath)

##aroian sobel
a = apath$coefficients[2]
b = bpath$coefficients[3]
SEa = coef(summary(apath))[ , "Std. Error"][2]
SEb = coef(summary(bpath))[ , "Std. Error"][3]
zscore = (a*b)/(sqrt((b^2*SEa^2)+(a^2*SEb^2)+(SEa*SEb)))
zscore
pnorm(abs(zscore), lower.tail = F)*2

total = cpath$coefficients[2] ##c path
direct = bpath$coefficients[2] ##c' path
indirect = a*b

total; direct; indirect

##bootstrapping the mediation effect
##write a function that gives you the numbers you want
##we want the indirect effect
indirectsaved = function (dataset, random) {
  d = dataset[random, ] ##randomize by row
apath = lm(m ~ x, data = d)
bpath = lm(y ~ x + m, data = d)
indirect = apath$coefficients[2]*bpath$coefficients[3]
return(indirect)
}

library(boot)
bootresults = boot(data = noout,
                   statistic = indirectsaved,
                   R = 1000)
bootresults
boot.ci(bootresults, 
        conf = .95,
        type = "norm")

####moderated mediation####
##figure out where you want the moderation
##this example shows you moderation on path c and c'

##mean center continuous variables in moderation
noout$cx = scale(noout$x, scale = F)
noout$cw = scale(noout$w, scale = F)

##c path, x predicts y
cpath = lm(y ~ cx*cw, data = noout)
summary(cpath)

##a path, x predicts m
apath = lm(m ~ cx, data = noout)
summary(apath)

##b path, m predicts y with x
##c' path x is diminished with m predicting y
bpath = lm(y ~ cx*cw + m, data = noout)
summary(bpath)

##c' path for simple slopes at w
noout$lowcw = noout$cw + sd(noout$cw)
noout$highcw = noout$cw - sd(noout$cw)

##conditional direct effects c' paths
##low results
bpathlow = lm(y ~ cx*lowcw + m, data = noout)
summary(bpathlow)
##average results
summary(bpath)
##high results
bpathhigh = lm(y ~ cx*highcw + m, data = noout)
summary(bpathhigh)

##aroian sobel
a = apath$coefficients[2]
b = bpath$coefficients[4]
SEa = coef(summary(apath))[ , "Std. Error"][2]
SEb = coef(summary(bpath))[ , "Std. Error"][4]
zscore = (a*b)/(sqrt((b^2*SEa^2)+(a^2*SEb^2)+(SEa*SEb)))
zscore
pnorm(abs(zscore), lower.tail = F)*2

indirect = apath$coefficients[2]*bpath$coefficients[4]
indirect

##bootstrapping the moderated mediation effect
##write a function that gives you the numbers you want
##we want the indirect effect
indirectsaved = function (dataset, random) {
  d = dataset[random, ] ##randomize by row
  apath = lm(m ~ cx, data = d)
  bpath = lm(y ~ cx*cw + m, data = d)
  indirect = apath$coefficients[2]*bpath$coefficients[4]
  return(indirect)
}

library(boot)
bootresults = boot(data = noout,
                   statistic = indirectsaved,
                   R = 1000)
bootresults
boot.ci(bootresults, 
        conf = .95,
        type = "norm")
