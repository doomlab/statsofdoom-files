##Import the data
setwd("~/e_files/TEACHING/527 Adv Statistics/2016 - R/1 notes and examples/3 Data screening")
imported = read.csv("DS data.csv")

##accuracy
notypos = imported
summary(notypos)

##deal with factoring
##deal with the space in gender and level
notypos$gender = factor(notypos$gender, 
                        levels = c("female", "male"),
                        labels = c("Female", "Male"))
notypos$level = factor(notypos$level,
                       levels = c("freshman", "sophomore", "junior", "senior", "masters", "phd"),
                       labels = c("Freshman", "Sophomore", "Junior", "Senior", "Masters", "PhD"))

##actually factor something that's an int or num
table(notypos$advising)
notypos$advising = factor(notypos$advising,
                          levels = c(1, 2), 
                          labels = c("Yes", "No"))

##min and max problems
summary(notypos)
notypos$q5[ notypos$q5 < 1] = 1
notypos$q8[ notypos$q8 < 1] = 1
notypos$q14[ notypos$q14 < 1] = 1
notypos$q2[ notypos$q2 > 7] = NA
notypos$q9[ notypos$q9 > 7] = NA
notypos$q13[ notypos$q13 > 7] = NA
notypos$q14[ notypos$q14 > 7] = NA
notypos$q15[ notypos$q15 > 7] = NA

##missing data
percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}
##participants (row)
missing = apply(notypos, 1, percentmiss)
missing

replacepeople = subset(notypos, missing <= 5)
nopeople  = subset(notypos, missing > 5)

##by variable (column)
apply(replacepeople, 2, percentmiss)
names(replacepeople)
replaceall = replacepeople[ , 4:26]
nocolumn = replacepeople[ , 1:3]

library(mice)
tempnomiss = mice(replaceall)
replaced = complete(tempnomiss, 1)
summary(replaced)

allcolumns = cbind(replaced,nocolumn)
allrows = rbind(allcolumns, nopeople)

##outliers
mahal = mahalanobis(allcolumns[ , -c(24:26)],
                    colMeans(allcolumns[ , -c(24:26)], na.rm = T),
                    cov(allcolumns[ , -c(24:26)], use = "pairwise.complete.obs"))

cutoff = qchisq(1-.001, ncol(allcolumns[ , -c(24:26)]))
ncol(allcolumns[ , -c(24:26)])
cutoff
summary(mahal < cutoff)
noout = subset(allcolumns, mahal < cutoff)

##assumptions
##additivity
correl = cor(noout[ , -c(24:26)])
symnum(correl)

final = noout

##set up for assumptions
random = rchisq(nrow(final), 7)
fake = lm(random ~ ., data = final)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
library(moments)
skewness(final$q1, na.rm = T)
skewness(final[ , -c(24:26)], na.rm = T)
kurtosis(final[ , -c(24:26)], na.rm = T)
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homog + s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)
abline(v = -3)
abline(v = 3)
abline(h = 2)
abline(h = -2)
