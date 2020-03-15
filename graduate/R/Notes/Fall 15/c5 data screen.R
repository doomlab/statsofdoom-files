##set my working directory
setwd("~/e_files/TEACHING/745 Grad Statistics/notes/fall 15")

##import the data to get started
master = read.csv("c5 data screen.csv", header=TRUE)

####accuracy
##check categorical data
table(master$Sex)
table(master$SES)

##just saving as a different dataset
notypos = master

##fix the categorical labels and typos
notypos$Sex = factor(notypos$Sex, 
                     levels = c(1,2), 
                     labels = c("Women", "Men"))
notypos$SES = factor(notypos$SES, 
                     levels = c(1,2, 3),
                     labels = c("Low", "Medium", "High"))
table(notypos$Sex)
table(notypos$SES)

##now check for continuous variable problems
summary(notypos)

##grade and absences check
table(notypos$Grade)
table(notypos$Absences)

notypos[ notypos$Grade > 34, 
         "Grade"] = NA
table(notypos$Grade)

notypos[ notypos$Absences > 34, 
         "Absences"] = NA
table(notypos$Absences)

##fix the RS columns all at once (good for things with common rules)
##first figure out which columns you want to replace
notypos[ , 6:19]
##what are the rules for replacing
notypos[ , 6:19] > 7
##put those two things together
notypos[ , 6:19][ notypos[ , 6:19] > 7 ]
# now overwrite with a number/missing code
notypos[ , 6:19][ notypos[ , 6:19] > 7 ] = NA
summary(notypos)

##get just the means and SDs by COLUMN
apply(notypos, 2, mean) ##we have to deal with the factor variables
notypos[, -c(1,3)]
apply(notypos[, -c(1,3)], 2, mean) ##we have deal with the missing data
apply(notypos[, -c(1,3)], 2, mean, na.rm = TRUE)
apply(notypos[, -c(1,3)], 2, sd, na.rm = TRUE)
####accuracy

####missing data
summary(notypos)
View(notypos)

##percent missing function
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##columns
apply(notypos, 2, percentmiss) 
##exclude 13

##rows
apply(notypos, 1, percentmiss) ##ok that's crazy speak, let's save it
missing = apply(notypos, 1, percentmiss) 
table(missing)

##get ready to replace
install.packages("mice")
library(mice)

##replace only the data that you should
##subset out the bad rows
replacepeople = notypos[ missing < 6 , ]  ##note we are going to fudge a little bit
dontpeople = notypos[ missing >= 6 , ]

##figure out the columns to exclude
replacecolumn = replacepeople[ , -c(1, 3, 13)]
dontcolumn = replacepeople[ , c(1,3,13)]

##let's mice it!
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##put everything back together
filledin_none = cbind(dontcolumn, nomiss)
filledin_missing = rbind(dontpeople, filledin_none) ##if you want to exclude people 
                                                    ##completely with missing cases, 
                                                    ##you can skip this step

####missing

####outliers
##first, drop all the factor columns, as mahalanobis only runs on continuous variables
##or only run on the variables you are using for this analysis. 
filledin_none[ , -c(1,2)]

##create the mahalanobis scores
mahal = mahalanobis(filledin_none[ , -c(1,2)], 
                    colMeans(filledin_none[ , -c(1,2)], na.rm = TRUE),
                    cov(filledin_none[ , -c(1,2)], use="pairwise.complete.obs"))
mahal

##now get rid of the people who have bad scores
##what's a bad score?
##find the cut off
cutoff = qchisq(.999,ncol(filledin_none[ , -c(1,2)])) 

##figure out who's bad
summary(mahal < cutoff)

##ditch'em!
noout = filledin_none[ mahal < cutoff, ]
####outliers

####assumptions

##additivity: correlations
correlations = cor(noout[,-c(1,2)], use="pairwise.complete.obs")
correlations
symnum(correlations)

##make the random stuff
random = rchisq(nrow(noout), 7)
fake = lm(random~., data=noout)

##get the linearity plot
##create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##normality
install.packages("moments")
library(moments)

##get the values
skewness(noout[ , -c(1,2)], na.rm=TRUE)
kurtosis(noout[ , -c(1,2)], na.rm=TRUE)
hist(noout$RS12)

##multivariate normality
hist(standardized, breaks=15)

##homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
