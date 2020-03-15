##set my working directory
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 16")

##import the data to get started
master = read.csv("c5 data screen.csv", header=TRUE)

####accuracy####
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

notypos$Grade[ notypos$Grade > 34 ] = NA
table(notypos$Grade)

notypos$Absences[ notypos$Absences > 34 ] = NA
table(notypos$Absences)

##fix the RS columns all at once (good for things with common rules)
##first figure out which columns you want to replace
names(notypos)
head(notypos[ , 6:19])
##what are the rules for replacing
notypos[ , 6:19] > 7
##put those two things together
notypos[ , 6:19][ notypos[ , 6:19] > 7 ]
# now overwrite with a number/missing code
notypos[ , 6:19][ notypos[ , 6:19] > 7 ] = NA
summary(notypos)

##get just the means and SDs by COLUMN
apply(notypos, 2, mean) ##we have to deal with the factor variables
head(notypos[, -c(1,3)])
apply(notypos[, -c(1,3)], 2, mean) ##we have deal with the missing data
apply(notypos[, -c(1,3)], 2, mean, na.rm = TRUE)
apply(notypos[, -c(1,3)], 2, sd, na.rm = TRUE)

####missing data####
summary(notypos)
View(notypos)

##percent missing function
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

##rows - participants
##do this first to eliminate participants with too much missing data
##like participants that don't finish the experiment
apply(notypos, 1, percentmiss) ##ok that's crazy speak, let's save it
missing = apply(notypos, 1, percentmiss) 
table(missing)
##subset out the bad rows
##Make this 5% for your homework!!
replacepeople = subset(notypos, missing <= 6)  ##note we are going to fudge a little bit
dontpeople = subset(notypos, missing > 6)

##columns
##be sure to use the replacepeople dataset 
##otherwise you are including the participants who didn't finish or are mnar
apply(replacepeople, 2, percentmiss) 
##we can replace all the continuous columns
##figure out the columns to exclude
replacecolumn = replacepeople[ , -c(1,3)]
dontcolumn = replacepeople[ , c(1,3)]

##get ready to replace
install.packages("mice")
library(mice)

##replace only the data that you should
##let's mice it!
tempnomiss = mice(replacecolumn)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##put everything back together
allcolumns = cbind(dontcolumn, nomiss)
summary(allcolumns)

##if you want to exclude people completely with missing cases you can skip this step
allrows = rbind(dontpeople, allcolumns) 
summary(allrows)
##notice that it matched them based on column name (whew)

##here I decided to ignore all rows, because I need all the data 
nomissing = allcolumns

####outliers####
##first, drop all the factor columns, as mahalanobis only runs on continuous variables
##or only run on the variables you are using for this analysis. 
head(nomissing[ , -c(1, 2)])

##create the mahalanobis scores
mahal = mahalanobis(nomissing[ , -c(1,2)], 
                    colMeans(nomissing[ , -c(1,2)], na.rm = TRUE),
                    cov(nomissing[ , -c(1,2)], use="pairwise.complete.obs"))
mahal

##now get rid of the people who have bad scores
##what's a bad score?
##find the cut off
cutoff = qchisq(1 - .001,ncol(nomissing[ , -c(1,2)])) 

##for questions Dr. B asks and write ups
ncol(nomissing[ , -c(1,2)]) ##this is df
cutoff ##this is cutoff score

##figure out who's bad
##false is bad! 
summary(mahal < cutoff)

##ditch'em!
noout = subset(nomissing, mahal < cutoff)

####assumptions####

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

##multivariate normality
hist(standardized, breaks=15)

##homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)
