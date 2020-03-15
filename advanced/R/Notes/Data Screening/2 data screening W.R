##working directory
setwd("~/OneDrive - Missouri State University/TEACHING/751 SEM/class assignments/R/")

##read in the dataset
master = read.csv("2 data screening.csv")

##accuracy
summary(master)

##missing
percentmiss = function(x) { sum(is.na(x)) / length(x) * 100}
missing = apply(master[ , -1], 1, percentmiss)
table(missing)
replacepeople = subset(master, missing <= 5)
summary(replacepeople)
apply(replacepeople[ , -1], 2, percentmiss)

nomiss = replacepeople

##outliers
mahal = mahalanobis(nomiss[ , -1],
                    colMeans(nomiss[ , -1]),
                    cov(nomiss[ , -1]))
cutoff = qchisq(1-.001, ncol(nomiss[ , -1])) ##cutoff
cutoff 
ncol(nomiss[ , -1]) ##df
table(mahal < cutoff)
noout = subset(nomiss, mahal < cutoff)

##additivity
correl = cor(nomiss[ , -1])
symnum(correl)

##assumption set up
random = rchisq(nrow(nomiss), 7)
fake = lm(random ~., data = nomiss[ , -1])
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##Linearity
qqnorm(standardized)
abline(0,1)

##homog and s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)
