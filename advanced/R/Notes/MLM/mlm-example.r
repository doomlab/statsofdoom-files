##set working directory
setwd("~/Downloads")

#import the data
example = read.csv("mlm example.csv")

##factor the categorical variables
example$gender = factor(example$gender, 
                        levels = c(1, 2),
                        labels = c("Male", "Female"))
example$college = factor(example$college,
                         levels = c(1,2,3),
                         labels = c("Arts", "Sciences", "Math"))

##data screening
##accuracy
summary(example)

##missing data
summary(example)

##outliers
mahal = mahalanobis(example[ , -c(1,2)],
                    colMeans(example[ , -c(1,2)], na.rm = TRUE),
                    cov(example[ , -c(1,2)], use = "pairwise.complete.obs"))
summary(mahal)
cutoff = qchisq(1-.001, ncol(example[ , -c(1,2)]))
summary(mahal < cutoff)
example = example[mahal < cutoff , ]

##multicollinearity
correlation = cor(example[ , -c(1,2)])
symnum(correlation)

##assumptions
random = rchisq(nrow(example), 7)
fake = lm(random ~ ., data = example)
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
library(reshape)
library(nlme)
##need a participant number
example$partno = c(1:nrow(example))
example$tech_exp = scale(example$tech_exp, scale = F)
example$comp_exp = scale(example$comp_exp, scale = F)

examplelong = melt(example,
                   id = c("partno", "gender", "college", "comp_anx"),
                   measured = c("tech_exp", "comp_exp"))
##intercept only model
##gls = generalized least squares
##ML = maximum likelihood
model1 = gls(value ~ 1, 
             data = examplelong, 
             method = "ML", 
             na.action = "na.omit")
summary(model1)

##random intercept only model
##note we switched to LME function
model2 = lme(value ~ 1, 
             data = examplelong, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model2)
anova(model1, model2)

##let's pretend we need to do the MLM (nest the variables)
##let's add predictors
model3 = lme(value ~ gender + college + comp_anx, 
             data = examplelong, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model3)
summary(examplelong)

tapply(examplelong$value, examplelong$gender, mean)
tapply(examplelong$value, examplelong$college, mean)

##random slopes
model4 = lme(value ~ gender + college + comp_anx,
             data = examplelong, 
             method = "ML", 
             na.action = "na.omit",
             random = ~ comp_anx|partno,
             control = lmeControl(msMaxIter = 200))
summary(model4)
anova(model3, model4)
