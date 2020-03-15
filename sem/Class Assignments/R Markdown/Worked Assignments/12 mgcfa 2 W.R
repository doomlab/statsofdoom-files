##set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/751 SEM/class assignments/R")

master = read.csv("12 mgcfa.csv")

library(lavaan)
library(semTools)

overallmodel = '
depression =~ Q3 + Q5 + Q10 + Q13 + Q16  + Q17 + Q21
anxiety =~ Q2 + Q4 + Q7 + Q9 + Q15 + Q19 + Q20
stress =~ Q1 + Q6 + Q8 + Q11 + Q12 + Q14 + Q18
'

overallmodel.fit = cfa(overallmodel,
                       data = master,
                       meanstructure = T)
summary(overallmodel.fit,
        rsquare = T,
        standardized = T,
        fit.measures = T)

##group subset
male = subset(master, gender == "male")
female = subset(master, gender == "female")

overallmodel.fit.M = cfa(overallmodel,
                       data = male,
                       meanstructure = T)
summary(overallmodel.fit.M,
        rsquare = T,
        standardized = T,
        fit.measures = T)

overallmodel.fit.F = cfa(overallmodel,
                       data = female,
                       meanstructure = T)
summary(overallmodel.fit.F,
        rsquare = T,
        standardized = T,
        fit.measures = T)

multisteps = measurementInvariance(overallmodel,
                                   data = master, 
                                   group = 'gender',
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)
fitmeasures(multisteps$fit.residuals)

partial = partialInvariance(multisteps,
                            type = "strict")
strictfree = partial$results

multisteps2 = measurementInvariance(overallmodel,
                                   data = master, 
                                   group = 'gender',
                                   strict = T,
                                   group.partial = c("Q11~~Q11"))

fitmeasures(multisteps2$fit.residuals)
summary(multisteps2$fit.residuals)

##latent means
loadings = parameterestimates(multisteps2$fit.residuals)
womenload = subset(loadings, group == "1" & op == "=~", select = "est")
menload = subset(loadings, group == "2" & op == "=~", select = "est")

##figure out the means

##first, multiply the loadings by the values using apply function
##here you would want to change the columns used and the loading name
##if you have multiple factors, rearrange column order in apply
womenmultiply = apply(female[ , 3:ncol(female)], 1, function(x) { x * womenload })
menmultiply = apply(male[ , 3:ncol(male)], 1, function(x) { x * menload })

##next save that data as a data frame
menmultiply = as.data.frame(menmultiply)
menmultiply = t(menmultiply) ##flip it so it's people per row

womenmultiply = as.data.frame(womenmultiply)
womenmultiply = t(womenmultiply) ##flip it so it's people per row

##if you have multiple factors, separate here into menlatent1, 2, etc. 
##get the row mean for each person - use rowSums for total scores
menlatentD = rowMeans(menmultiply[ , 1:7])
womenlatentD = rowMeans(womenmultiply[ , 1:7])

menlatentA = rowMeans(menmultiply[ , 8:14])
womenlatentA = rowMeans(womenmultiply[ , 8:14])

menlatentS = rowMeans(menmultiply[ , 15:21])
womenlatentS = rowMeans(womenmultiply[ , 15:21])

##let's do three ttests!
t.test(menlatentD, womenlatentD, 
       alternative = "two.sided", 
       paired=FALSE,
       var.equal = TRUE,
       na.action=TRUE)

t.test(menlatentA, womenlatentA, 
       alternative = "two.sided", 
       paired=FALSE,
       var.equal = TRUE,
       na.action=TRUE)

t.test(menlatentS, womenlatentS, 
       alternative = "two.sided", 
       paired=FALSE,
       var.equal = TRUE,
       na.action=TRUE)

##let's get the SD
sd(menlatentD)
sd(womenlatentD)
sd(menlatentA)
sd(womenlatentA)
sd(menlatentS)
sd(womenlatentS)

length(menlatent)
length(womenlatent)

##effect size
library(effsize)
cohen.d(menlatentD, womenlatentD)
cohen.d(menlatentA, womenlatentA)
cohen.d(menlatentS, womenlatentS)

