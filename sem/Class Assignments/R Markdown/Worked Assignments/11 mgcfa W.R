##working directory
setwd("~/OneDrive - Missouri State University/TEACHING/751 SEM/class assignments/R")

##import data
master = read.csv("11 mgcfa.csv")

##factor the variable
master$race_test = factor(master$race_test,
                          levels = c(1, 2),
                          labels = c("Black", "White"))
table(master$race_test)

##overall model
overallmodel = '
RS =~ RS1 + RS2 + RS3 + RS4 + RS5 + RS6 + RS7 + RS8 + RS9 + RS10 + RS11 + RS12 + RS13 + RS14
'

##overall model cfa
library(lavaan)
library(semPlot)
library(semTools)

overallmodel.fit = cfa(overallmodel,
                       data = master,
                       meanstructure = T)
summary(overallmodel.fit, 
        rsquare = T, 
        standardized = T,
        fit.measures = T)

##subset out groups
white = subset(master, race_test == "White")
black = subset(master, race_test == "Black")

##white
overallmodel.fit.W = cfa(overallmodel,
                       data = white,
                       meanstructure = T)
summary(overallmodel.fit.W, 
        rsquare = T, 
        standardized = T,
        fit.measures = T)

##black
overallmodel.fit.B = cfa(overallmodel,
                       data = black,
                       meanstructure = T)
summary(overallmodel.fit.B, 
        rsquare = T, 
        standardized = T,
        fit.measures = T)

##measurement invariance
options(scipen = 999)
masternomiss = subset(master, race_test == "Black" | race_test == "White")
multisteps = measurementInvariance(overallmodel, 
                                   data = masternomiss, 
                                   group = "race_test",
                                   strict = T)

fitmeasures(multisteps$fit.configural)
fitmeasures(multisteps$fit.loadings)
fitmeasures(multisteps$fit.intercepts)

partial = partialInvariance(multisteps, 
                            type = "intercepts")
interceptfree = partial$results

multisteps2 = measurementInvariance(overallmodel, 
                                   data = masternomiss, 
                                   group = "race_test",
                                   strict = T,
                                   group.partial = c("RS4~1"))
fitmeasures(multisteps2$fit.intercepts)
fitmeasures(multisteps2$fit.residuals)

partial = partialInvariance(multisteps2, 
                            type = "strict")
interceptfree = partial$results