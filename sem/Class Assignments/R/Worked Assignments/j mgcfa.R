library(lavaan)
library(semPlot)

#data 
data <- read.csv("~/e_files/TEACHING/751 SEM/class assignments/R/j mgcfa.csv")

##screen of the variable
table(data$race_test)
data = subset(data, race_test > 0)
data$race_test = factor(data$race_test, labels=c("Black", "White"))

##model
model = '
RS =~ RS1 + RS2 + RS3 + RS4 + RS5 + RS6 + RS7 + RS8 + RS9 + RS10 + RS11 + RS12 + RS13 + RS14
'

##all groups
allgroups.fit = cfa(model, data=data, meanstructure = TRUE)
summary(allgroups.fit, standardized=TRUE, rsquare=TRUE)
fitmeasures(allgroups.fit)
semPaths(allgroups.fit, whatLabels = "std", layout = "tree")

##separate group models 
white = subset(data, race_test == "White")
black = subset(data, race_test == "Black")

order = c("df", "chisq", "rmsea", "srmr", "cfi")

white.fit = cfa(model, data=white, meanstructure = TRUE)
summary(white.fit, standardized=TRUE, rsquare=TRUE)
fitmeasures(white.fit, fit.measures = order)

black.fit = cfa(model, data=black, meanstructure = TRUE)
summary(black.fit, standardized=TRUE, rsquare=TRUE)
fitmeasures(black.fit, fit.measures = order)

##configural invariance
configural.fit = cfa(model, data=data, meanstructure = TRUE, group = "race_test")
summary(configural.fit, standardized = TRUE, rsquare = TRUE)
fitmeasures(configural.fit, fit.measures = order)

#metric invariance
metric.fit = cfa(model, data=data, meanstructure = TRUE, group = "race_test",
                 group.equal = c("loadings"))
summary(metric.fit, standardized = TRUE, rsquare = TRUE)
fitmeasures(metric.fit, fit.measures = order)

#scalar invariance
scalar.fit = cfa(model, data=data, meanstructure = TRUE, group = "race_test",
                 group.equal = c("loadings","intercepts"))
summary(scalar.fit, standardized = TRUE, rsquare = TRUE)
fitmeasures(scalar.fit, fit.measures = order)

##figure out the issue
partialmod = modindices(scalar.fit)
intercepts = partialmod[partialmod$op == "~1", ]
intercepts[order(intercepts$mi, decreasing=TRUE), ]

##rules
##metric =~ loadings
##scalar ~1 intercepts
##strict ~~ variances


#partial invariance
partial.scalar.fit = cfa(model, data=data, meanstructure = TRUE, group = "race_test",
                 group.equal = c("loadings","intercepts"), group.partial = c("RS4 ~1"))
summary(partial.scalar.fit, standardized = TRUE, rsquare = TRUE)
fitmeasures(partial.scalar.fit, fit.measures = order)

#strict invariance
strict.fit = cfa(model, data=data, meanstructure = TRUE, group = "race_test",
                         group.equal = c("loadings","intercepts", "residuals"), 
                          group.partial = c("RS4 ~1"))
summary(strict.fit, standardized = TRUE, rsquare = TRUE)
fitmeasures(strict.fit, fit.measures = order)

#partial strict
##figure out the issue
partialmod = modindices(strict.fit)
residuals = partialmod[partialmod$op == "~~", ]
residuals = partialmod[partialmod$lhs == partialmod$rhs, ]
residuals[order(residuals$mi, decreasing=TRUE), ]

#partial strict
partial.strict.fit = cfa(model, data=data, meanstructure = TRUE, group = "race_test",
                 group.equal = c("loadings","intercepts", "residuals"), 
                 group.partial = c("RS4 ~1", "RS3 ~~  RS3"))
summary(partial.strict.fit, standardized = TRUE, rsquare = TRUE)
fitmeasures(partial.strict.fit, fit.measures = order)
