##libraries
library(lavaan)
library(semPlot)

##working directory
setwd(here::here())

##data
data = read.csv("mgcfa RS.csv")

##set up the data to have factor variables
##look at the data
table(data$Sex)

##exclude the bad data
data = subset(data, Sex < 3)

##make it a factor with labels
data$Sex = factor(data$Sex, labels = c("Men", "Women")) 
table(data$Sex)

####overall cfa everyone together####
overallmodel = '
RS =~ RS1 + RS2 + RS3 + RS4 + RS5 + RS6 + RS7 + RS8 + RS9 + RS10 + RS11 + RS12 + RS13 + RS14
'

overall.fit = cfa(model = overallmodel, 
                  data=data, 
                  meanstructure = TRUE)

summary(overall.fit, 
        standardized=TRUE, 
        rsquare=TRUE, 
        fit.measure = TRUE)

parameterestimates(overall.fit, standardized=TRUE) ##CIs for parameters
fitted(overall.fit) ##look at cov table
residuals(overall.fit) ##look at residuals
fitmeasures(overall.fit) ##fit indices
modificationindices(overall.fit) ##modification indices

semPaths(overall.fit, 
         whatLabels = "std", 
         layout = "tree")

####separate group models####
##first got to subset the data
men = subset(data, Sex == "Men")
women = subset(data, Sex =="Women")

men.fit = cfa(overallmodel, 
              data=men, 
              meanstructure=TRUE)

summary(men.fit, 
        standardized=TRUE, 
        rsquare=TRUE,
        fit.measure = TRUE)

parameterestimates(men.fit, standardized=TRUE) ##CIs for parameters
fitted(men.fit) ##look at cov table
residuals(men.fit) ##look at residuals
fitmeasures(men.fit) ##fit indices
modificationindices(men.fit) ##modification indices

semPaths(men.fit, 
         whatLabels = "std", 
         layout = "tree")

women.fit = cfa(overallmodel, 
                data=women, 
                meanstructure=TRUE)

summary(women.fit, 
        standardized=TRUE, 
        rsquare=TRUE,
        fit.measure = TRUE)

parameterestimates(women.fit, standardized=TRUE) ##CIs for parameters
fitted(women.fit) ##look at cov table
residuals(women.fit) ##look at residuals
fitmeasures(women.fit) ##fit indices
modificationindices(women.fit) ##modification indices

semPaths(women.fit,
         whatLabels = "std", 
         layout = "tree")

####multi group testing####
mg_models = measEq.syntax(overall.fit,
                          group = "Sex", 
                          ID.fac ="marker")
cat(as.character(mg_models))

####multi group testing####
library(semTools)
options(scipen = 999)
multisteps = measurementInvariance(overallmodel, 
                                   data = data, 
                                   group = "Sex",
                                   strict = T)
##we see that it breaks down at strict invariance
##therefore this next step will test the parameters for strict
##"metric", "scalar", "strict", or "means"
##save the testing
partial = partialInvariance(multisteps, 
                  type = "strict")
##save only the results for easier viewing
strictfree = partial$results
##click on the name, sort by FREE CFI
##release the biggest one first

partialstrict = measurementInvariance(overallmodel, 
                                   data = data, 
                                   group = "Sex",
                                   strict = T,
                                   group.partial = c("RS9~~RS9"))
##didn't quite bring us up to the right amount
##do the same partial steps as earlier
partial = partialInvariance(partialstrict, 
                            type = "strict")
##save only the results for easier viewing
strictfree = partial$results

partialstrict2 = measurementInvariance(overallmodel, 
                                      data = data, 
                                      group = "Sex",
                                      strict = T,
                                      group.partial = c("RS9~~RS9",
                                                        "RS13~~RS13"))

####side note####
##when you save these models, it saves all the individual models
configural = partialstrict2$fit.configural
summary(configural)

metric = partialstrict2$fit.loadings
summary(metric)

scalar = partialstrict2$fit.intercepts
summary(scalar)

strict = partialstrict2$fit.residuals
summary(strict)

##latent means
loadings = parameterestimates(strict)
menload = subset(loadings, group == "1" & op == "=~", select = "est")
womenload = subset(loadings, group == "2" & op == "=~", select = "est")

##figure out the means

##first, multiply the loadings by the values using apply function
##here you would want to change the columns used and the loading name
##if you have multiple factors, rearrange column order in apply
menmultiply = apply(men[, 4:ncol(men)], 1, function(x) { x * menload })
womenmultiply = apply(women[, 4:ncol(women)], 1, function(x) { x * womenload })

##next save that data as a data frame
menmultiply = as.data.frame(menmultiply)
menmultiply = t(menmultiply) ##flip it so it's people per row

womenmultiply = as.data.frame(womenmultiply)
womenmultiply = t(womenmultiply) ##flip it so it's people per row

##if you have multiple factors, separate here into menlatent1, 2, etc. 
##get the row mean for each person - use rowSums for total scores
menlatent = rowMeans(menmultiply)
womenlatent = rowMeans(womenmultiply)

##example for multiple factors
menlatent1 = apply(menmultiply[ , c(1,4, 8, 10)], 1, mean)
menlatent2 = apply(menmultiply[ , c(2, 3, 5, 6, 7)], 1, mean)

##let's do a ttest!
t.test(menlatent, womenlatent, 
       alternative = "two.sided", 
       paired=FALSE,
       var.equal = TRUE,
       na.action=TRUE)

##let's get the SD
sd(menlatent)
sd(womenlatent)
length(menlatent)
length(womenlatent)

##effect size
library(effsize)
cohen.d(menlatent, womenlatent)
