##libraries
library(lavaan)
library(semPlot)
library(effsize)

#data 
data <- read.csv("~/e_files/TEACHING/751 SEM/class assignments/R/k mgcfa.csv")

##check the data for problems (data screening should be here)
table(data$gender)

##program the overall model
model = '
depression =~ Q3+Q5+Q10+Q13+Q16+Q17+Q21
anxiety =~ Q2+Q4+Q7+Q9+Q15+Q19+Q20
stress =~ Q1+Q6+Q8+Q11+Q12+Q14+Q18
'

order = c("df", "chisq", "rmsea", "srmr", "cfi")

##an overall model no groups
overall.fit = cfa(model, data=data, meanstructure = TRUE)
summary(overall.fit, standardized = T, rsquare=T)
fitmeasures(overall.fit, fit.measures = order)

##test each group separately
##subset the data
women = subset(data, gender == "female")
men = subset(data, gender == "male")

#women group
women.fit = cfa(model, data=women, meanstructure = TRUE)
summary(women.fit, standardized = T, rsquare=T)
fitmeasures(women.fit, fit.measures = order)

#men group
men.fit = cfa(model, data=men, meanstructure = TRUE)
summary(men.fit, standardized = T, rsquare=T)
fitmeasures(men.fit, fit.measures = order)

##configural invariance
configural.fit = cfa(model, data=data, meanstructure = TRUE,
                     group="gender")
summary(configural.fit, standardized = T, rsquare=T)
semPaths(configural.fit, whatLabels = "std", layout = "tree")
fitmeasures(configural.fit, fit.measures = order)

##metric invariance
metric.fit = cfa(model, data=data, meanstructure = TRUE,
                     group="gender", group.equal = c("loadings"))
summary(metric.fit, standardized = T, rsquare=T)
fitmeasures(metric.fit, fit.measures = order)

##scalar invariance
metric.fit = cfa(model, data=data, meanstructure = TRUE,
                 group="gender", group.equal = c("loadings", "intercepts"))
summary(metric.fit, standardized = T, rsquare=T)
fitmeasures(metric.fit, fit.measures = order)

##strict invariance
strict.fit = cfa(model, data=data, meanstructure = TRUE,
                 group="gender", group.equal = c("loadings", "intercepts", "residuals"))
summary(strict.fit, standardized = T, rsquare=T)
fitmeasures(strict.fit, fit.measures = order)

#partial strict invariance 
partialmod = modindices(strict.fit)
variances = subset(partialmod, op == "~~")
variances2 = subset(variances, lhs == rhs)
variances2[order(variances2$mi, decreasing=T) , ]

##rules
##metric =~ loadings
##scalar ~1 intercepts
##strict ~~ variances

partial.strict.fit = cfa(model, data=data, meanstructure = TRUE,
                 group="gender", group.equal = c("loadings", "intercepts", "residuals"),
                 group.partial = c("Q21 ~~ Q21"))
summary(partial.strict.fit, standardized = T, rsquare=T)
fitmeasures(partial.strict.fit, fit.measures = order)

##weighted scores (latent means)
loadings = parameterestimates(partial.strict.fit)

##get the men and women loadings
womenload = subset(loadings, group=="1" & op == "=~", select = "est")
menload = subset(loadings, group == "2" & op == "=~", select = "est")


##multiply the loadings by the participant scores
##but first!!!! make sure the columns are in the same order~!!!
##loadings are in this order: Q3+Q5+Q10+Q13+Q16+Q17+Q21 Q2+Q4+Q7+Q9+Q15+Q19+Q20 Q1+Q6+Q8+Q11+Q12+Q14+Q18
##look at your dataset and that order
##data Q3 Q5 Q10 Q13 Q16 Q17 Q21 Q2 Q4 Q7 Q9 Q15 Q19 Q20 Q1 Q6 Q8 Q11 Q12 Q14 Q18

##if they aren't in the same order
## women[ , c("Q3", "Q5", "Q27")]
## women[ , c(1, 4, 7, 15)]

dim(womenload)
dim(women)

##multipled out
womenmultiply = apply(women[ , -c(1:2)], 1, function (x) {x * womenload})
menmultiply = apply(men[-c(1:2)], 1, function(x) {x * menload})

##combine together into a data frame
womenmultiply = as.data.frame(womenmultiply)
womenmultiply = t(womenmultiply)

menmultiply = as.data.frame(menmultiply)
menmultiply = t(menmultiply)

##column names
colnames(menmultiply) = colnames(womenmultiply) = c("Q3"," Q5"," Q10"," Q13"," Q16"," Q17",
"Q21"," Q2"," Q4"," Q7"," Q9"," Q15"," Q19"," Q20"," Q1",
"Q6"," Q8"," Q11"," Q12"," Q14"," Q18")

##create latent means = rowMeans
##create latent totals = rowSums

##get means for each latent variable
womendepress = rowMeans(womenmultiply[ , 1:7])
womenanx = rowMeans(womenmultiply[ , 8:14])
womenstress = rowMeans(womenmultiply[ , 9:21])

mendepress = rowMeans(menmultiply[ , 1:7])
menanx = rowMeans(menmultiply[ , 8:14])
menstress = rowMeans(menmultiply[ , 9:21])

##t.test
t.test(womendepress, mendepress, alternative = "two.sided", paired = FALSE, na.action = na.omit)
sd(womendepress)
sd(mendepress)
cohen.d(womendepress, mendepress)

t.test(womenanx, menanx, alternative = "two.sided", paired = FALSE, na.action = na.omit)
sd(womenanx)
sd(menanx)
cohen.d(womenanx, menanx)

t.test(womenstress, menstress, alternative = "two.sided", paired = FALSE, na.action = na.omit)
sd(womenstress)
sd(menstress)
cohen.d(womenstress, menstress)

