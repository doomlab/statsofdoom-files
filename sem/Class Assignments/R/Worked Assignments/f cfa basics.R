library(lavaan)
library(semPlot)

##load the data
data <- read.csv("~/e_files/TEACHING/751 SEM/class assignments/R/f cfa basics.csv")

##create the models
three.model = '
familiar =~ q3 + q13 + q14 + q16 + q20 + q21 + q22 + q23 + q27 + q30
attitudes =~ q1 + q2 + q4 + q5 + q8 + q11 + q18 + q19 + q28 + q29
aversion =~ q6 + q7 + q9 + q10 + q12 + q15 + q17 + q24 + q25 + q26
'

one.model = '
computer =~ q3 + q13 + q14 + q16 + q20 + q21 + q22 + q23 + q27 + q30 + q1 + q2 + q4 + q5 + q8 + q11 + q18 + q19 + q28 + q29 + q6 + q7 + q9 + q10 + q12 + q15 + q17 + q24 + q25 + q26
'

##run the models
three.fit = cfa(three.model, data = data)
one.fit = cfa(one.model, data = data)

##create pictures
semPaths(three.fit, whatLabels="std", layout="tree")
semPaths(one.fit, whatLabels = "std", layout = "tree")


##summaries
summary(three.fit, standardized=TRUE, rsquare=TRUE)
modindices(three.fit, sort. = TRUE, minimum.value = 30.00)
summary(one.fit, standardized=TRUE, rsquare=TRUE)
fitMeasures(three.fit)
fitMeasures(one.fit)

##residual correlations
correl = residuals(three.fit, type="cor")
View(correl$cor)
zcorrel = residuals(three.fit, type = "standardized")
View(zcorrel$cov)
