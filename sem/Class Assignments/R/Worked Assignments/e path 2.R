library(lavaan)
library(semPlot)

data.cor = lav_matrix_lower2full(c(1, .19, 1, -.16, -.20, 1, -.37, -.06, .36, 1, -.06, -.05, -.03, -.25, 1, .13, -.06, -.09, -.28, .41, 1))
rownames(data.cor) = colnames(data.cor) = c("agg", "with", "edu", "age", "emotion", "conduct")

model = '
conduct ~ agg + age + edu
emotion ~ age + edu + with
edu ~ with
age ~ agg
age ~~ edu
'

model.fit = sem(model, sample.cov = data.cor, sample.nobs = 200)
summary(model.fit, standardized=TRUE, fit.measures=TRUE)
fitMeasures(model.fit)
semPaths(model.fit, whatLabels = "par", layout = "spring")

model2 = 'conduct ~ age + edu
emotion ~ age + edu 
'

model.fit2 = sem(model2, sample.cov = data.cor, sample.nobs = 200)
summary(model.fit2, standardized=TRUE, fit.measures=TRUE)
fitMeasures(model.fit2)
semPaths(model.fit2, whatLabels = "par", layout = "spring")

model3 = 'conduct ~ agg 
emotion ~ with
'
model.fit3 = sem(model3, sample.cov = data.cor, sample.nobs = 200)
summary(model.fit3, standardized=TRUE, fit.measures=TRUE)
fitMeasures(model.fit3)
semPaths(model.fit3, whatLabels = "par", layout = "spring")
