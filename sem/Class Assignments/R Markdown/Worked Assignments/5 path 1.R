library(lavaan)
library(semPlot)

##import the correlation matrix
data.cor = lav_matrix_lower2full(c(1, .178, 1, .230, .327, 1, .106, .245, .183, 1, .195, .356, .721, .178, 1))
rownames(data.cor) = colnames(data.cor) = c("race", "ses", "cog", "school", "acad")

model = ' 
acad ~ cog + race + ses + school
school ~ cog + race + ses
cog ~ race + ses
ses ~ race
'

model.fit = sem(model, sample.cov = data.cor, sample.nobs = 18058)
summary(model.fit, standardized=TRUE)
semPaths(model.fit, whatLabels = "par", layout= "spring")

##example 2
data.cov = lav_matrix_lower2full(c(84.85, 71.28, 140.34, 18.83, -6.25, 72.92, 60.05, 84.54, 37.18, 139.48))
rownames(data.cov) = colnames(data.cov) = c("teacher", "social", "material", "achieve")

model2 = '
achieve ~ b1*social + b2*material + c*teacher
material ~ a2*teacher
social ~ a1*teacher
indirect:= a1*b1
indirect2:=a2*b2
'

model.fit2 = sem(model2, sample.cov = data.cov, sample.nobs=40)
summary(model.fit2, standardized=TRUE)
semPaths(model.fit2, whatLabels = "par", layout="spring")
