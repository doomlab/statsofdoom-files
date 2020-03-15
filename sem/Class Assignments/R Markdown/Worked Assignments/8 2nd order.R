library(lavaan)
library(semPlot)

##import the dataset
setwd("/Users/buchanan/OneDrive - Missouri State University/TEACHING/751 SEM/class assignments/R")
data <- read.csv("8 2nd order.csv")

##create the model - first order model
first.model = '
depression =~ Q3+Q5+Q10+Q13+Q16+Q17+Q21
anxiety =~ Q2+Q4+Q7+Q9+Q15+Q19+Q20
stress =~ Q1+Q6+Q8+Q11+Q12+Q14+Q18
'

##second order
second.model = '
depression =~ Q3+Q5+Q10+Q13+Q16+Q17+Q21
anxiety =~ Q2+Q4+Q7+Q9+Q15+Q19+Q20
stress =~ Q1+Q6+Q8+Q11+Q12+Q14+Q18
global =~ depression + anxiety + stress
'
alternative.model = '
depression =~ Q3+Q5+Q10+Q13+Q16+Q17+Q21
anxiety =~ Q2+Q4+Q7+Q9+Q15+Q19+Q20
stress =~ Q1+Q6+Q8+Q11+Q12+Q14+Q18
global =~ NA*depression + anxiety + stress
global ~~ 1*global 
'
##bifactor model
bifactor.model = '
depression =~ Q3+Q5+Q10+Q13+Q16+Q17+Q21
anxiety =~ Q2+Q4+Q7+Q9+Q15+Q19+Q20
stress =~ Q1+Q6+Q8+Q11+Q12+Q14+Q18
global =~ Q3+Q5+Q10+Q13+Q16+Q17+Q21+Q2+Q4+Q7+Q9+Q15+Q19+Q20+Q1+Q6+Q8+Q11+Q12+Q14+Q18
'

##run the models
first.fit = cfa(first.model, data=data)
second.fit = cfa(second.model, data=data)
bifactor.fit = cfa(bifactor.model, data=data, orthogonal = TRUE, std.lv = TRUE)

##pictures
semPaths(first.fit, whatLabels = "std", layout="tree")
semPaths(second.fit, whatLabels = "std", layout="tree")
semPaths(bifactor.fit, whatLabels = "std", layout="tree")

##fit indicees
fitMeasures(first.fit)
fitMeasures(second.fit)
fitmeasures(bifactor.fit)

#summary
summary(first.fit, standardized = TRUE, rsquare=TRUE)
summary(second.fit, standardized = TRUE, rsquare=TRUE)
summary(bifactor.fit, standardized = TRUE, rsquare=TRUE)
