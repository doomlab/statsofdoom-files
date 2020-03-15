library(lavaan)
library(semPlot)

#data
data=lav_matrix_lower2full(c(1.00,
                             .53,	1.00,	
                             .15,	.18,	1.00,		
                             .52,	.29,	-.05,	1.00,	
                             .30,	.34,	.23,	.09,	1.00))
colnames(data) = 
  rownames(data) = 
  c("morale", "illness", "neuro", "relationship", "SES") 

#model 1
model1 = 'illness ~ morale
relationship ~ morale
morale ~ SES + neuro
'

#model 2
model2 = 'SES ~ illness + neuro
morale ~ SES + illness
relationship ~ morale + neuro
'

#model 1

model1.fit = sem(model1, 
                 sample.cov=data, 
                 sample.nobs=469)
summary(model1.fit, 
        rsquare=TRUE, 
        fit.measures=TRUE)

fitmeasures(model1.fit)

semPaths(model1.fit, 
         whatLabels="par", 
         layout="spring")

#model 2

model2.fit = sem(model2, 
                 sample.cov=data, 
                 sample.nobs=469)
summary(model2.fit, 
        rsquare=TRUE, 
        fit.measures=TRUE)

fitmeasures(model2.fit)

semPaths(model2.fit, 
         whatLabels="par", 
         layout="spring")

options(scipen = 999)
anova(model1.fit, model2.fit)
pchisq(40.303 - 3.245, 4-3, lower.tail = F)
