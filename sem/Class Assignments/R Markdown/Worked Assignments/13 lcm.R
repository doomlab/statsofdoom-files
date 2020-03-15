library(lavaan)
library(semPlot)

##load the data
heart.cov = lav_matrix_lower2full(c(
  3.59,				
  3.11,	3.10,			
  2.91,	2.80,	2.82,		
  3.22,	3.05,	2.86,	3.30,	
  2.88,	2.63,	2.62,	2.82,	2.71)
  )

heart.mean = c(11.97,	11.72,	12.03,	11.96,	12.10)

##give it some names
names(heart.mean) = rownames(heart.cov) = 
  colnames(heart.cov) = c("Time1", "Time2", "Time3", "Time4", "Time5")

##intercept only model (intercept average)
model1 = '
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4 + 1*Time5
i~~0*i
Time1 ~~ r*Time1
Time2 ~~ r*Time2
Time3 ~~ r*Time3
Time4 ~~ r*Time4
Time5 ~~ r*Time5
'

order = c("df", "chisq", "rmsea", "srmr", "cfi")

model1.fit = growth(model1, sample.cov = heart.cov, sample.mean = heart.mean, sample.nobs = 200)
fitmeasures(model1.fit, fit.measures = order)
summary(model1.fit)

##intercept mean and variance
model2 = '
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4 + 1*Time5
Time1 ~~ r*Time1
Time2 ~~ r*Time2
Time3 ~~ r*Time3
Time4 ~~ r*Time4
Time5 ~~ r*Time5
'

model2.fit = growth(model2, sample.cov = heart.cov, sample.mean = heart.mean, sample.nobs = 200)
fitmeasures(model2.fit, fit.measures = order)
summary(model2.fit)

#model 3 slope added no slope average
model3 = '
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4 + 1*Time5
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4 + 4*Time5
s ~ 0*1 ##average slope across time points
s ~~ 0*i ##covariance of slope and intercept
Time1 ~~ r*Time1
Time2 ~~ r*Time2
Time3 ~~ r*Time3
Time4 ~~ r*Time4
Time5 ~~ r*Time5
'
model3.fit = growth(model3, sample.cov = heart.cov, sample.mean = heart.mean, sample.nobs = 200)
fitmeasures(model3.fit, fit.measures = order)
summary(model3.fit)


#model 4 slope average variance and covariance 
model4 = '
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4 + 1*Time5
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4 + 4*Time5
Time1 ~~ r*Time1
Time2 ~~ r*Time2
Time3 ~~ r*Time3
Time4 ~~ r*Time4
Time5 ~~ r*Time5
'
model4.fit = growth(model4, sample.cov = heart.cov, sample.mean = heart.mean, sample.nobs = 200)
fitmeasures(model4.fit, fit.measures = order)
summary(model4.fit, standardized=T)


#model  all things estimated
model5 = '
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4 + 1*Time5
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4 + 4*Time5
'
model5.fit = growth(model5, sample.cov = heart.cov, sample.mean = heart.mean, sample.nobs = 200)
fitmeasures(model5.fit, fit.measures = order)
summary(model5.fit, standardized=T)
