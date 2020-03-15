##libraries
library(lavaan)
library(semPlot)

##load the data
crime.cov = lav_matrix_lower2full(c(.63, 
                                    .50, .60, 
                                    .48, .48, .58, 
                                    .47, .48, .51, .67))

crime.mean = c(5.17, 5.32, 5.40, 5.52)

names(crime.mean) = 
  rownames(crime.cov) = 
  colnames(crime.cov) = c("Time1", "Time2", "Time3", "Time4")

##models
# mean latent intercept and constrained residual variances
crime.model1 = '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
i~~0*i
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'
crime.fit1 = growth(crime.model1,
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)
summary(crime.fit1)
parameterestimates(crime.fit1, standardized=TRUE) ##CIs for parameters
fitmeasures(crime.fit1) ##fit indices

# mean latent intercept that is allowed to vary, and constrained residual variances
crime.model2 = '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'
crime.fit2 = growth(crime.model2, 
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)

summary(crime.fit2)

parameterestimates(crime.fit2, standardized=TRUE) ##CIs for parameters
fitmeasures(crime.fit2) ##fit indices


# mean latent intercept that is allowed to vary, 
##mean latent slope, and constrained residual variances
crime.model3 = '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# slope
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
s~0*1
s~~0*i
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'
crime.fit3 = growth(crime.model3, 
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)
summary(crime.fit3)
parameterestimates(crime.fit3, standardized=TRUE) ##CIs for parameters
fitmeasures(crime.fit3) ##fit indices

# mean latent intercept that is allowed to vary, 
##mean latent slope that is allowed to vary, and constrained residual variances
crime.model4 = '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# slope
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'
crime.fit4 = growth(crime.model4, 
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)

summary(crime.fit4)
parameterestimates(crime.fit4, standardized=TRUE) ##CIs for parameters
fitmeasures(crime.fit4) ##fit indices

# unconstrained model
crime.model5 = '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# slope
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
'
crime.fit5 = growth(crime.model5, 
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)

summary(crime.fit5)
parameterestimates(crime.fit5, standardized=TRUE) ##CIs for parameters
fitmeasures(crime.fit5) ##fit indices

