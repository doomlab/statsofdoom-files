##load the libraries
library(lavaan)
library(semPlot)


##data model 1
data.cor = lav_matrix_lower2full(c(1.00, 
                                   .74,	1.00,	
                                   .27,	.42,	1.00,	
                                   .31,	.40,	.79,	1.00,	
                                   .32,	.35,	.66,	.59,	1.00))

sd.cor = c(32.94,	22.75,	13.39,	13.68,	14.38)

rownames(data.cor) = 
  colnames(data.cor) = 
  names(sd.cor) = c("father", "mother", "famo", "problems", "intimacy" )

data.cov = cor2cov(data.cor, sd.cor)

##model 1
model = '
adjust =~ problems + intimacy
family =~ father + mother + famo'

##run model 1
model.fit = cfa(model, 
                sample.cov = data.cov, 
                sample.nobs = 203)

inspect(model.fit, "cov.lv")
inspect(model.fit, "cor.lv")

summary(model.fit, 
        rsquare=TRUE, 
        standardized=TRUE,
        fit.measures = TRUE)

parameterestimates(model.fit, standardized=TRUE) ##CIs for parameters
fitted(model.fit) ##look at cov table
residuals(model.fit) ##look at residuals
fitmeasures(model.fit) ##fit indices
modificationindices(model.fit) ##modification indices

semPaths(model.fit, 
         whatLabels="std", 
         layout="tree")
##should see errors here

##fix the errors
model.fit = cfa(model, 
                sample.cov = data.cor, 
                sample.nobs = 203)

summary(model.fit, 
        rsquare=TRUE, 
        standardized=TRUE,
        fit.measures = TRUE)

parameterestimates(model.fit, standardized=TRUE) ##CIs for parameters
fitted(model.fit) ##look at cov table
residuals(model.fit) ##look at residuals
fitmeasures(model.fit) ##fit indices
modificationindices(model.fit, sort. = T) ##modification indices

semPaths(model.fit, 
         whatLabels="std", 
         layout="tree")

###test the directional model
semmodel = '
adjust =~ problems + intimacy
family =~ father + mother + famo
adjust~family'

##analyze the directional model
model.fit = sem(semmodel, 
                sample.cov = data.cor, 
                sample.nobs = 203)

summary(model.fit, 
        rsquare=TRUE, 
        standardized=TRUE,
        fit.measures = TRUE)

parameterestimates(model.fit, standardized=TRUE) ##CIs for parameters
fitted(model.fit) ##look at cov table
residuals(model.fit) ##look at residuals
fitmeasures(model.fit) ##fit indices
modificationindices(model.fit, sort. = T) ##modification indices

semPaths(model.fit, 
         whatLabels="std", 
         layout="spring")

##example 2

##enter the data
data.cor = lav_matrix_lower2full(c(1.00, 
                                   .42,	1.00, 
                                   -.43,	-.50,	1.00, 
                                   -.39,	-.43,	.78,	1.00,	
                                   -.24,	-.37,	.69,	.73,	1.00, 
                                   -.31,	-.33,	.63,	.87,	.72,	1.00,	
                                   -.25,	-.25,	.49,	.53,	.60,	.59,	1.00, 
                                   -.25,	-.26,	.42,	.42,	.44,	.45,	.77,	1.00,	
                                   -.16,	-.18,	.23,	.36,	.38,	.38,	.59,	.58, 1.00))

sd.cor = c(13.00,	13.50,	13.10,	12.50,	13.50,	14.20,	9.50,	11.10,	8.70)

rownames(data.cor) = 
  colnames(data.cor) = 
  names(sd.cor) = c("parent_psych","low_SES","verbal",
                    "reading","math","spelling","motivation","harmony","stable")

data.cov = cor2cov(data.cor, sd.cor)

#build the model
model = '
risk <~ low_SES + parent_psych + verbal
achieve =~ reading + math + spelling
adjustment =~ motivation + harmony + stable
risk =~ achieve + adjustment
'

model.fit = sem(model, 
                sample.cov = data.cov, 
                sample.nobs = 158)

summary(model.fit, 
        rsquare=TRUE, 
        standardized=TRUE,
        fit.measures = TRUE)

parameterestimates(model.fit, standardized=TRUE) ##CIs for parameters
fitted(model.fit) ##look at cov table
residuals(model.fit) ##look at residuals
fitmeasures(model.fit) ##fit indices
modificationindices(model.fit, sort. = TRUE) ##modification indices

semPaths(model.fit, 
         whatLabels="std", 
         layout="tree")
