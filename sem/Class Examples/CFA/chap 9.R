library(lavaan)
library(semPlot)

##import the data
wisc4.cov = lav_matrix_lower2full(c(8.29,
                                    5.37,9.06,
                                    2.83,4.44,8.35,
                                    2.83,3.32,3.36,8.88,
                                    5.50,6.66,4.20,3.43,9.18,
                                    6.18,6.73,4.01,3.33,6.77,9.12,
                                    3.52,3.77,3.19,2.75,3.88,4.05,8.88,
                                    3.79,4.50,3.72,3.39,4.53,4.70,4.54,8.94,
                                    2.30,2.67,2.40,2.38,2.06,2.59,2.65,2.83,8.76,
                                    3.06,4.04,3.70,2.79,3.59,3.67,3.44,4.20,4.53,9.73))

wisc4.sd = c(2.88,3.01,2.89,2.98,3.03,3.02,2.98,2.99,2.96,3.12) 

names(wisc4.sd) = 
  colnames(wisc4.cov) = 
  rownames(wisc4.cov) =c("Comprehension", "Information", 
                         "Matrix.Reasoning", "Picture.Concepts", 
                         "Similarities", "Vocabulary",  "Digit.Span", 
                         "Letter.Number",  "Coding", "Symbol.Search") 


##first order model
wisc4.fourFactor.model ='
gc =~ Comprehension + Information +  Similarities + Vocabulary 
gf =~ Matrix.Reasoning + Picture.Concepts
gsm =~  Digit.Span + Letter.Number
gs =~ Coding + Symbol.Search
'   

wisc4.fourFactor.fit=cfa(model=wisc4.fourFactor.model, 
                         sample.cov=wisc4.cov, 
                         sample.nobs=550)

summary(wisc4.fourFactor.fit, 
        fit.measure=TRUE, 
        standardized=TRUE,
        rsquare = TRUE)

parameterestimates(wisc4.fourFactor.fit, standardized=TRUE) ##CIs for parameters
fitted(wisc4.fourFactor.fit) ##look at cov table
residuals(wisc4.fourFactor.fit) ##look at residuals
fitmeasures(wisc4.fourFactor.fit) ##fit indices
modificationindices(wisc4.fourFactor.fit, sort. = T) ##modification indices

semPaths(wisc4.fourFactor.fit, 
         whatLabels="std", 
         layout="tree")

##higher order model
wisc4.higherOrder.model='
gc =~ Comprehension + Information + Similarities + Vocabulary 
gf =~ Matrix.Reasoning + Picture.Concepts
gsm =~  Digit.Span + Letter.Number
gs =~ Coding + Symbol.Search

g=~ NA*gf + gc  + gsm + gs 
g~~ 1*g
'
#you can also do this:
wisc4.higherOrder.model='
gc =~ Comprehension + Information + Similarities + Vocabulary 
gf =~ Matrix.Reasoning + Picture.Concepts
gsm =~  Digit.Span + Letter.Number
gs =~ Coding + Symbol.Search

g=~ gf + gc  + gsm + gs 
'

wisc4.higherOrder.fit = cfa(model=wisc4.higherOrder.model, 
                            sample.cov=wisc4.cov, 
                            sample.nobs=550)

summary(wisc4.higherOrder.fit, 
        fit.measure=TRUE, 
        standardized=TRUE, 
        rsquare = TRUE)

parameterestimates(wisc4.higherOrder.fit, standardized=TRUE) ##CIs for parameters
fitted(wisc4.higherOrder.fit) ##look at cov table
residuals(wisc4.higherOrder.fit) ##look at residuals
fitmeasures(wisc4.higherOrder.fit) ##fit indices
modificationindices(wisc4.higherOrder.fit, sort. = T) ##modification indices

semPaths(wisc4.higherOrder.fit, 
         whatLabels="std", 
         layout="tree")

##bifactor model
wisc4.bifactor.model='
gc =~ Comprehension + Information +  Similarities + Vocabulary 
gf =~ a*Matrix.Reasoning + a*Picture.Concepts  
gsm =~  b*Digit.Span + b*Letter.Number
gs =~ c*Coding + c*Symbol.Search 
g =~ Information + Comprehension + Matrix.Reasoning + Picture.Concepts + Similarities + Vocabulary +  Digit.Span + Letter.Number + Coding + Symbol.Search
'
wisc4.bifactor.fit=cfa(model=wisc4.bifactor.model, 
                       sample.cov=wisc4.cov,
                       sample.nobs=550, 
                       orthogonal=TRUE)

##note I changed the std.lv=TRUE from the book 
##be sure to look at the standardized solution for the paths of the specific domains
##if you leave off std.lv = true, you can get the plots to work, otherwise no

summary(wisc4.bifactor.fit, 
        fit.measure=TRUE, 
        rsquare=TRUE, 
        standardized=TRUE)

parameterestimates(wisc4.bifactor.fit, standardized=TRUE) ##CIs for parameters
fitted(wisc4.bifactor.fit) ##look at cov table
residuals(wisc4.bifactor.fit) ##look at residuals
fitmeasures(wisc4.bifactor.fit) ##fit indices
modificationindices(wisc4.bifactor.fit, sort. = T) ##modification indices

semPaths(wisc4.bifactor.fit, 
         whatLabels = "std", 
         layout = "tree")
