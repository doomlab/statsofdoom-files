##mtmm example

#load the libraries
library(lavaan)
library(semPlot)

##working directory
setwd("~/OneDrive - Missouri State University/TEACHING/751 SEM/class examples/c mtmm")

##read the data
data = read.csv("mtmm example.csv")

##set up model cfas
methods = '
mlq =~ m1+m2+m3+m4+m5+m6+m8+m9+m10
pil =~ p3+p4+p8+p12+p17+p20
'

traits = '
meaning =~ m1 + m2 + m5 + m10 + p4 + p12 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + p3 + p8 + p20
'

methods.fit = cfa(methods, 
                  data=data, 
                  std.lv=TRUE)

summary(methods.fit, 
        rsquare=TRUE, 
        standardized=TRUE,
        fit.measure = TRUE)

parameterestimates(methods.fit, standardized=TRUE) ##CIs for parameters
fitted(methods.fit) ##look at cov table
residuals(methods.fit) ##look at residuals
fitmeasures(methods.fit) ##fit indices
modificationindices(methods.fit) ##modification indices

semPaths(methods.fit, 
         whatLabels = "std", 
         layout = "tree")

traits.fit = cfa(traits, 
                 data=data, 
                 std.lv=TRUE)

inspect(traits.fit, "cor.lv")

summary(traits.fit, 
        rsquare=TRUE, 
        standardized=TRUE,
        fit.measure = TRUE)

parameterestimates(traits.fit, standardized=TRUE) ##CIs for parameters
fitted(traits.fit) ##look at cov table
residuals(traits.fit) ##look at residuals
fitmeasures(traits.fit) ##fit indices
modificationindices(traits.fit, sort. = T) ##modification indices

semPaths(traits.fit, 
         whatLabels = "std", 
         layout = "tree")

##complete model

model1 = '
mlq =~ m1+m2+m3+m4+m5+m6+m8+m9+m10
pil =~ p3+p4+p8+p12+p17+p20
meaning =~ m1 + m2 + m5 + m10 + p4 + p12 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + p3 + p8 + p20

##fix the covariances
mlq~~0*meaning
pil~~0*meaning
mlq~~0*purpose
pil~~0*purpose
'

model1.fit = cfa(model1, 
                 data=data, 
                 std.lv=TRUE)

summary(model1.fit, 
        rsquare=TRUE, 
        standardized=TRUE,
        fit.measure = TRUE)

parameterestimates(model1.fit, standardized=TRUE) ##CIs for parameters
fitted(model1.fit) ##look at cov table
residuals(model1.fit) ##look at residuals
fitmeasures(model1.fit) ##fit indices
modificationindices(model1.fit) ##modification indices

semPaths(model1.fit, 
         whatLabels = "std", 
         layout = "tree")

###model 2
model2 = '
mlq =~ m1+m2+m3+m4+m5+m6+m8+m9+m10
pil =~ p3+p4+p8+p12+p17+p20
'

model2.fit = cfa(model2, 
                 data=data, 
                 std.lv=TRUE)

summary(model2.fit, 
        rsquare=TRUE, 
        standardized=TRUE, 
        fit.measure = TRUE)

parameterestimates(model2.fit, standardized=TRUE) ##CIs for parameters
fitted(model2.fit) ##look at cov table
residuals(model2.fit) ##look at residuals
fitmeasures(model2.fit) ##fit indices
modificationindices(model2.fit) ##modification indices

semPaths(model2.fit, 
         whatLabels = "std", 
         layout = "tree")

anova(model1.fit, model2.fit)

##model 3
model3 = '
mlq =~ m1+m2+m3+m4+m5+m6+m8+m9+m10
pil =~ p3+p4+p8+p12+p17+p20
meaning =~ m1 + m2 + m5 + m10 + p4 + p12 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + p3 + p8 + p20

##fix the covariances
mlq~~0*meaning
pil~~0*meaning
mlq~~0*purpose
pil~~0*purpose
meaning~~1*purpose
'

model3.fit = cfa(model3, 
                 data=data, 
                 std.lv=TRUE)

summary(model3.fit, 
        rsquare=TRUE, 
        standardized=TRUE, 
        fit.measure = TRUE)

parameterestimates(model3.fit, standardized=TRUE) ##CIs for parameters
fitted(model3.fit) ##look at cov table
residuals(model3.fit) ##look at residuals
fitmeasures(model3.fit) ##fit indices
modificationindices(model3.fit) ##modification indices

semPaths(model3.fit,
         whatLabels = "std", 
         layout = "tree")

anova(model1.fit, model3.fit)

#model 4
model4 = '
mlq =~ m1+m2+m3+m4+m5+m6+m8+m9+m10
pil =~ p3+p4+p8+p12+p17+p20
meaning =~ m1 + m2 + m5 + m10 + p4 + p12 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + p3 + p8 + p20

##fix the covariances
mlq~~0*meaning
pil~~0*meaning
mlq~~0*purpose
pil~~0*purpose
pil~~0*mlq
'

model4.fit = cfa(model4, 
                 data=data, 
                 std.lv=TRUE)

summary(model4.fit, 
        rsquare=TRUE, 
        standardized=TRUE, 
        fit.measure = TRUE)

parameterestimates(model4.fit, standardized=TRUE) ##CIs for parameters
fitted(model4.fit) ##look at cov table
residuals(model4.fit) ##look at residuals
fitmeasures(model4.fit) ##fit indices
modificationindices(model4.fit) ##modification indices

semPaths(model4.fit, 
         whatLabels = "std", 
         layout = "tree")

anova(model1.fit, model4.fit)
