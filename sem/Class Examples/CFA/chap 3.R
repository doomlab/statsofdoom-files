library(lavaan)
library(semPlot)

##example 1 - one latent variable
##enter the correlation matrix
wisc4.cor = lav_matrix_lower2full(c(1,
                                    0.72,1,
                                    0.64,0.63,1,
                                    0.51,0.48,0.37,1,
                                    0.37,0.38,0.38,0.38,1))

# enter the SDs
wisc4.sd = c(3.01 , 3.03 , 2.99 , 2.89 , 2.98)

# name the variables
colnames(wisc4.cor) = 
  rownames(wisc4.cor) = 
  c("Information", "Similarities", 
    "Word.Reasoning", "Matrix.Reasoning", "Picture.Concepts")

names(wisc4.sd) =  c("Information", "Similarities", 
                     "Word.Reasoning", "Matrix.Reasoning", 
                      "Picture.Concepts")

# convert correlations and SDs to covarainces
wisc4.cov = cor2cov(wisc4.cor,wisc4.sd)

# specify single factor model
wisc4.model='
g =~ a*Information + b*Similarities + c*Word.Reasoning + d*Matrix.Reasoning + e*Picture.Concepts
'
# fit model
wisc4.fit = cfa(model = wisc4.model, 
                sample.cov = wisc4.cov, 
                sample.nobs = 550,  
                std.lv = FALSE)

# examine parameter estimates
summary(wisc4.fit,
        standardized=TRUE, 
        rsquare = TRUE,
        fit.measures=TRUE)

##note you can also use the following - 
##gives you the CIs
parameterestimates(wisc4.fit,
                   standardized=TRUE)

##other new functions
fitted(wisc4.fit) ##look at cov table
residuals(wisc4.fit) ##look at residuals
fitmeasures(wisc4.fit) ##fit indices
modificationindices(wisc4.fit) ##modification indices

##draw the model
semPaths(wisc4.fit, 
         whatLabels="par", 
         layout="tree")

semPaths(wisc4.fit, 
         whatLabels="std", 
         layout="tree")

##example 2 - only standardized on latent
wisc4.fit.Std = cfa(wisc4.model, 
                    sample.cov=wisc4.cov, 
                    sample.nobs=550, 
                    std.lv=TRUE)

summary(wisc4.fit.Std,
        standardized=TRUE, 
        rsquare = TRUE,
        fit.measures=TRUE)

parameterestimates(wisc4.fit.Std,
                   standardized=TRUE)
fitted(wisc4.fit.Std) ##look at cov table
residuals(wisc4.fit.Std) ##look at residuals
fitmeasures(wisc4.fit.Std) ##fit indices
modificationindices(wisc4.fit.Std) ##modification indices

semPaths(wisc4.fit.Std, 
         whatLabels="par", 
         layout="tree")

##example 3 - two factor model
wisc4.model2='
V =~ a*Information + b*Similarities + c*Word.Reasoning 
F =~ d*Matrix.Reasoning + e*Picture.Concepts
V~~f*F
'
wisc4.fit2 = cfa(wisc4.model2, 
                 sample.cov=wisc4.cov, 
                 sample.nobs=550,
                 std.lv = F)

summary(wisc4.fit2,
        standardized=TRUE, 
        rsquare = TRUE,
        fit.measures=TRUE)

parameterestimates(wisc4.fit2,
                   standardized=TRUE)
fitted(wisc4.fit2) ##look at cov table
residuals(wisc4.fit2) ##look at residuals
fitmeasures(wisc4.fit2) ##fit indices
modificationindices(wisc4.fit2) ##modification indices

anova(wisc4.fit, wisc4.fit2)

semPaths(wisc4.fit2, 
         whatLabels="std", 
         layout="tree")

##fully latent model
wisc4SEM.model = '
# define latent variables
V =~ a*Information + b*Similarities + c*Word.Reasoning 
F =~ d*Matrix.Reasoning + e*Picture.Concepts
# define structural relations
V~k*F
'
wisc4SEM.fit = cfa(wisc4SEM.model, 
                   sample.cov=wisc4.cov, 
                   sample.nobs=550,
                   std.lv = F)

summary(wisc4SEM.fit,
        standardized=TRUE, 
        rsquare = TRUE,
        fit.measures=TRUE)

parameterestimates(wisc4SEM.fit,
                   standardized=TRUE)
fitted(wisc4SEM.fit) ##look at cov table
residuals(wisc4SEM.fit) ##look at residuals
fitmeasures(wisc4SEM.fit) ##fit indices
modificationindices(wisc4SEM.fit) ##modification indices

semPaths(wisc4SEM.fit, 
         whatLabels="par", 
         layout="spring")
