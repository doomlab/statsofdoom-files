##install.packages("ltm")
library(ltm)
library(mirt)

##if you get an error run the following:
install.packages("mvtnorm")
install.packages("msm")

data(LSAT)
head(LSAT)

##run the model with logistic distribution using a 2PL
IRTmodel = ltm(LSAT ~ z1, IRT.param = TRUE)
##get the parameters
summary(IRTmodel)
coef(IRTmodel)
plot(IRTmodel, type = "ICC") ## all items at once
plot(IRTmodel, type = "ICC", items = 3) ## one item at a time
plot(IRTmodel, type = "IIC", items = 0) ## Test Information Function
factor.scores(IRTmodel)
person.fit(IRTmodel)
item.fit(IRTmodel)

##run with a 3PL
IRTmodel2 = tpm(LSAT, type = "latent.trait", IRT.param = TRUE)
summary(IRTmodel2)
coef(IRTmodel2)
plot(IRTmodel2, type = "ICC")
plot(IRTmodel2, type = "IIC", items = 0)
factor.scores(IRTmodel2)
person.fit(IRTmodel2)
item.fit(IRTmodel2)

anova(IRTmodel, IRTmodel2)

####polytomous IRT####
setwd("~/OneDrive - Missouri State University/TEACHING/751 SEM/class examples/f irt")
polydata = read.csv("poly irt.csv", header = T)
polydata = na.omit(polydata)
##reverse code if necessary
polydata$Q99_9 = 8 - polydata$Q99_9
##separate into separate factors

polydata1 = polydata[ , c(1, 4, 5, 6, 9)]
polydata2 = polydata[ , c(2, 3, 7, 8, 10)]

polymodel1 = mirt(data = polydata1, 
             model = 1, 
             itemtype = "gpcm")

summary(polymodel1) ##standardized coefficients 
coef(polymodel1, IRTpars = T) ##coefficients
itemplot(polymodel1, 1, type = "trace") ##curves for each item
itemplot(polymodel1, 1, type = "info") ##IIC for each item
plot(polymodel1, type = "trace") ##curves for all items at once
plot(polymodel1, type = "info") ##test information curve
plot(polymodel1) ##expected score curve
fscores(polymodel1) ##factor scores
itemfit(polymodel1) ##item fit statistics
personfit(polymodel1) ##person fit statistics

polymodel2 = mirt(data = polydata2, 
                  model = 1, 
                  itemtype = "gpcm")

summary(polymodel2) ##standardized coefficients 
coef(polymodel2, IRTpars = T) ##coefficients
itemplot(polymodel2, 4, type = "trace") ##curves for each item
itemplot(polymodel2, 1, type = "info") ##IIC for each item
plot(polymodel2, type = "trace") ##curves for all items at once
plot(polymodel2, type = "info") ##test information curve
plot(polymodel2) ##expected score curve
fscores(polymodel2) ##factor scores
itemfit(polymodel2) ##item fit statistics
personfit(polymodel2) ##person fit statistics

