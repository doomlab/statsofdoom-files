##working directory
setwd("~/OneDrive - Missouri State University/TEACHING/751 SEM/class assignments/R")

##import file
master = read.csv("15 mirt.csv")

##recode
table(master$q5)
master$q5 = 8 - master$q5
master$q8 = 8 - master$q8
master$q13 = 8 - master$q13

summary(master)

##gpcm
library(mirt)
polymodel = mirt(data = master, 
                  model = 1, 
                  itemtype = "gpcm")

summary(polymodel) ##standardized coefficients 
coef(polymodel, IRTpars = T) ##coefficients

##three graphs of five items each
plot(polymodel, type = "trace") ##curves for all items at once
plot(polymodel, type = "info") ##test information curve

itemplot(polymodel, 12, type = "info") ##IIC for each item
itemplot(polymodel, 5, type = "trace") ##curves for each item

polymodel = mirt(data = master[ , -c(5,8,9,13)], 
                 model = 1, 
                 itemtype = "gpcm")
