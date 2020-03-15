library(ltm)

#load the data
data <- read.csv("~/e_files/TEACHING/751 SEM/class assignments/R/m IRT exam.csv", header=FALSE)

##just the 2:5 questions
data = data[ , 2:5]

##summary of data
summary(data)

##2PL
model = ltm(data ~ z1, IRT.param = TRUE)
coef(model)
plot(model, type="ICC")
abline(.5,0)
plot(model, type = "IIC")
plot(model, type = "IIC", items = 0)
factor.scores(model)
person.fit(model)
item.fit(model)

##3PL
model2 = tpm(data, type="latent.trait", IRT.param = TRUE)
options(scipen = 999)
coef(model2)
plot(model, type="ICC")
plot(model, type = "IIC", items = 0)
factor.scores(model)
person.fit(model)
item.fit(model)

anova(model,model2)
summary(model)
summary(model2)
