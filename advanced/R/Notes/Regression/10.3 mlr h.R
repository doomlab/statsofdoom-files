##dr b's working directory
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/10 Regression")

##import the file
master = read.csv("data 2.csv")

##accuracy
summary(master)

master$sex = factor(master$sex, 
                    levels = c(0,1),
                    labels = c("Female", "Male"))

##run the FINAL!! model to get assumption checks
output = lm(car ~ sex + age + extro, data = master)

##outliers
##mahal
mahal = mahalanobis(master[ , -1],
                    colMeans(master[ , -1]),
                    cov(master[ , -1]))
cutoff = qchisq(1-.001, ncol(master[ , -1]))
cutoff ##cutoff score
ncol(master[ , -1]) ##df
badmahal = as.numeric(mahal > cutoff) ##notice the direction of > 
table(badmahal)

##leverage
k = 3 ##number of IVs in the final step
leverage = hatvalues(output)
cutleverage = (2*k+2) / nrow(master)
cutleverage ##cut off
badleverage = as.numeric(leverage > cutleverage)
table(badleverage)

##cooks
cooks = cooks.distance(output)
cutcooks = 4 / (nrow(master) - k - 1)
cutcooks ##get the cut off
badcooks = as.numeric(cooks > cutcooks)
table(badcooks)

##overall outliers
##add them up!
totalout = badmahal + badleverage + badcooks
table(totalout)

##get rid of them!
noout = subset(master, totalout < 2)

##run a no outlier analysis otherwise these graphs
##will include the outliers
##be sure to run the final model
output = lm(car ~ sex + age + extro, data = noout)

##additivity
correl = cor(noout[ , -1], use="pairwise.complete.obs")
correl
symnum(correl)
summary(output, correlation = T)

##assumption set up
standardized = rstudent(output)
fitted = scale(output$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity and homoscedasticity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##first step real regression
model1 = lm(car ~ sex + age, data = noout)
summary(model1)

##get beta
library(QuantPsyc)
lm.beta(model1)

##pr values
library(ppcor)
partials = pcor(noout[ , c(2,4)], method = "pearson")
partials$estimate ^ 2 ##make sure pr is squared!

##t^2 / (t^2 + dfresidual)
6.004^2 / (6.004^2 + 36)
2.923^2 / (2.923^2 + 36)

##second step real regression
model2 = lm(car ~ sex + age + extro, data = noout)
summary(model2)

##compare models
anova(model1, model2)

##get beta
library(QuantPsyc)
lm.beta(model2)

##pr values
library(ppcor)
partials = pcor(noout[ , c(2:4)], method = "pearson")
partials$estimate ^ 2 ##make sure pr is squared!

##graph
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

##get final fitted values
fitted = model2$fitted.values

##make the plot
scatter = ggplot(noout, aes(fitted, car))
scatter +
  cleanup +
  geom_point() + 
  geom_smooth(method = "lm", color = "black") +
  xlab("Gender + Age + Extroversion") +
  ylab("Car Care Score")
