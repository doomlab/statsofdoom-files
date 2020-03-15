##dr b's working directory
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/10 Regression")

##import the file
master = read.csv("data 1.csv")

##accuracy
summary(master)

##run the final model to get assumption checks
output = lm(grade ~ books + attend, data = master)

##outliers
##mahal
mahal = mahalanobis(master,
                    colMeans(master),
                    cov(master))
cutoff = qchisq(1-.001, ncol(master))
cutoff ##cutoff score
ncol(master) ##df
badmahal = as.numeric(mahal > cutoff) ##notice the direction of > 
table(badmahal)

##leverage
k = 2 ##number of IVs
leverage = hatvalues(output)
cutleverage = (2*k+2) / nrow(master)
cutleverage ##cut off
badleverage = as.numeric(leverage > cutleverage)
table(badleverage)

##cooks
cooks = cooks.distance(output)
cutcooks = 4 / (nrow(master) - k - 1)
cutcooks
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
output = lm(grade ~ books + attend, data = noout)

##additivity
correl = cor(noout, use="pairwise.complete.obs")
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

##final real regression
model = lm(grade ~ books + attend, data = noout)
summary(model)

##get beta
library(QuantPsyc)
lm.beta(model)

##pr values
library(ppcor)
partials = pcor(noout, method = "pearson")
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
fitted = model$fitted.values

##make the plot
scatter = ggplot(noout, aes(fitted, grade))
scatter +
  cleanup +
  geom_point() + 
  geom_smooth(method = "lm", color = "black") +
  xlab("Books + Attendance") +
  ylab("Grades in Class")
  