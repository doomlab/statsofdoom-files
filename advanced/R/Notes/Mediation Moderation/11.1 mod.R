##dr b's working directory
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/11 Med Mod")

##import the file
master = read.csv("data 1.csv")

##accuracy
summary(master)

##run the final model to get assumption checks
output = lm(grade ~ scale(books, scale = F) * scale(attend, scale = F), 
            data = master)

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
k = 3 ##number of IVs
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
output = lm(grade ~ scale(books, scale = F) * scale(attend, scale = F), 
            data = noout)

##additivity not necessary bec we have simple moderation 
##correl = cor(noout, use="pairwise.complete.obs")
##correl
##symnum(correl)

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

##moderation 
library(QuantPsyc)

##run the regression
model = moderate.lm(books, attend, grade, noout)
summary(model)

##run the post hoc simple slopes
simple = sim.slopes(model, meanCenter(noout$attend))
simple

mean(noout$attend)
sd(noout$attend)

# use mouse click to place legend in graph.mod
graph.mod(simple,books,grade,master,
          xlab = "Centered Books Read",
          ylab = "Grade in Class")
