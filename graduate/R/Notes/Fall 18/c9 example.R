##make up some data
measure1 = c(rnorm(50, 10,2), rnorm(50, 15, 2))
measure2 = c(rnorm(50, 12, 2), rnorm(50, 20, 2))
exampledata = cbind(group = c(rep(1,50), rep(2,50)), measure1, measure2)
exampledata = as.data.frame(exampledata)
exampledata$group = factor(exampledata$group,
                           levels = c(1,2),
                           labels = c("YouTube", "Class"))

##data screening
##accuracy
summary(exampledata)

##missing data
##check for NAs
##example of dealing with missing data 
exampledata$measure1[exampledata$measure1 < 10] = NA 

##calculate percent missing
percentmiss = function(x) {
  sum(is.na(x)) / length(x) * 100
}

##calculate by column
apply(exampledata, 2, percentmiss)

##calculate by row
missing = apply(exampledata, 1, percentmiss)
table(missing)

##exclude too much missing
replacerow = exampledata[missing =< 5 , ] ##excludes too much missing
dontrow = exampledata[missing > 5, ] ##keep those participants 
summary(replacerow)

library(mice)
tempnomiss = mice(replacerow)

##end example missing data

##outliers
##if you only have one column of data (i.e. between subjects, with one variable)
zmeasure1 = scale(exampledata$measure1)
noout = exampledata[abs(zmeasure1) <= 3.00 , ]

##if you have repeated measures or multiple hypotheses
mahal = mahalanobis(exampledata[ , -1],
                    colMeans(exampledata[ , -1], na.rm = TRUE), 
                    cov(exampledata[ , -1], use = "pairwise.complete.obs"))
cutoff = qchisq(.999,ncol(exampledata [ , -1]))
summary(mahal < cutoff)
noout = exampledata[ mahal < cutoff , ]

##additivity does not apply
##cor(exampledata[, -1])

##assumptions
##set up
random = rchisq(nrow(noout), 7)
fake = lm(random ~ ., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

#normality
hist(standardized)
hist(noout$measure1)
hist(noout$measure2)

library(moments)
apply(noout[ , -1], 2, skewness)
apply(noout[ , -1], 2, kurtosis)
tapply(noout$measure1, noout$group, skewness)
tapply(noout$measure1, noout$group, kurtosis)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity and homoscedasticity
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

##independent t
options(scipen = 999)
t.test(measure1 ~ group,
       data = noout, 
       var.equal = TRUE,
       paired = FALSE)
M = tapply(noout$measure1, noout$group, mean)
sd = tapply(noout$measure1, noout$group, sd)
n = tapply(noout$measure1, noout$group, length)
se = sd / sqrt(n)

t.test(measure2 ~ group,
       data = noout,
       var.equal = TRUE,
       paired = FALSE)
M2 = tapply(noout$measure2, noout$group, mean)
sd2 = tapply(noout$measure2, noout$group, sd)
n2 = tapply(noout$measure2, noout$group, length)
se2 = sd2 / sqrt(n2)

##chart
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(color = "black"))

library(ggplot2)
barchart = ggplot(noout, aes(group, measure1))
barchart +
  stat_summary(fun.y = mean,
               geom = "bar",
               fill = "white",
               color = "black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar",
               width = .2,
               position = "dodge") +
  xlab("Learning Group") +
  ylab("Average Confidence Score") +
  scale_x_discrete(labels = c("Watched YouTube", "Came to Class")) +
  scale_y_continuous(limits = c(0,20)) +
  cleanup

##dependent t
library(reshape)
longnoout = melt(noout,
                 id = "group",
                 measured = c("measure1", "measure2"))
colnames(longnoout) = c("group", "time", "confidence")

t.test(confidence ~ time,
       data = longnoout,
       var.equal = TRUE, 
       paired = TRUE)
tapply(longnoout$confidence, longnoout$time, mean)
tapply(longnoout$confidence, longnoout$time, sd)
tapply(longnoout$confidence, longnoout$time, length)

##mean difference score and sd / se differences
differences = noout$measure1 - noout$measure2
mean(differences)
sd(differences)

##graph
barchart2 = ggplot(longnoout, aes(time, confidence))
barchart2 +
  stat_summary(fun.y = mean,
               geom = "bar",
               fill = "white",
               color = "black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar",
               width = .2,
               position = "dodge") +
  ylab("Confidence Ratings") +
  xlab("Time of Rating") +
  scale_x_discrete(labels = c("Time 1", "Time 2")) +
  scale_y_continuous(limits = c(0,20)) +
  cleanup
  