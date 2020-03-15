####make up some data####
measure1 = c(rnorm(50, 10, 2), rnorm(50, 15, 2))
measure2 = c(rnorm(50, 12, 2), rnorm(50, 20, 2))
exampledata = cbind(group = c(rep(1,50), rep(2,50)), measure1, measure2)
exampledata = as.data.frame(exampledata)
exampledata$group = factor(exampledata$group,
                           levels = c(1,2),
                           labels = c("YouTube", "Class"))

####data screening####
##accuracy
summary(exampledata)

##missing data
##check for NAs
##example of dealing with missing data 
exampledata$measure1[exampledata$measure1 < 10] = NA 

##calculate percent missing
percentmiss = function(x) { sum(is.na(x)) / length(x) * 100 }

##calculate by row
missing = apply(exampledata, 1, percentmiss)
table(missing)

##exclude too much missing
replacepeople = subset(exampledata, missing <= 5) 
dontpeople = subset(exampledata, missing > 5)

##calculate by column
apply(replacepeople, 2, percentmiss)

nomissing = replacepeople
##end example missing data

##outliers
##if you only have one column of data (i.e. between subjects, with one variable)
zmeasure1 = scale(nomissing$measure1)
summary(zmeasure1)
noout = subset(nomissing, abs(zmeasure1) <= 3.00)

##if you have repeated measures or multiple hypotheses
mahal = mahalanobis(nomissing[ , -1],
                    colMeans(nomissing[ , -1], na.rm = TRUE), 
                    cov(nomissing[ , -1], use = "pairwise.complete.obs"))
cutoff = qchisq(1 - .001, ncol(nomissing[ , -1]))
cutoff
ncol(nomissing[ , -1])

summary(mahal < cutoff)
noout = subset(nomissing, mahal < cutoff)

##additivity does not apply

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

####independent t####
options(scipen = 999)
t.test(measure1 ~ group,
       data = noout, 
       var.equal = TRUE,
       paired = FALSE)

M = tapply(noout$measure1, noout$group, mean)
stdev = tapply(noout$measure1, noout$group, sd)
n = tapply(noout$measure1, noout$group, length)
se = stdev / sqrt(n)

M;stdev;n;se

t.test(measure2 ~ group,
       data = noout,
       var.equal = TRUE,
       paired = FALSE)

M2 = tapply(noout$measure2, noout$group, mean)
stdev2 = tapply(noout$measure2, noout$group, sd)
n2 = tapply(noout$measure2, noout$group, length)
se2 = stdev2 / sqrt(n2)

M2;stdev2;n2;se2

##effect size
d.indt(m1 = 11.47, sd1 = 1.37, n1 = 21,
       m2 = 14.89, sd2 = 1.86, n2 = 50,
       a = .05, k = 2)

d.indt(m1 = 11.93, sd1 = 2.10, n1 = 33,
       m2 = 19.92, sd2 = 2.05, n2 = 50,
       a = .05, k = 2)

##chart
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

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
  coord_cartesian(ylim = c(0,20)) +
  cleanup

####dependent t####
library(reshape)
longnoout = melt(noout,
                 id = "group",
                 measured = c("measure1", "measure2"))
colnames(longnoout) = c("group", "time", "confidence")

t.test(confidence ~ time,
       data = longnoout,
       var.equal = TRUE, 
       paired = TRUE)

M3 = tapply(longnoout$confidence, longnoout$time, mean)
stdev3 = tapply(longnoout$confidence, longnoout$time, sd)
n3 = tapply(longnoout$confidence, longnoout$time, length)
se3 = stdev3 / sqrt(n3)

M3; stdev3;n3;se3

##effect size for d averages
d.deptavg(m1 = 13.71, sd1 = 2.32, 
          m2 = 16.75, sd2 = 4.44,
          n = 83, a = .05, k = 2)

##mean difference score and sd / se differences
differences = noout$measure1 - noout$measure2
mean(differences)
sd(differences)
sd(differences) / sqrt(length(differences))

d.deptdiff(mdiff = -3.04, sddiff = 3.15, 
           n = 83, a = .05, k = 2)

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
  coord_cartesian(ylim = c(0,20)) +
  cleanup
  