##set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/2 Basics")
####opening files####
#basics = read.csv("basics review.csv")
correldata = read.csv("correlation.csv")
dept = read.csv("dependent t.csv")
indt = read.csv("independent t.csv")
singlet = read.csv("single sample t.csv")

####changes the names of columns####
colnames(correldata)[1] = "feminine"
colnames(basics)[1] = "partno"

####descriptives####
data(airquality)
head(airquality)

table(airquality$Temp)
table(airquality$Month)

hist(airquality$Temp, breaks = 10)

summary(airquality)
mean(airquality$Temp, na.rm = T)
var(airquality$Temp, na.rm = T)
sd(airquality$Temp, na.rm = T)

sd(airquality$Temp, na.rm = T) / sqrt(length(na.omit(airquality$Temp)))

library(moments)
skewness(airquality$Temp, na.rm = T)
kurtosis(airquality$Temp, na.rm = T)

####single t####
##single sample t requires 1) column 2) mu = ?
t.test(singlet$SATscore, mu = 1250)
sd(singlet$SATscore, na.rm = T)
length(na.omit(singlet$SATscore))
d.singlet(m = 1370, u = 1250, sd = 112.6784, n = 15, a = .05, k = 2)

####dep t####
library(reshape)
dept$partno = 1:nrow(dept)
deptlong = melt(dept,
                id = "partno",
                measured = c("before", "after"))

colnames(deptlong) = c("partno", "time", "score")
t.test(deptlong$score ~ deptlong$time,
       paired = T, ##because dept t test is paired
       var.equal = T)
summary(dept)
M = tapply(deptlong$score, deptlong$time, mean, na.rm = T)
stdev = tapply(deptlong$score, deptlong$time, sd, na.rm = T)
N = tapply(deptlong$score, deptlong$time, length)
M;stdev;N
d.deptavg(m1 = 5.57, sd1 = 1.99, n = 7,
          m2 = 4.43, sd2 = 2.88, 
          a = .05, k = 2)

####Independent t####
indt$group = factor(indt$group,
                    levels = c(1,2),
                    labels = c("Experimental", "Control"))
t.test(indt$correctq ~ indt$group,
       paired = F, ##because indt t is two groups
       var.equal = T)
M = tapply(indt$correctq, list(indt$group), mean, na.rm = T) ##list allows you to add more IVs
stdev = tapply(indt$correctq, list(indt$group), sd, na.rm = T)
N  = tapply(indt$correctq, list(indt$group), length)
M;stdev;N

d.indt(m1 = 17.75, m2 = 23.00, 
       sd1 = 3.30, sd2 = 2.16,
       n1 = 4, n2 = 4,
       a = .05, k = 2)

##correlation
library(Hmisc)
rcorr(correldata) ##bad
rcorr(as.matrix(correldata)) ##good do it this way
options(scipen = 999)
