####chapter 2####

##frequency table
table(airquality$Temp)

##length function
length(airquality$Temp)
length(airquality)

##percent frequency tables
table(airquality$Temp) / length(airquality$Temp) * 100

##grouped frequency stem and leaf
stem(airquality$Temp, scale = .2)

##load the package / library
##remember you must install them first
library(ggplot2)

##basic histogram
myplot = ggplot(airquality, aes(Temp))
myplot + geom_histogram(binwidth = 5)

##frequency polygon
myplot + geom_freqpoly(binwidth = 5)

####chapter 3####
##factor the variable 
table(airquality$Month)
airquality$Month = factor(airquality$Month,
                          levels = c(5,6,7,8,9),
                          labels = c("May", "June", "July", "August", "September"))

##theme coding
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))
##scatterplots
myplot = ggplot(airquality, aes(Temp, Ozone))
myplot + 
  geom_point() + 
  cleanup +
  xlab("STUFF AND THINGS") +
  ylab("SUPER HOT STUFF")

##scatterplots with a line of best fit
myplot = ggplot(airquality, aes(Temp, Ozone))
myplot + 
  geom_point() + 
  cleanup +
  xlab("STUFF AND THINGS") +
  ylab("SUPER HOT STUFF") +
  geom_smooth(method = "lm")

##bar graph
myplot = ggplot(airquality, aes(Month, Temp))
myplot +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  cleanup +
  ylab("Temperature in F")

####chapter 4####
mycolumn = c(8,11,19,10,22,13,16,12,9,15,8,
             14,12,14,15,15,16,8,15,11,15,14,19)
##get the mean
summary(mycolumn)
mean(mycolumn, na.rm = T)

##median
median(mycolumn, na.rm = T)

##mode
temp <- table(as.vector(mycolumn))
names(temp)[temp == max(temp)]

##outlier example
outlier = c(mycolumn, 200)
summary(outlier)
temp <- table(as.vector(outlier))
names(temp)[temp == max(temp)]

##range
max(mycolumn, na.rm = T) - min(mycolumn, na.rm = T)

##variance
var(mycolumn, na.rm = T)

##standard deviation
sd(mycolumn, na.rm = T)

##population formula
pop.var <- function(x) var(x) * (length(x)-1) / length(x) 
pop.sd <- function(x) sqrt(pop.var(x))

pop.var(mycolumn)
pop.sd(mycolumn)

##IQR
summary(mycolumn)
IQR(mycolumn, na.rm = T)

####chapter 8####
##power example
##enter numbers here
popmean = 80
popsd = 30
N = 5
alpha = .05
samplemean = 100
lower = F

##auto calculate
popse = popsd / sqrt(N)
mneed = popmean + popse*qnorm(alpha, lower.tail = lower)
z = (mneed - samplemean)/popse
mneed #mneed
z #Z difference
pnorm(z, lower.tail = lower) * 100 ##power

####chapter 9####
data2 = c(2, 3, 5, 5, 2)
summary(data2)
sd(data2)
sd(data2) / sqrt(length(data2))

qt(.01, 4, lower.tail = F)

t.test(data2,
       mu = 2.20,
       alternative = "greater",
       conf.level = .99)
##be sure to run effsize

##single sample t from means
d.singlet(m = 3.4, u = 2.2, sd = 1.52, n = 5, a = .01, k = 2)

####chapter 10####
difference = Chapter10_data$new - Chapter10_data$old

mean(difference)
sd(difference)
sd(difference) / sqrt(length(difference))

qt(.01, 6, lower.tail = F)

t.test(Chapter10_data$new,
       Chapter10_data$old,
       paired = T,
       alternative = "greater",
       conf.level = .99)

##dependent t differences
d.deptdiff(mdiff = 0.8571429, sddiff = 1.069045, 
           n = 7, a = .01, k = 2)

####chapter 11####
summary(dataset)
m1 = 160
m2 = 148

sd1 = sd(dataset$rb, na.rm = T)
sd2 = sd(dataset$pop, na.rm = T)
sd1
sd2

n1 = length(dataset$rb)
n2 = length(dataset$pop)
n1
n2

spooled = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2 - 2))
spooled

sdifference = sqrt((spooled^2/n1 + spooled^2/n2))
sdifference

qt(.05/2, 18, lower.tail = F)

t.test(dataset$rb,
       dataset$pop,
       paired = F,
       var.equal = T, 
       alternative = "two.sided", 
       conf.level = .95)

d.indt(m1 = m1, m2 = m2, 
       sd1 = sd1, sd2 = sd2, 
       n1 = n1, n2 = n2, 
       a = .05, k = 2)

####chapter 12####
##step 2 calculate the means 
summary(chapter12)

sd(chapter12$cheese)
sd(chapter12$candy)
sd(chapter12$fruit)

##reshape the data
library(reshape)
chatper12 = as.data.frame(chapter12)
longdata = melt(chapter12,
                measured = c("cheese", "candy", "fruit"))

##add a participant number
longdata$partno = 1:nrow(longdata)

##the actual anova
library(ez)
ezANOVA(data = longdata,        
        dv = value,        
        between = variable,       
        wid = partno,         
        type = 3,        
        return_aov = T)

##step 4 calculate f critical
qf(.05, 2, 27, lower.tail = F)

##post hoc tests
pairwise.t.test(longdata$value,
                longdata$variable,
                paired = F, 
                var.equal = T,
                p.adjust.method = "bonferroni")
options(scipen = 999)

####chapter 15####
##load the library
library(ggplot2)

##theme coding
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"))

myplot = ggplot(chapter15, aes(Bars, Churches))
myplot +
  theme +
  geom_point() +
  xlab("Number of Bars") +
  ylab("Number of Churches")

cor.test(chapter15$Bars, chapter15$Churches)

qt(.05/2, 5, lower.tail = F)

####chapter 16####
##load the library
library(ggplot2)

##theme coding
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"))

myplot = ggplot(newdata, aes(posemo, negemo))
myplot +
  theme +
  geom_point() +
  xlab("Positive Emotion") +
  ylab("Negative Emotion")

output = lm(negemo ~ posemo, data = newdata)
summary(output)

install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(output)

qt(.05/2, 35337, lower.tail = F)

##example multiple regression
output = lm(Clout ~ power + Authentic + affiliation + Tone, 
            data = multiple)
summary(output)

lm.beta(output)

qt(.05/2, 58426, lower.tail = F)

##load the library
library(ggplot2)

##theme coding
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"))

myplot = ggplot(multiple, aes(output$fitted.values, Clout))
myplot +
  theme +
  geom_point() +
  xlab("Predicted Score") +
  ylab("Clout Scores")
