data = c(26, 30, 28, 29, 25, 28, 32, 35, 24, 23)
summary(data)

sd(data, na.rm = T)

sd(data, na.rm = T) / sqrt(length(data))

3.71 / sqrt(10)

qt(.05, 9, lower.tail = T)
qt(.05, 99, lower.tail = T)
qnorm(.05, lower.tail = T)

(28-30)/ 1.17

t.test(data,
       mu = 30, 
       alternative = "less",
       conf.level = .95)

t.test(data,
       mu = 30, 
       alternative = "two.sided",
       conf.level = .95)

(28-30)/3.71


##second example
data2 = c(2, 3, 5, 5, 2)
summary(data2)
sd(data2)
sd(data2) / sqrt(length(data2))

qt(.01, 4,lower.tail = F)
t.test(data2,
       mu = 2.20,
       alternative = "greater",
       conf.level = .99)
##be sure to run effsize

##single sample t from means
d.singlet(m = 3.4, u = 2.2, sd = 1.52, n = 5, a = .01, k = 2)










