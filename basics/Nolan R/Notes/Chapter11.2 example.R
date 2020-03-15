####example 1####
data = cbind(al = c(3.4, 5.4, 3.2, 1.1, 4.5, 4.4, NA),
             nl = c(1.5, 2.3, 2.6, 3.7, 4.1, 1.7, 2.8))
data = as.data.frame(data)

summary(data)
m1 = 3.667
m2 = 2.671
m1 = mean(data$al, na.rm = T)
m2 = mean(data$nl, na.rm = T)
m1
m2

sd1 = sd(data$al, na.rm = T)
sd2 = sd(data$nl, na.rm = T)
sd1
sd2

n1 = 6
n2 = length(data$nl)
n1
n2

spooled = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2 - 2))
spooled

sdifference = sqrt((spooled^2/n1 + spooled^2/n2))
sdifference

qt(.01, 11, lower.tail = F)

t.test(data$al,
       data$nl,
       paired = F,
       var.equal = T,
       conf.level = .99,
       alternative = "greater")

t.test(data$al,
       data$nl,
       paired = F,
       var.equal = T,
       conf.level = .99,
       alternative = "two.sided")

d.indt(m1 = m1, m2 = m2,
       sd1 = sd1, sd2 = sd2,
       n1 = n1, n2 = n2,
       a = .01, k = 2)

##cheat for t
(m1 - m2) / sdifference

##cheat for ci
(m1 - m2) + qt(.01/2, 11, lower.tail = F)*sdifference
(m1 - m2) - qt(.01/2, 11, lower.tail = F)*sdifference

##cheat for effect size
(m1 - m2) / spooled

####example 2####
dataexp2 = cbind(new = c(2,3,4,5,3,4,5,3,4),
             old = c(3,2,3,4,1,3,4,2,3))
dataexp2 = as.data.frame(dataexp2)

summary(dataexp2)
m1 = 2.778
m2 = 3.667

sd1 = sd(dataexp2$old, na.rm = T)
sd2 = sd(dataexp2$new, na.rm = T)
sd1
sd2

n1 = length(dataexp2$old)
n2 = length(dataexp2$new)
n1
n2

spooled = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2 - 2))
spooled

sdifference = sqrt((spooled^2/n1 + spooled^2/n2))
sdifference

qt(.05, 16, lower.tail = T)

t.test(dataexp2$old,
       dataexp2$new,
       paired = F,
       var.equal = T,
       conf.level = .95,
       alternative = "less")

t.test(dataexp2$old,
       dataexp2$new,
       paired = F,
       var.equal = T,
       conf.level = .95,
       alternative = "two.sided")

d.indt(m1 = m1, m2 = m2,
       sd1 = sd1, sd2 = sd2,
       n1 = n1, n2 = n2,
       a = .05, k = 2)
