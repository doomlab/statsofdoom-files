##Get the mean
summary(chapter11.data)
m1 = 3.125
m2 = 6.50

##Get the sd
sd1 = sd(chapter11.data$grumps, na.rm = T)
sd2 = sd(chapter11.data$pew, na.rm = T)

##Get N
n1 = length(chapter11.data$grumps)
n2 = length(chapter11.data$pew)

spooled = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2 - 2))
spooled

sdifference = sqrt((spooled^2/n1 + spooled^2/n2))
sdifference

qt(.05/2,14, lower.tail = F)
qt(.05/2,14, lower.tail = T)

t.test(chapter11.data$grumps,
       chapter11.data$pew,
       paired = F, 
       var.equal = T,
       alternative = "two.sided",
       conf.level = .95)

##just in case you get the numbers from the book
(m1 - m2) / sdifference

(m1 - m2) + qt(.05/2,14, lower.tail = F) * sdifference
(m1 - m2) - qt(.05/2,14, lower.tail = F) * sdifference

(m1 - m2) / spooled

d.indt(m1 = m1, m2 = m2, 
       sd1 = sd1, sd2 = sd2, 
       n1 = n1, n2 = n2, 
       a = .05, k = 2)


####example 2####
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
