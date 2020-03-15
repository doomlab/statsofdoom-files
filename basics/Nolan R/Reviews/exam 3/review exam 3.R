##single sample t 
summary(review.exam.3.1)
sd(review.exam.3.1$agression)
sd(review.exam.3.1$agression) / sqrt(length(review.exam.3.1$agression))
length(review.exam.3.1$agression)

qt(.05, 6, lower.tail = F)
t.test(review.exam.3.1$agression,
       mu = 6.647,
       conf.level = .95, 
       alternative = "greater")

d.singlet(m = 9.571, u = 6.647,
          sd = 3.87, n = 7,
          a = .05, k = 2)

##dependent t test
difference = review.exam.3.2$aftersales - review.exam.3.2$beforesales
summary(difference)
sd(difference)
sd(difference) / sqrt(length(difference))
length(difference)

qt(.05/2, 3, lower.tail = F)

t.test(review.exam.3.2$aftersales,
       review.exam.3.2$beforesales,
       paired = T, 
       conf.level = .95,
       alternative = "two.sided")

d.deptdiff(mdiff = -.20, sddiff = 1.14, n = 4,
           a = .05, k = 2)

##Indepedent t
summary(review.exam.3.3)
m1 = 16201
m2 = 15989
sd1 = sd(review.exam.3.3$Women)
sd1
sd2 = sd(review.exam.3.3$Men)
sd2
n1 = length(review.exam.3.3$Women)
n1
n2 = length(review.exam.3.3$Men)
n2

spooled = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2 - 2))
spooled
sdifference = sqrt((spooled^2/n1 + spooled^2/n2))
sdifference

qt(.01, 8, lower.tail = F)

t.test(review.exam.3.3$Women,
       review.exam.3.3$Men,
       paired = F,
       var.equal = T, 
       conf.level = .99,
       alternative = "greater")

t.test(review.exam.3.3$Women,
       review.exam.3.3$Men,
       paired = F,
       var.equal = T, 
       conf.level = .99,
       alternative = "two.sided")

d.indt(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2,
       n1 = n1, n2 = n2, a = .01, k = 2)


##anova
summary(review.exam.3.4)
sd(review.exam.3.4$Herbal.Tea)
sd(review.exam.3.4$Homeopathy)
sd(review.exam.3.4$Antihistamine)
sd(review.exam.3.4$Placebo)

library(reshape)
longdata = melt(review.exam.3.4,
                measured = c("Herbal.Tea", "Homeopathy",
                             "Antihistamine", "Placebo"))
longdata$partno = 1:nrow(longdata)
longdata = na.omit(longdata)

library(ez)
ezANOVA(data = longdata,
        dv = value,
        between = variable,
        wid = partno,
        type = 3,
        return_aov = T)

qf(.05, 3, 16, lower.tail = F)

pairwise.t.test(longdata$value,
                longdata$variable,
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
