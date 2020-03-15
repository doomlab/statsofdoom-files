##how I imported the file
chapter12 <- read.csv("~/e_files/TEACHING/200 Statistics/nolan 3rd R/powerpoints/chapter12.csv")

####demo for source table only####
##ss total
mean(unlist(chapter12))
sum((chapter12 - 4.7)^2)

##ss within
summary(chapter12)
sum((chapter12$cheese - 6.3)^2)
sum((chapter12$candy - 2.7)^2)
sum((chapter12$fruit - 5.10)^2)
8.1+8.1+12.9 

##ss between
(6.3 - 4.7)^2 *10 + 
  (2.7 - 4.7)^2 *10 + 
  (5.1 - 4.7)^2 *10

####end demo you would normally start here####
##step 2 calculate the means 
summary(chapter12)

sd(chapter12$cheese)
sd(chapter12$candy)
sd(chapter12$fruit)

##reshape the data
library(reshape)
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
