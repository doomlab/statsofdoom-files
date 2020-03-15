##one way between subjects anova##
##set the working directory (this will be different on your computer)
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/5 ANOVA")

##import the file
master = read.csv("bn 1 anova.csv")

##you can also skip the above two steps and simply use the import dataset feature.
##BUT you should copy the code that it spits out on the bottom, so you can
##rerun your entire code and have it work. 

##accuracy
summary(master)

##missing data
summary(master)

##outliers
notypos = master
zscores = scale(notypos$friends)
summary(abs(zscores) < 3)
noout = subset(notypos, abs(zscores) < 3)

##assumption set up
random = rchisq(nrow(noout), 7)
fake = lm(random~., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##run the anova
library(ez)

##add a participant number
noout$partno = 1:nrow(noout)

##run anova
ezANOVA(data = noout,
        wid = partno,
        between = group,
        dv = friends,
        type = 3)

##post hocs
tapply(noout$friends, list(noout$group), mean)
tapply(noout$friends, list(noout$group), sd)

pairwise.t.test(noout$friends, noout$group, 
                paired = F,
                var.equal = T,
                p.adjust.method = "bonferroni")

##new MOTE coding
##this will be different than the video, in that you can load the library,
##rather than run the script, use this version
library(MOTE)
tapply(noout$friends, list(noout$group), length)
d.ind.t(m1 = 3.00, m2 = 5.33, sd1 = .82, sd2 = 2.52, n1 = 4, n2 = 3, a = .05)
d.ind.t(m1 = 3.00, m2 = 1.50, sd1 = .82, sd2 = 1.29, n1 = 4, n2 = 4, a = .05)
d.ind.t(m1 = 1.50, m2 = 5.33, sd1 = 1.29, sd2 = 2.52, n1 = 4, n2 = 3, a = .05)

##graph
library(ggplot2)

##theme coding
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

##build the graph
bargraph = ggplot(noout, aes(group, friends))

##add all the layers
bargraph +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  theme +
  xlab("Health Condition") +
  ylab("Number of Friends") +
  scale_x_discrete(labels = c("Excellent", "Fair", "Poor"))