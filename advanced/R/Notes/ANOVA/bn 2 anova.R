##set working directory yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/5 ANOVA")

##import the file
##if you use the import dataset option, at least copy the code it spits out here
master = read.csv("bn 2 anova.csv")

##accuracy
summary(master)

##factoring year
master$year = factor(master$year,
                     levels = c("2007", "2008"))
##notice I didn't use labels ... I didn't want to change the levels, 
##I just wanted to switch to a factor, so labels not needed.

##outliers
zscore = scale(master$money)
summary(abs(zscore) < 3) ##see how many outliers
noout = subset(master, abs(zscore) < 3) ##exclude outliers

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
##add a participant number
noout$partno = 1:nrow(noout)

##run the ANOVA
library(ez)
ezANOVA(data = noout,
        wid = partno,
        between = .(year, type),
        dv = money,
        type = 3)

##year only
tapply(noout$money, list(noout$year), mean)
tapply(noout$money, list(noout$year), sd)
tapply(noout$money, list(noout$year), length)
##no follow up needed


##type of sport only
tapply(noout$money, list(noout$type), mean)
tapply(noout$money, list(noout$type), sd)
tapply(noout$money, list(noout$type), length)
##example follow up
pairwise.t.test(noout$money, noout$type,
                paired = F,
                var.equal = T,
                p.adjust.method = "bonferroni")

##get means, sd, length for the interaction
tapply(noout$money, list(noout$year, noout$type), mean)
tapply(noout$money, list(noout$year, noout$type), sd)
tapply(noout$money, list(noout$year, noout$type), length)


##split on sport
baseball = subset(noout, type == "Baseball")
basketball = subset(noout, type == "Basketball")
football = subset(noout, type == "Football")
soccer = subset(noout, type == "Soccer")
volleyball = subset(noout, type == "Volleyball")

##post hoc
pairwise.t.test(baseball$money, baseball$year, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(basketball$money, basketball$year, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(football$money, football$year, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(soccer$money, soccer$year, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
pairwise.t.test(volleyball$money, volleyball$year, 
                paired = F,
                var.equal = T, 
                p.adjust.method = "bonferroni")
##effect size
##new MOTE coding
##this will be different than the video, in that you can load the library,
##rather than run the script
library(MOTE)
d.ind.t(m1 = 234.76, sd1 = 151.09, n1 = 1617,
       m2 = 222.73, sd2 = 147.74, n2 = 1483,
       a = .05)

d.ind.t(m1 = 197.58, sd1 = 121.34, n1 = 1866, 
       m2 = 185.33, sd2 = 112.41, n2 = 1677, 
       a = .05)

d.ind.t(m1 = 360.77, sd1 = 222.28, n1 = 2190, 
       m2 = 338.72, sd2 = 219.94, n2 = 2075,
       a = .05)

d.ind.t(m1 = 320.98, sd1 = 180.08, n1 = 1676,
       m2 = 302.54, sd2 = 177.00, n2 = 1478,
       a = .05)

d.ind.t(m1 = 275.99, sd1 = 166.21, n1 = 2088,
       m2 = 264.03, sd2 = 161.89, n2 = 1887,
       a = .05)

##graph
library(ggplot2)

theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

bargraph = ggplot(noout, aes(type, money, fill = year))
bargraph +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  theme +
  xlab("Type of Sport") +
  ylab("Money per Transaction") + 
  scale_fill_manual(name="Year", values = c("Light Gray", "Dark Gray"))
