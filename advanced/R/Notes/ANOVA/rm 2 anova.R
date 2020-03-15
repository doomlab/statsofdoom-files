##set working directory yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/5 ANOVA")

##import the file
##if you use the import dataset option, at least copy the code it spits out here
master = read.csv("rm 2 anova.csv")

##here I am dropping all the other columns I'm not using
##notice we already have a participant number
master = master[, 2:6]

##accuracy
summary(master)

##outliers
##I am dropping the subject number column here
mahal = mahalanobis(master[ , -1],
                    colMeans(master[ , -1], na.rm = T),
                    cov(master[ , -1], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(master[ , -1]))
cutoff ##cut off score
ncol(master[ , -1]) ##df
summary(mahal < cutoff) ##see how many outliers
noout = subset(master, mahal < cutoff) ##exclude outliers

##additivity
correl = cor(noout[ , -1], use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
random = rchisq(nrow(noout[ , -1]), 7)
fake = lm(random~., data = noout[ , -1])
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
##we don't need to add one for this example 
##noout$partno = 1:nrow(noout)

##switch wide to long
library(reshape)

names(noout)
longdata = melt(noout,
                id = "subject",
                measured = c("fsglobsglo", "fsghibsglo", "fsglobsghi", "fsghibsghi"))

colnames(longdata) = c("subject", "temp", "rating")

##fix the double IV issue
longdata$fsg = c(rep("Low", 157),
                 rep("High", 157),
                 rep("Low", 157),
                 rep("High", 157)
                 )
longdata$bsg = c(rep("Low", 157+157),
                 rep("High", 157+157)
                 )

table(longdata$fsg, longdata$bsg)

longdata$fsg = factor(longdata$fsg,
                      levels = c("Low", "High"), 
                      labels = c("Low", "High"))

longdata$bsg = factor(longdata$bsg,
                      levels = c("Low", "High"), 
                      labels = c("Low", "High"))

##run the ANOVA
library(ez)
ezANOVA(data = longdata,
        wid = subject,
        within = .(fsg, bsg),
        dv = rating,
        type = 3)

##get means, sd, length for the interaction
tapply(longdata$rating, list(longdata$fsg, longdata$bsg), mean)
tapply(longdata$rating, list(longdata$fsg, longdata$bsg), sd)
tapply(longdata$rating, list(longdata$fsg, longdata$bsg), length)

##fsg only
tapply(longdata$rating, list(longdata$fsg), mean)
tapply(longdata$rating, list(longdata$fsg), sd)
tapply(longdata$rating, list(longdata$fsg), length)

##bsg only
tapply(longdata$rating, list(longdata$bsg), mean)
tapply(longdata$rating, list(longdata$bsg), sd)
tapply(longdata$rating, list(longdata$bsg), length)

##split on fsg
fsglow = subset(longdata, fsg == "Low")
fsghigh = subset(longdata, fsg == "High")

##post hoc
pairwise.t.test(fsglow$rating, fsglow$bsg, 
                paired = T,
                p.adjust.method = "bonferroni")
pairwise.t.test(fsghigh$rating, fsghigh$bsg, 
                paired = T,
                p.adjust.method = "bonferroni")

##effect size
library(MOTE)
d.dep.t.avg(m1 = 50.23, m2 = 56.10, 
          sd1 = 12.70, sd2 = 12.57, 
          n = 157, a = .05)

d.dep.t.avg(m1 = 60.30, m2 = 77.97, 
          sd1 = 12.50, sd2 = 11.56, 
          n = 157, a = .05)

##graph
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

bargraph = ggplot(longdata, aes(fsg, rating, fill = bsg))
bargraph +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  cleanup +
  xlab("Forward Strength") +
  ylab("Rating of Word Pair") + 
  scale_fill_manual(name="Backward Strength", values = c("#ff0000", "Pink"))
