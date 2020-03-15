##set working directory yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/5 ANOVA")

##import the file
##if you use the import dataset option, at least copy the code it spits out here
master = read.csv("rm 2 anova.csv")

##here I am dropping all the other columns I'm not using
##notice we already have a participant number
master = master[ , c(1:2, 7:8)]

##accuracy
summary(master)

##fix those group labels
master$group = factor(master$group,
                      levels = c("debias", "jam"),
                      labels = c("Debiasing Instructions", "JAM Instructions"))

##outliers
##I am dropping the subject number and group column here
mahal = mahalanobis(master[ , -c(1:2)],
                    colMeans(master[ , -c(1:2)], na.rm = T),
                    cov(master[ , -c(1:2)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(master[ , -c(1:2)]))
cutoff ##cut off score
ncol(master[ , -c(1:2)]) ##df
summary(mahal < cutoff) ##see how many outliers
noout = subset(master, mahal < cutoff) ##exclude outliers

##additivity
correl = cor(noout[ , -c(1:2)], use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
##you can include the between subjects here, but not subject number
random = rchisq(nrow(noout[ , -2]), 7)
fake = lm(random~., data = noout[ , -2])
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
##look at all the columns
names(noout)
##be sure to include the group variable in your id field, you do not want to melt that data.
longdata = melt(noout,
                id = c("subject", "group"),
                measured = c("bsglo", "bsghi"))
colnames(longdata) = c("subject", "group", "bsg", "rating")

##run the ANOVA
library(ez)
ezANOVA(data = longdata,
        wid = subject,
        within = bsg,
        between = group,
        dv = rating,
        type = 3)

##just to get levene's
ezANOVA(data = longdata,
        wid = subject,
        between = group,
        dv = rating,
        type = 3)


##group only
tapply(longdata$rating, list(longdata$group), mean)
tapply(longdata$rating, list(longdata$group), sd)
tapply(longdata$rating, list(longdata$group), length)

##bsg only
tapply(longdata$rating, list(longdata$bsg), mean)
tapply(longdata$rating, list(longdata$bsg), sd)
tapply(longdata$rating, list(longdata$bsg), length)

##interaction 
tapply(longdata$rating, list(longdata$bsg, longdata$group), mean)
tapply(longdata$rating, list(longdata$bsg, longdata$group), sd)
tapply(longdata$rating, list(longdata$bsg, longdata$group), length)


##split on instructions
debias = subset(longdata, group == "Debiasing Instructions")
jam = subset(longdata, group == "JAM Instructions")

##post hoc
pairwise.t.test(debias$rating, debias$bsg, 
                paired = T,
                p.adjust.method = "bonferroni")
pairwise.t.test(jam$rating, jam$bsg, 
                paired = T,
                p.adjust.method = "bonferroni")

##effect size
##new MOTE coding
##this will be different than the video, in that you can load the library,
##rather than run the script
library(MOTE)
d.dep.t.avg(m1 = 60.31, m2 = 72.98, 
          sd1 = 12.34, sd2 = 12.16, 
          n = 73, a = .05)

d.dep.t.avg(m1 = 68.28, m2 = 82.72, 
          sd1 = 10.84, sd2 = 8.29, 
          n = 84, a = .05)

##graph
library(ggplot2)

theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

bargraph = ggplot(longdata, aes(group, rating, fill = bsg))
bargraph +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  theme +
  xlab("Instruction Group") +
  ylab("Rating of Word Pair") + 
  scale_fill_manual(name="Backward Strength",
                    labels = c("Low", "High"),
                    values = c("Light Gray", "Gray")) +
  coord_cartesian(ylim = c(0,100))
