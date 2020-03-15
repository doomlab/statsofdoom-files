##set working directory yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/5 ANOVA")
##import the file
##if you use the import dataset option, at least copy the code it spits out here
master = read.csv("rm 1 anova.csv")

##accuracy
summary(master)

##outliers
mahal = mahalanobis(master,
                    colMeans(master, na.rm = T),
                    cov(master, use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(master))
cutoff ##cut off score
ncol(master) ##df
summary(mahal < cutoff) ##see how many outliers
noout = subset(master, mahal < cutoff) ##exclude outliers

##additivity
correl = cor(noout, use = "pairwise.complete.obs")
symnum(correl)
correl

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

##switch wide to long
library(reshape)
longdata = melt(noout,
                id = "partno",
                measured = c("neutral", "positive", "negative"))
colnames(longdata) = c("partno", "pictype", "heartrate")

##run the ANOVA
library(ez)
ezANOVA(data = longdata,
           wid = partno,
           within = pictype,
           dv = heartrate,
           type = 3)

##get means, sd, length
tapply(longdata$heartrate, list(longdata$pictype), mean)
tapply(longdata$heartrate, list(longdata$pictype), sd)
tapply(longdata$heartrate, list(longdata$pictype), length)

##post hoc
pairwise.t.test(longdata$heartrate, longdata$pictype, 
                paired = T,
                p.adjust.method = "bonferroni")

options(scipen = 999)

##effect size
library(MOTE)
d.dep.t.avg(m1 = 87.5, m2 = 134.11, 
          sd1 = 16.73, sd2 = 21.75, 
          n = 18, a = .05)

d.dep.t.avg(m1 = 87.5, m2 = 149.56, 
          sd1 = 16.73, sd2 = 27.75, 
          n = 18, a = .05)

d.dep.t.avg(m1 = 149.56, m2 = 134.11, 
          sd1 = 27.75, sd2 = 21.75, 
          n = 18, a = .05)

##graph
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

bargraph = ggplot(longdata, aes(pictype, heartrate))
bargraph +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  cleanup +
  xlab("Picture Viewed") +
  ylab("Heart Rate") +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative"))

