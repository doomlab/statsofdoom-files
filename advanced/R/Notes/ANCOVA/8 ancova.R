##dr b's working directory, yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/8 ANCOVA")

##import the file as master
master = read.csv("bn ancova.csv")

##only use columns we need
master = master[ , c(1:3, 6:7)]

##accuracy
summary(master)
##get rid of that blank religion group
notypos = master
table(notypos$religion) ##look at what's there
notypos$religion = factor(notypos$religion,
                          levels = c("CATHOLIC", "JEWISH",
                                     "NONE OR OTHER", "PROTESTANT"),
                          labels = c("Catholic", "Jewish",
                                     "None or Other", "Protestant"))
table(notypos$religion) ##check that it's right

##missing
summary(notypos)

##outliers
cutoff = qchisq(1-.001, ncol(notypos[ , 2:3]))
mahal = mahalanobis(notypos[ , 2:3],
                    colMeans(notypos[ , 2:3], na.rm = T),
                    cov(notypos[ , 2:3], use = "pairwise.complete.obs"))
cutoff ##cut off score
ncol(notypos[ , 2:3]) ##df
summary(mahal < cutoff) ##number of outliers
noout = subset(notypos, mahal < cutoff) ##exclude outliers

##additivity only necessary with 2 or more CVs

##assumption set up
random = rchisq(nrow(noout), 7)
fake = lm(random~., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##Linearity
qqnorm(standardized)
abline(0,1)

##homog + s
plot(fitted,standardized)
abline(0,0)
abline(v=0)

##Levene's test
library(car)
leveneTest(attdrug ~ emplmnt*religion,
           data = noout, center = mean)

##ancova
output = lm(attdrug ~ phyheal + emplmnt*religion,
          data = noout)
##get the output
summary.aov(output)

##etas
res = 584.1
10 / (res + 10)
3 / (res + 3)
7.6 / (res + 7.6)
10.8 / (res + 10.8)

##interpret the CV
cor(noout$attdrug, noout$phyheal)

##create the adjusted means for post hoc
library(effects)
effect("emplmnt", output)
effect("religion", output)
effect("emplmnt*religion", output)
##proof these are different (nonadjusted)
##just for teaching purposes, don't use these
tapply(noout$attdrug, 
       list(noout$emplmnt, noout$religion), mean)

##you can still use tapply for sd and N
tapply(noout$attdrug, 
       list(noout$emplmnt, noout$religion), sd)
tapply(noout$attdrug, 
       list(noout$emplmnt, noout$religion), length)

##interactions
##split up the data
catholic = subset(noout, religion == "Catholic")
jewish = subset(noout, religion == "Jewish")
protestant = subset(noout, religion == "Protestant")
none = subset(noout, religion == "None or Other")

##run a post hoc
library(multcomp)

##if you wanted to do main effects for levels > 2
post = glht(output, linfct = mcp(religion = "Tukey"))
summary(post)

##run little ancovas
##be sure you use your subset datasets
cath_out = lm(attdrug ~ phyheal + emplmnt, data = catholic)
jew_out = lm(attdrug ~ phyheal + emplmnt, data = jewish)
prot_out = lm(attdrug ~ phyheal + emplmnt, data = protestant)
none_out = lm(attdrug ~ phyheal + emplmnt, data = none)

cath_post = glht(cath_out, linfct = mcp(emplmnt = "Tukey"))
summary(cath_post)
jew_post = glht(jew_out, linfct = mcp(emplmnt = "Tukey"))
summary(jew_post)
prot_post = glht(prot_out, linfct = mcp(emplmnt = "Tukey"))
summary(prot_post)
none_post = glht(none_out, linfct = mcp(emplmnt = "Tukey"))
summary(none_post)

##effect size
##using d.ind.t because between subjects
##you can still use tapply for sd and N
library(MOTE)
tapply(noout$attdrug, 
       list(noout$emplmnt, noout$religion), sd)
tapply(noout$attdrug, 
       list(noout$emplmnt, noout$religion), length)

##interaction
d.ind.t(m1 = 8.04, sd1 = 1.09, n1 = 56,
       m2 = 7.71, sd2 =  .99, n2 = 62,
       a = .05)

d.ind.t(m1 = 7.78, sd1 = 1.21, n1 = 47,
       m2 = 7.61, sd2 = 1.11, n2 = 44,
       a = .05)

d.ind.t(m1 = 7.82, sd1 = 1.19, n1 = 83,
       m2 = 7.50, sd2 =  1.08, n2 = 92,
       a = .05)

d.ind.t(m1 = 7.12, sd1 = 1.18, n1 = 30,
       m2 = 7.67, sd2 =  1.35, n2 = 46,
       a = .05)

##graph the interaction
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

##if you have NAs...you'll have to ditch them first
graphdata = na.omit(noout)

bargraph = ggplot(graphdata, aes(religion, attdrug, fill = emplmnt))
bargraph +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  cleanup +
  xlab("Religion Group") +
  ylab("Attitudes Toward Drugs") + 
  scale_fill_manual(name="Employment", 
                    values = c("Light Gray", "Dark Gray"),
                    labels = c("Employed", "Unemployed"))
