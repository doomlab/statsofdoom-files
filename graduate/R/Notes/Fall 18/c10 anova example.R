#between subjects one-way anova

##set working directory
setwd("~/Downloads")

##import the data
library(haven)
master = read_spss("c10 anova example.sav")
master$cokes = factor(master$cokes,
                      levels = c(1,2,3),
                      labels = c("none", "one to two", "many"))

##accuracy
summary(master)

##missing
percentmiss = function(x){sum(is.na(x)) / length(x) * 100}
##columns
apply(master, 2, percentmiss)
##rows
missing = apply(master, 1, percentmiss)
table(missing)

##exclude missing data
nomiss = subset(master, missing <= 5)

##outliers
hist(nomiss$anxiety)
zscored = scale(nomiss$anxiety)
noout = subset(nomiss, abs(zscored) <= 3)

##assumptions
##additivity - do not check this one

##set up for assumptions
random = rchisq(nrow(noout), 7)
fake = lm(random ~ ., data = noout) ##screen all the variables
fitted = scale(fake$fitted.values)
standardized = rstudent(fake)

##normality
hist(noout$anxiety)
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homog+s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

library(ez)
#add participant number
noout$partno = 1:nrow(noout)

options(scipen = 999)

#gives you levene's and the anova
ezANOVA(data = noout,
        dv = anxiety,
        between = cokes,
        wid = partno,
        type = 3, 
        detailed = T)

##correct for homogeneity problems if levene's is significant only 
oneway.test(anxiety ~ cokes, data = noout)

#get ges from the ez output
#0.04443828

#get omega from MOTE
omega.F(dfm = 2,
        dfe = 165, 
        Fvalue = 3.836652,
        n = 168, #nrow(noout)
        a = .05)
#0.03266653


##follow up tests
##type 1 error - so why no t-tests only
##none versus 1-2
##none versus many
##1-2 versus many
#if you want to know how much type 1 error there is
1 - (1 - .05)^3

#post hoc tests here
pairwise.t.test(noout$anxiety,
                noout$cokes,
                p.adjust.method = "bonferroni", 
                paired = F, 
                var.equal = T)

##get numbers for effect size
##descriptives
M = tapply(noout$anxiety, noout$cokes, mean)
stdev = tapply(noout$anxiety, noout$cokes, sd)
N = tapply(noout$anxiety, noout$cokes, length)
se = sd / sqrt(N)

##placebo to low
effect1 = d.ind.t(m1 = M[1], m2 = M[2],
                  sd1 = stdev[1], sd2 = stdev[2],
                  n1 = N[1], n2 = N[2], a = .05)
effect1$d

##placebo to high
effect2 = d.ind.t(m1 = M[1], m2 = M[3],
                  sd1 = stdev[1], sd2 = stdev[3],
                  n1 = N[1], n2 = N[3], a = .05)
effect2$d

##low to high
effect3 = d.ind.t(m1 = M[2], m2 = M[3],
                  sd1 = stdev[2], sd2 = stdev[3],
                  n1 = N[2], n2 = N[3], a = .05)
effect3$d

##power here
library(pwr)

##power runs on cohen's f - not to be confused with anova f. 
##take the eta squared to convert
eta = 0.04443828
feta = sqrt(eta / (1-eta))

pwr.anova.test(k = 3, n = NULL, f = feta,
               sig.level = .05, power = .80)

##trend analysis
k = 3 ##set to the number of groups
noout$cokes2 = noout$cokes
contrasts(noout$cokes2) = contr.poly(k)
output2 = aov(anxiety ~ cokes2, data = noout)
summary.lm(output2)

library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

bargraph = ggplot(noout, aes(cokes, anxiety))
bargraph +
  cleanup + 
  stat_summary(fun.y = mean,
               geom = "bar",
               color = "black", 
               fill = "white") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = "dodge",
               width = .2) +
  xlab("Number of Cokes Drank During the Day") +
  ylab("Average Anxiety Score") +
  scale_x_discrete(labels = c("None", "One to Two", "Three or More")) +
  scale_y_continuous(limit = c(0,20))

