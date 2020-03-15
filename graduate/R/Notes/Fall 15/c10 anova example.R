#between subjects one-way anova

##set working directory
setwd("~/Downloads")

##import the data
library(memisc)
master = as.data.set(spss.system.file("c10 anova example.sav"))
master = as.data.frame(master)

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
fake = lm(random ~ anxiety, data = noout) ##screen only the dv
fitted = scale(fake$fitted.values)
standardized = rstudent(fake)

##normality
hist(nomiss$anxiety)
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homog+s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

##levene's test
library(car)
leveneTest(anxiety ~ cokes, data = noout, center = mean)

##run the anova
output = aov(anxiety ~ cokes, data = noout)
summary(output)
##correct for homogeneity problems
oneway.test(anxiety ~ cokes, data = noout)

##effect size R2 or eta2 
R2 = 65.7 / (65.7 + 1412.6)
R2
summary.lm(output)

##omega2
##formula = (SSM – dfM*MSR)/ (SStotal + MSR)
w2 = (65.7 - 2 *8.56) / (65.7 + 1412.6 + 8.56)
w2

##power here

##descriptives
M = tapply(noout$anxiety, noout$cokes, mean)
sd = tapply(noout$anxiety, noout$cokes, sd)
n = tapply(noout$anxiety, noout$cokes, length)
se = sd / sqrt(n)


##follow up tests
##type 1 error - so why no t-tests only
##none versus 1-2
##none versus many
##1-2 versus many
1 - (1 - .05)^3

##post hoc tests = t-test (independent)
##post hoc corrections = none/LSD, bonferroni, tukey/hsd, snk, scheffe
library(agricolae)
##only do one of these corrections
LSD.test(output, "cokes", group = FALSE, console = TRUE) ##no correction
LSD.test(output, "cokes", group = FALSE, console = TRUE, p.adj = "bon") ##bonferroni correction
SNK.test(output, "cokes", group = FALSE, console = TRUE) ##snk
HSD.test(output, "cokes", group = FALSE, console = TRUE) ##tukey
scheffe.test(output, "cokes", group = FALSE, console = TRUE) ##scheffe

##trend analysis
k = 3 ##set to the number of groups
noout$cokes2 = noout$cokes
contrasts(noout$cokes2) = contr.poly(k)
output2 = aov(anxiety ~ cokes2, data = noout)
summary.lm(output2)

library(ggplot2)
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(color = "black"))
bargraph = ggplot(noout, aes(cokes, anxiety))
bargraph +
  theme + 
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
