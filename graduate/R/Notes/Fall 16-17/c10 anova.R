##all the packages you need
#install.packages(c("haven", "devtools", "ez", "ggplot2", "MBESS", "Hmisc", "pwr"))
#devtools::install_github("doomlab/MOTE")

library(haven)
library(ez)
library(MOTE)
library(ggplot2)
library(pwr)

##load up the data - go practice importing SPSS 
library(haven)
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 15")
master = read_spss("c10 viagra.sav")

##running a one way anova
##use the ez package to also get levene's test 
library(ez)

##you must have a participant number for ezANOVA
master$partno = 1:nrow(master)
master$dose = as.factor(master$dose)
options(scipen = 999)
ezANOVA(data = master,
        dv = libido,
        between = dose,
        wid = partno,
        type = 3, 
        detailed = T)

##running a one way anova - if Levene's Test is significant
oneway.test(libido ~ dose, data = master)

##get omega
##formula = (SSM – dfM*MSR)/ (SStotal + MSR)
##plug and chug the numbers, you need to change this formula
##MSR = SSd / DFd or (SSR / DFR)
23.60 / 12
w2 = (20.13 - 2*1.967) / (20.13 + 23.60 + 1.967)
w2

##post hoc tests
pairwise.t.test(master$libido,
                master$dose,
                p.adjust.method = "bonferroni", 
                paired = F, 
                var.equal = T)

##get numbers for effect size
M = tapply(master$libido, master$dose, mean)
N = tapply(master$libido, master$dose, length)
stdev = tapply(master$libido, master$dose, sd)
M;stdev;N

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

library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

bargraph = ggplot(master, aes(dose, libido))
bargraph +
  cleanup +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2, 
               position = "dodge") +
  xlab("Dosage of Drug") +
  ylab("Average Libido")


##trend analysis
k = 3 ##set to the number of groups
contrasts(master$dose) = contr.poly(k) ##note this does change the original dataset
output = aov(libido ~ dose, data = master)
summary.lm(output)
##NOTE: be sure that you do post hoc tests first, since we've changed the data here. 

##trend analysis graph
####single line graph####
##AES is X axis (categorical), Y axis (continuous)
doseline = ggplot(master, aes(dose, libido))
doseline +
  stat_summary(fun.y = mean, ##adds the points
               geom = "point") +
  stat_summary(fun.y = mean, ##adds the line
               geom = "line",
               aes(group=1)) +
  stat_summary(fun.data = mean_cl_normal, ##adds the error bars
               geom = "errorbar", 
               width = .2) +
  xlab("Dose of Viagra") +
  ylab("Average Libido") + 
  cleanup

####power####
library(pwr)

##power runs on cohen's f - not to be confused with anova f. 
##take the eta squared to convert
eta = .46
feta = sqrt(eta / (1-eta))

pwr.anova.test(k = 3, n = NULL, f = feta,
               sig.level = .05, power = .80)
