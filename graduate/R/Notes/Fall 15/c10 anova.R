##load up the data - go practice importing SPSS 
library(memisc)
#setwd("~/e_files/TEACHING/745 Grad Statistics/notes/fall 15")
dataset = as.data.set(spss.system.file("c10 viagra.sav"))
dataset = as.data.frame(dataset)

##how to calculate Levene's test
library(car)
leveneTest(libido ~ dose, data = dataset, center = mean)

##running a one way anova - Levene's Test is not significant
output = aov(libido ~ dose, data = dataset)
summary(output)

##running a one way anova - Levene's Tes is significant
##note, do not save this function and use the summary function, just run directly
oneway.test(libido ~ dose, data = dataset)

##get the R2/eta2
summary.lm(output)

##get omega
##formula = (SSM – dfM*MSR)/ (SStotal + MSR)
##plug and chug the numbers, you need to change this formula
w2 = (20.13 - 2*1.967) / (20.13+ 23.60 + 1.967)
w2

##post hoc tests
install.packages("agricolae")
library(agricolae)
LSD.test(output, "dose", group = FALSE, console = TRUE) ##no correction
LSD.test(output, "dose", group = FALSE, console = TRUE, p.adj = "bon") ##bonferroni
SNK.test(output, "dose", group = FALSE, console = TRUE) ##snk
HSD.test(output, "dose", group = FALSE, console = TRUE) ##tukey
scheffe.test(output, "dose", group = FALSE, console = TRUE) ##scheffe
##NOTE: you would only do one test ... we are doing all of them for a demo only. 


##get numbers for effect size
M = tapply(dataset$libido, dataset$dose, mean)
N = tapply(dataset$libido, dataset$dose, length)
sd = tapply(dataset$libido, dataset$dose, sd)

##trend analysis
k = 3 ##set to the number of groups
contrasts(dataset$dose) = contr.poly(k) ##note this does change the original dataset
output = aov(libido ~ dose, data = dataset)
summary.lm(output)
##NOTE: be sure that you do post hoc tests first, since we've changed the data here. 

library(ggplot2)

theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(color = "black"))

bargraph = ggplot(dataset, aes(dose, libido))
bargraph +
  theme +
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
