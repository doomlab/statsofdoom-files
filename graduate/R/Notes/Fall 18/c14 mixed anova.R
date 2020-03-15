#install.packages(c("reshape", "ggplot2", "Hmisc", "devtools", "ez", "pwr", "MBESS"))
#devtools::install_github("doomlab/MOTE")

##set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 16")

####import the file####
mixed = read.csv("c14 mixed.csv", header = T)

##data screening would go here in wide format
##remember that additivity is essentially r < 1

####add participant number####
##you have to add this before melting or your RM will not be correctly marked
mixed$partno = 1:nrow(mixed)

####move to long format####
library(reshape)
mixedlong = melt(mixed,
              id = c("partno", "Gender"), 
              measured = c("high_char","med_char","none_char"))

####useful column names####
colnames(mixedlong) = c("partno", "Gender", "Charisma", "Rating")

##rename those levels and reorder
mixedlong$Charisma = factor(mixedlong$Charisma, 
                            levels = c("none_char", "med_char", "high_char"),
                            labels = c("None", "Medium", "High"))

####Levene's ####
##only BN variables
options(scipen = 999)
library(ez)
ezANOVA(data = mixedlong,
        dv = Rating,
        wid = partno,
        between = Gender,
        detailed = TRUE,
        type = 3)

####EZ ANOVA####
##for Mauchly's and ANOVA
ezANOVA(data = mixedlong,
        dv = Rating,
        wid = partno,
        within = Charisma,
        between = Gender,
        detailed = TRUE,
        type = 3)


####graph####
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

bargraph = ggplot(mixedlong, aes(Charisma, Rating, fill = Gender))
bargraph +
  cleanup +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  xlab("Charisma of Dater") +
  ylab("Average Rating Score of Dater") 

##simple effects
##split on the larger variable
none = subset(mixedlong, Charisma == "None")
med = subset(mixedlong, Charisma == "Medium")
high = subset(mixedlong, Charisma == "High")

##pairwise option
with(none, pairwise.t.test(Rating, 
                           Gender, 
                           paired = FALSE, 
                           var.equal = T, 
                           p.adjust.method = "bonferroni"))
with(med, pairwise.t.test(Rating, 
                           Gender,
                           var.equal = T,  
                           paired = FALSE, 
                           p.adjust.method = "bonferroni"))
with(high, pairwise.t.test(Rating, 
                           Gender,
                           var.equal = T,  
                           paired = FALSE, 
                           p.adjust.method = "bonferroni"))

####effect size####
library(MOTE)
M = with(mixedlong, tapply(Rating, list(Charisma, Gender), mean))
stdev = with(mixedlong, tapply(Rating, list(Charisma, Gender), sd))

##bn N values, run if you used independent t
N = with(mixedlong, tapply(Rating, list(Charisma, Gender), length))
##rm N values, run if you used dependent t
##N = length(mixed$partno)

M;stdev;N

##no charisma, m to w
effect1 = d.ind.t(M[1,1], M[1,2],
                  stdev[1,1], stdev[1,2],
                  N[1,1], N[1,2], a = .05)

effect1$d

##med charisma, m to w
effect2 = d.ind.t(M[2,1], M[2,2],
                  stdev[2,1], stdev[2,2],
                  N[2,1], N[2,2], a = .05)

effect2$d

##high charisma, m to w
effect3 = d.ind.t(M[3,1], M[3,2],
                  stdev[3,1], stdev[3,2],
                  N[3,1], N[3,2], a = .05)

effect3$d

####Power####
library(pwr)
##you would do power on the interaction effect usually, notice how this is different than one-way
##power runs on cohen's f - not to be confused with anova f. 
##take the eta squared to convert
eta = 0.68482482234
feta = eta / (1-eta)

#u is df model
#v is df error, but we are trying to figure out sample size for each condition,
##so we leave this one blank. 
#f2 is cohen f squared 

pwr.f2.test(u = 2, v = NULL, f2 = feta, sig.level = .05, power = .80)
