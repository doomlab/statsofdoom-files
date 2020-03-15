#install.packages(c("ez", "ggplot2", "Hmisc", "pwr", "devtools"))
#devtools::install_github("doomlab/MOTE")

##chapter 12 two-way ANOVAs
##set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 15")
##import the file
dataset = read.csv("c12 2 way.csv")
options(scipen = 999)

##check out the data to make sure it's ok
summary(dataset)

####change up sport####
dataset$Sport = factor(dataset$Sport,
                       levels = c("None", "Football", "Volleyball"))

####run that anova####
##first look at levene's
library(ez)
dataset$partno = 1:nrow(dataset)
ezANOVA(data = dataset,
        dv = Satisfaction,
        between = .(Gender, Sport),
        wid = partno,
        type = 3,
        detailed = T)

####omega formulas####
#(df model)*(Fmodel - 1) / (df model)*(Fmodel - 1)+a*n
table(dataset$Gender)
table(dataset$Sport)
tapply(dataset$Satisfaction, list(dataset$Gender, dataset$Sport), length)

ogender = (1*(2.03-1)) / (1*(2.03-1)+2*24)
osport = (2*(20.07 - 1)) / (2*(20.07 - 1)+(3*16))
ointeraction = (2*(11.91-1)) / (2*(11.91-1)+6*8)
ogender;osport;ointeraction

####graphs####
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

##gender
genderbar = ggplot(dataset, aes(Gender, Satisfaction))
genderbar +
  cleanup +
  stat_summary(fun.y = mean,
               geom = "bar",
               fill = "white",
               color = "black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar",
               position = "dodge",
               width = .2) +
  xlab("Gender of Participant") +
  ylab("Average Satisfaction Score")

##sport
sportbar = ggplot(dataset, aes(Sport, Satisfaction))
sportbar +
  cleanup +
  stat_summary(fun.y = mean,
               geom = "bar",
               fill = "white",
               color = "black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar",
               position = "dodge",
               width = .2) +
  xlab("Sport Rated") +
  ylab("Average Satisfaction Score")

##interaction
interactionbar = ggplot(dataset, aes(Sport, Satisfaction, fill = Gender))
interactionbar +
  cleanup +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar",
               position = position_dodge(width = 0.90), 
               width = 0.2) +
  xlab("Sport Rated") +
  ylab("Average Satisfaction Score") +
  scale_fill_grey()

##post hocs ONLY DO THE SIG EFFECTS, THIS IS A DEMO

####gender only has two levels####
tapply(dataset$Satisfaction, dataset$Gender, mean)
tapply(dataset$Satisfaction, dataset$Gender, sd)

####sports main effects####
pairwise.t.test(dataset$Satisfaction,
                dataset$Sport,
                p.adjust.method = "bonferroni",
                paired = F,
                var.equal = T)

####effect size####
library(MOTE)
M = tapply(dataset$Satisfaction, dataset$Sport, mean)
stdev = tapply(dataset$Satisfaction, dataset$Sport, sd)
N = tapply(dataset$Satisfaction, dataset$Sport, length)
M;stdev;N

##none to football
effect1 = d.ind.t(m1 = M[1], m2 = M[2],
                  sd1 = stdev[1], sd2 = stdev[2],
                  n1 = N[1], n2 = N[2], a = .05)
effect1$d

##none to volleyball
effect2 = d.ind.t(m1 = M[1], m2 = M[3],
                  sd1 = stdev[1], sd2 = stdev[3],
                  n1 = N[1], n2 = N[3], a = .05)
effect2$d

##football to volleyball
effect3 = d.ind.t(m1 = M[2], m2 = M[3],
                  sd1 = stdev[2], sd2 = stdev[3],
                  n1 = N[2], n2 = N[3], a = .05)
effect3$d

####simple effect analysis for the interaction####
none = subset(dataset, Sport == "None")
FB = subset(dataset, Sport == "Football")
VB = subset(dataset, Sport == "Volleyball")

pairwise.t.test(none$Satisfaction,
                none$Gender,
                p.adjust.method = "bonferroni",
                paired = F,
                var.equal = T)

pairwise.t.test(FB$Satisfaction,
                FB$Gender,
                p.adjust.method = "bonferroni",
                paired = F,
                var.equal = T)

pairwise.t.test(VB$Satisfaction,
                VB$Gender,
                p.adjust.method = "bonferroni",
                paired = F,
                var.equal = T)

M2 = tapply(dataset$Satisfaction, list(dataset$Gender, dataset$Sport), mean)
stdev2 = tapply(dataset$Satisfaction, list(dataset$Gender, dataset$Sport), sd)
N2 = tapply(dataset$Satisfaction, list(dataset$Gender, dataset$Sport), length)
M2;stdev2;N2

##notice the way we are referencing the means here 

##none gender
effect12 = d.ind.t(m1 = M2[1,1], m2 = M2[2,1],
                  sd1 = stdev2[1,1], sd2 = stdev2[2,1],
                  n1 = N2[1,1], n2 = N2[2,1], a = .05)
effect12$d

##football gender
effect22 = d.ind.t(m1 = M2[1,2], m2 = M2[2,2],
                   sd1 = stdev2[1,2], sd2 = stdev2[2,2],
                   n1 = N2[1,2], n2 = N2[2,2], a = .05)
effect22$d

##volleyball gender
effect32 = d.ind.t(m1 = M2[1,3], m2 = M2[2,3],
                   sd1 = stdev2[1,3], sd2 = stdev2[2,3],
                   n1 = N2[1,3], n2 = N2[2,3], a = .05)
effect32$d

####power####
library(pwr)
##you would do power on the interaction effect usually, notice how this is different than one-way
##power runs on cohen's f - not to be confused with anova f. 
##take the eta squared to convert
eta = .36
feta = eta / (1-eta)

#u is df model
#v is df error, but we are trying to figure out sample size for each condition,
##so we leave this one blank. 
#f2 is cohen f squared (so we do not take the square root like last week)

pwr.f2.test(u = 2, v = NULL, f2 = feta, sig.level = .05, power = .80)

