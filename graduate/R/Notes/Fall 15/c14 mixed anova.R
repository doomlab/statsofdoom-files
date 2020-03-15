##set working directory
setwd("~/e_files/TEACHING/745 Grad Statistics/notes/fall 15")

##import the file
mixed = read.csv("c14 mixed.csv", header = T)

##add participant number
mixed = cbind(mixed, partno = c(1:nrow(mixed)))

##move to long format
library(reshape)
mixedlong = melt(mixed,
              id = c("partno", "Gender"), 
              measured = c("high_char","med_char","none_char"))

##useful column names
colnames(mixedlong) = c("partno", "Gender", "Charisma", "Rating")

##rename those levels and reorder
mixedlong$Charisma = factor(mixedlong$Charisma, 
                            levels = c("none_char", "med_char", "high_char"),
                            labels = c("None", "Medium", "High"))

##Levene's 
library(car)
leveneTest(Rating ~ Gender, data = mixedlong, center = mean)

##EZ ANOVA
options(scipen = 999)
library(ez)
output = ezANOVA(data = mixedlong,
                 dv = Rating,
                 wid = partno,
                 within = Charisma,
                 between = Gender,
                 detailed = TRUE,
                 type = 3)
output

##graph
library(ggplot2)
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(color = "black"))
bargraph = ggplot(mixedlong, aes(Charisma, Rating, fill = Gender))
bargraph +
  theme +
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

##AOV option
library(agricolae)
noneout = aov(Rating ~ Gender, data = none)
medout = aov(Rating ~ Gender, data = med)
highout = aov(Rating ~ Gender, data = high)
HSD.test(noneout, "Gender", group = FALSE, console = TRUE) 
HSD.test(medout, "Gender", group = FALSE, console = TRUE) 
HSD.test(highout, "Gender", group = FALSE, console = TRUE) 

##pairwise option
with(none, pairwise.t.test(Rating, 
                           Gender, 
                           paired = FALSE, 
                           p.adjust.method = "bonferroni"))
with(med, pairwise.t.test(Rating, 
                           Gender, 
                           paired = FALSE, 
                           p.adjust.method = "bonferroni"))
with(high, pairwise.t.test(Rating, 
                           Gender, 
                           paired = FALSE, 
                           p.adjust.method = "bonferroni"))

##LME option
library(nlme)
noneout = lme(Rating ~ Gender, 
              random = ~1|partno, 
              data = none, 
              method = "ML")
medout = lme(Rating ~ Gender, 
              random = ~1|partno, 
              data = med, 
              method = "ML")
highout = lme(Rating ~ Gender, 
              random = ~1|partno, 
              data = high, 
              method = "ML")

library(multcomp)
nonet = glht(noneout, linfct = mcp(Gender = "Tukey"))
medt = glht(medout, linfct = mcp(Gender = "Tukey"))
hight = glht(highout, linfct = mcp(Gender = "Tukey"))
summary(nonet)
summary(medt)
summary(hight)
