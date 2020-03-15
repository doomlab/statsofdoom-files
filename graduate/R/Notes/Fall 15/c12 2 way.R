##chapter 12 two-way ANOVAs
##set working directory
setwd("~/e_files/TEACHING/745 Grad Statistics/notes/fall 15")
##import the file
dataset = read.csv("c12 2 way.csv")
options(scipen = 999)

##check out the data to make sure it's ok
summary(dataset)

##change up sport
dataset$Sport = factor(dataset$Sport,
                       levels = c("None", "Football", "Volleyball"))

##run levene's test
library(car)
leveneTest(Satisfaction ~ Gender*Sport, data = dataset, center = mean)

##run that anova
output = aov(Satisfaction ~ Gender*Sport, data = dataset)
summary(output)

##get the effect sizes for each one
ssresidual = 3488
npgender = 169 / (169 + ssresidual)
npsport = 3332 / (3332 + ssresidual)
npinteraction = 1978 / (1978 + ssresidual)
npgender; npsport; npinteraction

##omega formulas
#(df model)*(Fmodel - 1) / (df model)*(Fmodel - 1)+a*n
table(dataset$Gender)
table(dataset$Sport)
tapply(dataset$Satisfaction, list(dataset$Gender, dataset$Sport), length)

ogender = (1*(2.03-1)) / (1*(2.03-1)+2*24)
osport = (2*(20.07 - 1)) / (2*(20.07 - 1)+(3*16))
ointeraction = (2*(11.91-1)) / (2*(11.91-1)+6*8)
ogender;osport;ointeraction

##graphs
library(ggplot2)
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(color = "black"))

##gender
genderbar = ggplot(dataset, aes(Gender, Satisfaction))
genderbar +
  theme +
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
  theme +
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
  theme +
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

##gender only has two levels, so let's get the means
tapply(dataset$Satisfaction, dataset$Gender, mean)
tapply(dataset$Satisfaction, dataset$Gender, sd)

##sports
library(agricolae)
HSD.test(output, "Sport", group = FALSE, console = TRUE) ##tukey

##interactions
##pick across or down!
##split the datasets by the larger variable to get a smaller number of tests
none = subset(dataset, Sport == "None")
football = subset(dataset, Sport == "Football")
volley = subset(dataset, Sport == "Volleyball")

##run some small anova - be sure to exclude the variable you split by!
noneoutput = aov(Satisfaction ~ Gender, data = none)
footoutput = aov(Satisfaction ~ Gender, data = football)
volleyoutput = aov(Satisfaction ~ Gender, data = volley)
##don't need the summaries because we aren't wanting these ANOVAs

##get the post hocs!
HSD.test(noneoutput, "Gender", group = FALSE, console = TRUE) ##tukey
HSD.test(footoutput, "Gender", group = FALSE, console = TRUE) ##tukey
HSD.test(volleyoutput, "Gender", group = FALSE, console = TRUE) ##tukey
