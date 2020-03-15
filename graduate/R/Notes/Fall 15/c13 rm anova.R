##set working directory
setwd("~/e_files/TEACHING/745 Grad Statistics/notes/fall 15")

##import file
rmone = read.csv("c13 rm 1.csv")
rmtwo = read.csv("c13 rm 2.csv")

##add participant number
rmone = cbind(rmone, partno = c(1:nrow(rmone)))

##move to long format
library(reshape)
rmonel = melt(rmone,
              id = "partno", 
              measured = c("haunt", "costume", "punch", "party"))

##useful column names
colnames(rmonel) = c("partno", "costume", "rating")

##EZ ANOVA
options(scipen = 999)
library(ez)
output = ezANOVA(data = rmonel,
                 dv = rating,
                 wid = partno,
                 within = costume,
                 detailed = TRUE,
                 type = 3)
output

##post hoc tests
##bonferroni
pairwise.t.test(rmonel$rating, 
                rmonel$costume, 
                paired = TRUE, 
               p.adjust.method = "bonferroni")
##Tukey
##first run as lme
library(nlme)
output2 = lme(rating ~ costume, 
              random = ~1|partno, 
              data = rmonel, 
              method = "ML")
##then run the tukey on that
library(multcomp)
tukey = glht(output2, linfct = mcp(costume = "Tukey"))
summary(tukey)

##get the means, sd, N for calculating d avg
##d diff is not really possible since we don't get t values
with(rmonel, tapply(rating, costume, mean))
with(rmonel, tapply(rating, costume, sd))
with(rmonel, tapply(rating, costume, length))

##create a graph
library(ggplot2)
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(color = "black"))

##one way
bargraph = ggplot(rmonel, aes(costume, rating))
bargraph +
  theme +
  stat_summary(fun.y = mean,
               geom = "bar",
               fill = "white",
               color = "black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar",
               position = "dodge",
               width = .2) +
  xlab("Type of Idea") +
  ylab("Average Rating Score of Idea") +
  scale_x_discrete(labels = c("Haunted House", "Small Costume", "Punch Bowl", "House Party"))


##two way repeated only 
##add a participant number
rmtwo = cbind(rmtwo, partno = c(1: nrow(rmtwo)))
##melt the data
library(reshape)
rmtwol = melt(rmtwo,
              id = "partno",
              measured = c("beerpos","beerneg","beerneut",
                           "winepos","wineneg","wineneut",
                           "waterpos","waterneg","waterneut"))
summary(rmtwol)
##label the columns
colnames(rmtwol) = c("partno", "group", "attitude")
##fix the labels into two columns!
rmtwol$drink = gl(3,60,labels = c("Beer", "Wine", "Water"))
rmtwol$image = gl(3, 20, 180, labels = c("Positive", "Negative", "Neutral"))

##run the anova
library(ez)
output = ezANOVA(data = rmtwol,
                 dv = attitude,
                 wid = partno,
                 within = .(drink, image),
                 detailed = TRUE,
                 type = 3)
output

##post hoc tests
##bonferroni for drink
pairwise.t.test(rmtwol$attitude, 
                rmtwol$drink, 
                paired = TRUE, 
                p.adjust.method = "bonferroni")

##bonferroni for imagery
pairwise.t.test(rmtwol$attitude, 
                rmtwol$image, 
                paired = TRUE, 
                p.adjust.method = "bonferroni")
##Tukey
##first run as lme
library(nlme)
output2 = lme(attitude ~ drink*image, 
              random = ~1|partno, 
              data = rmtwol, 
              method = "ML")
##then run the tukey on that
library(multcomp)
tukey = glht(output2, linfct = mcp(drink = "Tukey", interaction_average=TRUE))
summary(tukey)
tukey2 = glht(output2, linfct = mcp(image = "Tukey", interaction_average=TRUE))
summary(tukey2)

##interaction simple effects analysis
##first, divide up the data by the larger levels
positive = subset(rmtwol, image == "Positive")
negative = subset(rmtwol, image == "Negative")
neutral = subset(rmtwol, image == "Neutral")

##bonferroni
pairwise.t.test(positive$attitude, 
                positive$drink, 
                paired = TRUE, 
                p.adjust.method = "bonferroni")

pairwise.t.test(negative$attitude, 
                negative$drink, 
                paired = TRUE, 
                p.adjust.method = "bonferroni")

pairwise.t.test(neutral$attitude, 
                neutral$drink, 
                paired = TRUE, 
                p.adjust.method = "bonferroni")

##Tukey
##first run as lme
library(nlme)
posout = lme(attitude ~ drink, 
              random = ~1|partno, 
              data = positive, 
              method = "ML")
negout = lme(attitude ~ drink, 
             random = ~1|partno, 
             data = negative, 
             method = "ML")
neuout = lme(attitude ~ drink, 
             random = ~1|partno, 
             data = neutral, 
             method = "ML")

##then run the tukey on that
library(multcomp)
post = glht(posout, linfct = mcp(drink = "Tukey"))
negt = glht(negout, linfct = mcp(drink = "Tukey"))
neut = glht(neuout, linfct = mcp(drink = "Tukey"))
summary(post)
summary(negt)
summary(neut)

##get the means, sd, N for calculating d avg
##d diff is not really possible since we don't get t values
with(rmtwol, tapply(attitude, drink, mean))
with(rmtwol, tapply(attitude, drink, sd))
with(rmtwol, tapply(attitude, drink, length))
with(rmtwol, tapply(attitude, image, mean))
with(rmtwol, tapply(attitude, image, sd))
with(rmtwol, tapply(attitude, image, length))
with(rmtwol, tapply(attitude, list(drink, image), mean))
with(rmtwol, tapply(attitude, list(drink, image), sd))
with(rmtwol, tapply(attitude, list(drink, image), length))

##make the graph
library(ggplot2)
bargraph = ggplot(rmtwol, aes(image, attitude, fill = drink))
bargraph +
  theme +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width=.9),
               width = .2) +
  xlab("Imagery Condition") +
  ylab("Average Attitude Rating") +
  scale_fill_discrete(name = "Drink in Ad")
