#install.packages(c("ez", "ggplot2", "Hmisc", "pwr", "devtools", "MBESS"))
#devtools::install_github("doomlab/MOTE")

####set working directory####
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 16")

####import file####
rmone = read.csv("c13 rm 1.csv")
rmtwo = read.csv("c13 rm 2.csv")

####data screening would go here####
min(abs(cor(rmone)))
min(abs(cor(rmtwo)))

####repeat measures one way####
##add a participant number
##you should add it before melting, that's important
rmone$partno = 1:nrow(rmone)

####go to long format####
library(reshape)
rmonelong = melt(rmone,
              id = "partno", 
              measured = c("haunt", "costume", "punch", "party"))

####useful column names####
colnames(rmonelong) = c("partno", "costume", "rating")

####EZ ANOVA####
options(scipen = 999)
library(ez)
ezANOVA(data = rmonelong,
        dv = rating,
        within = costume,
        wid = partno,
        detailed = T,
        type = 3)

####post hoc tests####
pairwise.t.test(rmonelong$rating, 
                rmonelong$costume, 
                paired = T, 
                var.equal = T,
                p.adjust.method = "bonferroni")

####effect size####
##run the effsize.R script
##get the means, sd, N for calculating d avg
M = with(rmonelong, tapply(rating, costume, mean))
stdev = with(rmonelong, tapply(rating, costume, sd))
N = length(rmone$partno) ##note here!
M;stdev;N

library(MOTE)

##haunt to costume
effect1 = d.dep.t.avg(m1 = M[1], m2 = M[2],
                  sd1 = stdev[1], sd2 = stdev[2],
                  n = N[1], a = .05)
effect1$d

##haunt to punch
effect2 = d.dep.t.avg(m1 = M[1], m2 = M[3],
                      sd1 = stdev[1], sd2 = stdev[3],
                      n = N[1], a = .05)
effect2$d

##haunt to party
effect3 = d.dep.t.avg(m1 = M[1], m2 = M[4],
                      sd1 = stdev[1], sd2 = stdev[4],
                      n = N[1], a = .05)
effect3$d

##costume to punch
effect4 = d.dep.t.avg(m1 = M[2], m2 = M[3],
                      sd1 = stdev[2], sd2 = stdev[3],
                      n = N[1], a = .05)
effect4$d

##costume to party
effect5 = d.dep.t.avg(m1 = M[2], m2 = M[4],
                      sd1 = stdev[2], sd2 = stdev[4],
                      n = N[1], a = .05)
effect5$d

##punch to party
effect6 = d.dep.t.avg(m1 = M[3], m2 = M[4],
                      sd1 = stdev[3], sd2 = stdev[4],
                      n = N[1], a = .05)
effect6$d

####create a graph####
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

##one way graphs
bargraph = ggplot(rmonelong, aes(costume, rating))
bargraph +
  cleanup +
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

####power####
library(pwr)
##you would do power on the interaction effect usually, notice how this is different than one-way
##power runs on cohen's f - not to be confused with anova f. 
##take the eta squared to convert
eta = 0.3274249
feta = eta / (1-eta)

#u is df model
#v is df error, but we are trying to figure out sample size for each condition,
##so we leave this one blank. 
#f2 is cohen f squared 

pwr.f2.test(u = 3, v = NULL, f2 = feta, sig.level = .05, power = .80)

####two way repeated only ####
##add a participant number
rmtwo$partno = 1:nrow(rmtwo)

####go to long format####
library(reshape)
rmtwolong = melt(rmtwo,
              id = "partno",
              measured = c("beerpos","beerneg","beerneut",
                           "winepos","wineneg","wineneut",
                           "waterpos","waterneg","waterneut"))
summary(rmtwolong)

####useful column names####
colnames(rmtwolong) = c("partno", "group", "attitude")

####fix the labels into two columns!####
rmtwolong$drink = gl(3, 60, 180, labels = c("Beer", "Wine", "Water"))
rmtwolong$image = gl(3, 20, 180, labels = c("Positive", "Negative", "Neutral"))

####EZ ANOVA####
options(scipen = 999)
library(ez)
ezANOVA(data = rmtwolong,
        dv = attitude,
        wid = partno,
        within = .(drink, image),
        detailed = T,
        type = 3)

####post hoc tests and effect size####
##main effects for drink
pairwise.t.test(rmtwolong$attitude, 
                rmtwolong$drink, 
                paired = T, 
                var.equal = T,
                p.adjust.method = "bonferroni")

M2 = with(rmtwolong, tapply(attitude, drink, mean))
stdev2 = with(rmtwolong, tapply(attitude, drink, sd))
N2 = length(rmtwo$partno) ##note here!
M2;stdev2;N2

library(MOTE)
##beer to wine
effect7 = d.dep.t.avg(m1 = M2[1], m2 = M2[2],
                      sd1 = stdev2[1], sd2 = stdev2[2],
                      n = N2[1], a = .05)
effect7$d

##beer to water
effect8 = d.dep.t.avg(m1 = M2[1], m2 = M2[3],
                      sd1 = stdev2[1], sd2 = stdev2[3],
                      n = N2[1], a = .05)
effect8$d

##wine to water
effect9 = d.dep.t.avg(m1 = M2[2], m2 = M2[3],
                      sd1 = stdev2[2], sd2 = stdev2[3],
                      n = N2[1], a = .05)
effect9$d

##main effects for imagery
pairwise.t.test(rmtwolong$attitude, 
                rmtwolong$image, 
                paired = T, 
                var.equal = T,
                p.adjust.method = "bonferroni")

M3 = with(rmtwolong, tapply(attitude, image, mean))
stdev3 = with(rmtwolong, tapply(attitude, image, sd))
N3 = length(rmtwolong$partno) ##note here!
M3;stdev3;N3

##positive to negative
effect10 = d.dep.t.avg(m1 = M3[1], m2 = M3[2],
                      sd1 = stdev3[1], sd2 = stdev3[2],
                      n = N3[1], a = .05)
effect10$d

##positive to neutral
effect11 = d.dep.t.avg(m1 = M3[1], m2 = M3[3],
                      sd1 = stdev3[1], sd2 = stdev3[3],
                      n = N2[1], a = .05)
effect11$d

##negative to neutral
effect12 = d.dep.t.avg(m1 = M3[2], m2 = M3[3],
                      sd1 = stdev3[2], sd2 = stdev3[3],
                      n = N2[1], a = .05)
effect12$d

####interaction simple effects analysis####
##first, divide up the data by the larger levels
positive = subset(rmtwolong, image == "Positive")
negative = subset(rmtwolong, image == "Negative")
neutral = subset(rmtwolong, image == "Neutral")

##post hoc test on the smaller datasets
pairwise.t.test(positive$attitude, 
                positive$drink, 
                paired = T, 
                var.equal = T,
                p.adjust.method = "bonferroni")

pairwise.t.test(negative$attitude, 
                negative$drink, 
                paired = T, 
                var.equal = T,
                p.adjust.method = "bonferroni")

pairwise.t.test(neutral$attitude, 
                neutral$drink, 
                paired = T, 
                var.equal = T,
                p.adjust.method = "bonferroni")

##get the means, sd, N for calculating d avg
M4= with(rmtwolong, tapply(attitude, list(drink, image), mean))
stdev4 = with(rmtwolong, tapply(attitude, list(drink, image), sd))
N4 = length(rmtwo$partno)
M4;stdev4;N4

library(MOTE)
##positive beer to wine
effect13 = d.dep.t.avg(m1 = M4[1,1], m2 = M4[2,1],
                   sd1 = stdev4[1,1], sd2 = stdev4[2,1],
                   n = N4[1], a = .05)
effect13$d

###...you would keep going for all nine pairwise combinations


####create the graph####
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

bargraph = ggplot(rmtwolong, aes(image, attitude, fill = drink))
bargraph +
  cleanup +
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

####power####
library(pwr)
##you would do power on the interaction effect usually, notice how this is different than one-way
##power runs on cohen's f - not to be confused with anova f. 
##take the eta squared to convert
eta = 0.1411741
feta = eta / (1-eta)

#u is df model
#v is df error, but we are trying to figure out sample size for each condition,
##so we leave this one blank. 
#f2 is cohen f squared 

pwr.f2.test(u = 4, v = NULL, f2 = feta, sig.level = .05, power = .80)

