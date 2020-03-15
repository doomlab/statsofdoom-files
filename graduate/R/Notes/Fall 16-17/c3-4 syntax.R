##Dr. B's computer
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 15")
##set the above line to your working directory

install.packages("haven")
install.packages("reshape")
library(haven)
library(reshape)

####read SPSS####
##read in an SPSS file with haven
chickdata = read_spss("c4 ChickFlick.sav")

####rename symbols####
##if your first column imports with a funky symbol
##remember you can fix it this way
##generally this will be the first column with bad imports
colnames(chickdata)[1] = "gender"

####factoring variables####
##make some fake data
notfactor = rep(1:3, 50)
table(notfactor)
factored = factor(notfactor, 
                  levels=c(1,2,3), 
                  labels = c("swiss", "feta", "gouda"))
table(factored)

####reshaping data####
##Load some data
cricket = read.csv("c4 Jiminy Cricket.csv", header=TRUE)
summary(cricket)

##wide to long
longcricket = melt(cricket, 
                   id = c("ID", "Strategy"), 
                   measured = c("Success_Pre", "Succcess_Post"))

####graph start####
##remember if you already have this package you do not need to install it
install.packages("ggplot2")
library(ggplot2)

####histograms####
##read in the data
cricket = read.csv("c4 Jiminy Cricket.csv", header=TRUE)

##create the blank plot
##notice for a histogram, AES only has ONE variable
crickethist = ggplot(cricket, aes(Success_Pre))
crickethist +
  geom_histogram(binwidth = 1, 
                 color = "black", 
                 fill = "white") +
  xlab("Success Pre Test") + 
  ylab("Frequency")

festival = read.csv("c4 festival.csv", header=TRUE)
festhist = ggplot(festival, aes(day1))
festhist +
  geom_histogram(binwidth = .4, 
                 color = "black", 
                 fill = "white") +
  xlab("Day 1 of Festival Hygiene") +
  ylab("Frequency") +
  theme_bw()

##Other clean up option
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

festhist +
  geom_histogram(binwidth = .4, 
                 color = "black", 
                 fill = "white") +
  xlab("Day 1 of Festival Hygiene") +
  ylab("Frequency") +
  cleanup 

library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 25))

####simple scatter plot####
##AES has X axis, Y axis
scatter = ggplot(exam, aes(Anxiety, Exam))
scatter +
  geom_point() +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup

####simple scatter with regression line####
scatter +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup

####grouped scatter plot###
##AES has X axis, Y axis, color/fill are the legend
scatter2 = ggplot(exam, aes(Anxiety, Exam, color = Gender, fill = Gender))
scatter2 +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup + 
  scale_fill_manual(name = "Gender of Participant",
                    labels = c("Men", "Women"),
                    values = c("black", "grey")) +
  scale_color_manual(name = "Gender of Participant",
                     labels = c("Men", "Women"),
                     values = c("black", "purple"))

##bar charts start
##import data and refactor
chick = read.csv("c4 ChickFlick.csv", header=TRUE)
chick$gender = factor(chick$gender, 
                      levels = c(1,2), 
                      labels = c("Men", "Women"))
chick$film = factor(chick$film,
                    levels = c(1,2),
                    labels = c("Bridget Jones", "Memento"))

####bar charts with one variable####
##AES has X axis (categorical), Y axis (continuous)
chickbar = ggplot(chick, aes(film, arousal))
chickbar + 
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2, 
               position = "dodge") +
  cleanup +
  xlab("Movie Watched by Participants") +
  ylab("Arousal Level") +
  scale_x_discrete(labels = c("Girl Film", "Guy Film"))

####bar chart two independent variables####
##AES has X axis (categorical), Y axis (continuous), fill legend (categorical)
chickbar2 = ggplot(chick, aes(film, arousal, fill = gender))
chickbar2 +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = "dodge") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = .2) +
  xlab("Film Watched") +
  ylab("Arousal Level") + 
  cleanup +
  scale_fill_manual(name = "Gender of Participant", 
                    labels = c("Boys", "Girls"),
                    values = c("Black", "Gray"))

##line graphs start
##import data
hiccups = read.csv("c4 Hiccups.csv", header=TRUE)
View(hiccups)

##melt the data to long format
##notice there's no ID, that's ok
library(reshape)
longhiccups = melt(hiccups,
                   measured = c("Baseline","Tongue","Carotid","Other"))

####single line graph####
##AES is X axis (categorical), Y axis (continuous)
colnames(longhiccups) = c("Intervention", "Hiccups")
hiccupline = ggplot(longhiccups, aes(Intervention, Hiccups))
hiccupline +
  stat_summary(fun.y = mean, ##adds the points
               geom = "point") +
  stat_summary(fun.y = mean, ##adds the line
               geom = "line",
               aes(group=1)) +
  stat_summary(fun.data = mean_cl_normal, ##adds the error bars
               geom = "errorbar", 
               width = .2) +
  xlab("Intervention Type") +
  ylab("Number of Hiccups") + 
  cleanup

##multiline graph start
##import, melt, and factor the data
texting = read.csv("c4 Texting.csv", header=TRUE)
texting$Group = factor(texting$Group,
                       levels = c(1,2),
                       labels = c("Texting Allowed", "No Texting Allowed"))
longtexting = melt(texting,
                   id = c("Group"),
                   measured = c("Baseline", "Six_months"))
colnames(longtexting) = c("Group", "Time", "Grammar_Score")

####multiline graph####
##AES has X axis (categorical), Y axis (continuous), color legend (categorical)
textline = ggplot(longtexting, 
                  aes(Time, Grammar_Score, 
                      color = Group))
textline +
  stat_summary(fun.y = mean,
               geom = "point") +
  stat_summary(fun.y = mean,
               geom = "line", 
               aes(group = Group)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = .2) + 
  xlab("Measurement Time") +
  ylab("Mean Grammar Score") +
  cleanup +
  scale_color_manual(name = "Texting Option",
                     labels = c("All the texts", "None of the texts"),
                     values = c("Black", "Grey")) +
  scale_x_discrete(labels = c("Baseline", "Six Months"))


