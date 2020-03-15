setwd("/Users/buchanan/e_files/TEACHING/745 Grad Statistics/notes/fall 15")


install.packages("ggplot2")
library(ggplot2)


##these are separated for demo purposes only
cricket = read.csv("c4 Jiminy Cricket.csv")
crickethist = ggplot(cricket, aes(Success_Pre))
crickethist + 
  geom_histogram()

crickethist + 
  geom_histogram(binwidth = 0.4)

crickethist + 
  geom_histogram(binwidth = 0.4, color = "green")

crickethist + 
  geom_histogram(binwidth = 0.4, color = "green") + 
  xlab("Success Pre Test") + 
  ylab("Frequency")

#more about histograms
festival = read.csv("c4 festival.csv", header=TRUE)
festivalhist = ggplot(festival, aes(day1))
festivalhist + 
  geom_histogram(binwidth = 0.4 ) + 
  xlab("Hygiene (Day 1 of Festival)") + 
  ylab("Frequency")


##scatterplots
exam = read.csv("c4 Exam Anxiety.csv", header=TRUE)
exam$Gender = factor(exam$Gender, 
                     levels=c(1,2), 
                     labels = c("Male", "Female"))
scatter = ggplot(exam, aes(Anxiety, Exam))
scatter + geom_point() + 
  xlab("Anxiety Level") + 
  ylab("Exam Performance")

scatter + geom_point() + 
  xlab("Anxiety Level") + 
  ylab("Exam Performance") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))

##scatter with regression line
scatter2 = ggplot(exam, aes(Anxiety, Exam))
scatter2 + 
  geom_point() + 
  geom_smooth(method = "lm", color = "Red") + 
  xlab("Anxiety Level") + 
  ylab("Exam Performance") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))

##grouped scatterplot
scatter = ggplot(exam, aes(Anxiety, Exam, color = Gender))
scatter + geom_point() + 
  geom_smooth(method = "lm", aes(fill = Gender)) + 
  xlab("Anxiety Level") + ylab("Exam Performance") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))

##boxplot
festivalbox = ggplot(festival, aes(gender, day1))
festivalbox + 
  geom_boxplot() + 
  xlab("Hygiene (Day 1 of Festival)") + 
  ylab("Frequency") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))

##bar charts
chick = read.csv("c4 ChickFlick.csv", header=TRUE)
chick$gender = factor(chick$gender, levels = c(1,2), labels=c("Men", "Women"))
chick$film = factor(chick$film, levels=c(1,2), labels=c("Bridget Jones Diary", "Memento"))

chickbar = ggplot(chick, aes(film, arousal))
chickbar + 
  stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) +
  xlab("Film Watched") + 
  ylab("Arousal Level") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))

##two independent variables
chickbar2 = ggplot(chick, aes(film, arousal, fill = gender))
chickbar2 + 
  stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Film Watched") + ylab("Arousal Level") + 
  scale_fill_discrete(name="Gender") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))

##line graphs
hiccups = read.csv("c4 Hiccups.csv", header=TRUE)
View(hiccups)

##rearrange for a graph
library(reshape)
longhiccups = melt(hiccups, measured = c("Baseline", "Tongue", "Carotid", "Other"))
View(longhiccups)

##line graphs now
hiccupline = ggplot(longhiccups, aes(variable, value))

hiccupline + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), color = "Red", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  xlab("Intervention") + ylab("Mean Number of Hiccups") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))

##multiline graphs
texting = read.csv("c4 Texting.csv", header=TRUE)
View(texting)
texting$Group = factor(texting$Group, levels=c(1,2), labels=c("Texting Allowed", "Texting Forbidden"))
longtexting = melt(texting, id = c("Group"), measured=c("Baseline", "Six_months"))
View(longtexting)
colnames(longtexting)=c("Group", "Time", "Grammar_Score")

##actual graph
textline = ggplot(longtexting, aes(Time, Grammar_Score, color = Group))

textline + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = Group)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  xlab("Measurement Time") + ylab("Mean Grammar Score") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"),
  legend.key = element_rect(fill = "white"))


