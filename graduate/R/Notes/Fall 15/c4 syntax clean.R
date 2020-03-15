library(ggplot2)
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 15")

##histograms
cricket = read.csv("c4 Jiminy Cricket.csv", header=TRUE)
crickethist = ggplot(cricket, aes(Success_Pre))
crickethist +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  xlab("Success Pre Test") + 
  ylab("Frequency")

festival = read.csv("c4 festival.csv", header=TRUE)
festhist = ggplot(festival, aes(day1))
festhist +
  geom_histogram(binwidth = .4, color = "black", fill = "white") +
  xlab("Day 1 of Festival Hygiene") +
  ylab("Frequency") +
  theme_bw()

##Other clean up option
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(color = "black"))

festhist +
  geom_histogram(binwidth = .4, color = "black", fill = "white") +
  xlab("Day 1 of Festival Hygiene") +
  ylab("Frequency") +
  cleanup


##scatterplot
exam = read.csv("c4 Exam Anxiety.csv", header=TRUE)
exam$Gender = factor(exam$Gender,
                     levels = c(1,2),
                     labels = c("Male", "Female"))

##simple scatter plot
scatter = ggplot(exam, aes(Anxiety, Exam))
scatter +
  geom_point() +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup

##simple scatter with regression line
scatter +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup

##grouped scatter plot
scatter2 = ggplot(exam, aes(Anxiety, Exam, color = Gender))
scatter2 +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = Gender)) +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup + 
  scale_fill_manual(name = "Gender of Participant",
                    labels = c("Men", "Women"),
                    values = c("black", "grey")) +
  scale_color_manual(name = "Gender of Participant",
                     labels = c("Men", "Women"),
                     values = c("black", "purple"))

##boxplot 
festbox = ggplot(festival, aes(gender, day1))
festbox + 
  geom_boxplot() +
  xlab("Gender of Festival Goer") +
  ylab("Hygiene of Day 1") +
  cleanup +
  scale_x_discrete(labels = c("Girls", "Boys"))

##bar charts with one variable
chick = read.csv("c4 ChickFlick.csv", header=TRUE)
chick$gender = factor(chick$gender, 
                      levels = c(1,2), 
                      labels = c("Men", "Women"))
chick$film = factor(chick$film,
                    levels = c(1,2),
                    labels = c("Bridget Jones", "Memento"))
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

##two independent variables
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

##line graphs
hiccups = read.csv("c4 Hiccups.csv", header=TRUE)
View(hiccups)

##melt the data to long format
library(reshape)
longhiccups = melt(hiccups,
                   measured = c("Baseline","Tongue","Carotid","Other"))

##single line graph
colnames(longhiccups) = c("Intervention", "Hiccups")
hiccupline = ggplot(longhiccups, aes(Intervention, Hiccups))
hiccupline +
  stat_summary(fun.y = mean, ##adds points
               geom = "point") +
  stat_summary(fun.y = mean, ##adds the line
               geom = "line",
               aes(group=1)) +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2) +
  xlab("Intervention Type") +
  ylab("Number of Hiccups") + 
  cleanup

##multiline graph
texting = read.csv("c4 Texting.csv", header=TRUE)
texting$Group = factor(texting$Group,
                       levels = c(1,2),
                       labels = c("Texting Allowed", "No Texting Allowed"))
longtexting = melt(texting,
                   id = c("Group"),
                   measured = c("Baseline", "Six_months"))
colnames(longtexting) = c("Group", "Time", "Grammar_Score")

##multiline graph
textline = ggplot(longtexting, aes(Time, Grammar_Score, color = Group))
textline +
  stat_summary(fun.y = mean,
               geom = "point") +
  stat_summary(fun.y = mean,
               geom = "line", 
               aes(group = Group)) +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = .2) + 
  xlab("Measurement Time") +
  ylab("Mean Grammar Score") +
  cleanup +
  scale_color_manual(name = "Texting Option",
                     labels = c("All the texts", "None of the texts"),
                     values = c("Black", "Grey")) +
  theme(legend.key = element_rect(fill = "white"))

