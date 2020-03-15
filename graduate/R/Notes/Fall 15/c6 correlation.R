##set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 15")

##load the data
examdata = read.csv("c6 correlation.csv", header = TRUE)
examdata$Gender = factor(examdata$Gender, 
                         levels = c(1,2),
                         labels = c("Male", "Female"))

##make a scatterplot
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

##code taken from chatper 4 notes
####simple scatter plot####
scatter = ggplot(examdata, aes(Anxiety, Exam))
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

####grouped scatter plot####
scatter2 = ggplot(examdata, aes(Anxiety, Exam, color = Gender))
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

####simple scatter x/y limits####
scatter = ggplot(examdata, aes(Anxiety, Exam))
scatter +
  geom_point() +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup +
  coord_cartesian(xlim = c(0,100), ylim = c(60,101))

####covariance example####
adverts = c(5,4,4,6,8)
packets = c(8,9,10,13,15)
plot(adverts,packets)
abline(lm(packets~adverts))

####correlation####
library(Hmisc)
cor(examdata[,-c(1, 5)], use="pairwise.complete.obs", method = "pearson")
rcorr(as.matrix(examdata[,-c(1, 5)]), type = "pearson")
cor.test(examdata$Exam, examdata$Anxiety, method = "pearson")

####nonparametric example####
liardata = read.csv("c6 liar.csv", header = TRUE)
with(liardata, cor.test(Creativity, Position, method = "spearman"))
with(liardata, cor.test(Creativity, Position, method = "kendall"))

####point biserial example####
liardata$Novice2 = as.numeric(liardata$Novice)
with(liardata, cor.test(Creativity, Novice2))

####comparing correlations####
library(cocor)

##independent correlations
new = subset(liardata, Novice == "First Time")
old = subset(liardata, Novice == "Had entered Competition Before")
inddata = list(new, old)
cocor(~Creativity + Position | Creativity + Position,
      data = inddata)

##dependent correlations
cocor(~Revise + Exam | Revise + Anxiety, 
      data = examdata)

####partial correlations####
library(ppcor)
pcor(examdata[ , -c(1,5)], method = "pearson")
spcor(examdata[ , -c(1,5)], method = "pearson")
