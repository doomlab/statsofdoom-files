##correlation
correlation <- read.csv("~/e_files/TEACHING/200 Statistics/nolan 3rd R/exams/exam 4/correlation.csv")

##scatterplot
library(ggplot2)
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"),
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"))

scatter = ggplot(correlation, aes(math, belief))
scatter +
  geom_point() + 
  theme +
  xlab("Math Ability") +
  ylab("Belief Math is Innate")

cor.test(correlation$math, correlation$belief)

qt(.05/2, 4, lower.tail = F)

##regression
regression1 <- read.csv("~/e_files/TEACHING/200 Statistics/nolan 3rd R/exams/exam 4/regression1.csv")

output = lm(Response.Time ~ Familiarity, data = regression1)
summary(output)

library(QuantPsyc)
lm.beta(output)

qt(.05/2, 5, lower.tail = F)

scatter = ggplot(regression1, aes(Familiarity, Response.Time))
scatter +
  geom_point() + 
  theme +
  xlab("Familiarity") +
  ylab("Response Time") +
  geom_smooth(method = "lm")

##multiple regression
regression2 <- read.csv("~/e_files/TEACHING/200 Statistics/nolan 3rd R/exams/exam 4/regression2.csv")

output = lm(Rating ~ Letter.Frequency + Word.Frequency + Typing.Speed,
            data = regression2)
summary(output)

lm.beta(output)

qt(.05/2, 11, lower.tail = F)

predicted = output$fitted.values
myplot = ggplot(regression2, aes(predicted, Rating))
myplot +
  geom_point() +
  xlab("Predicted Score") +
  ylab("Rating") +
  theme +
  geom_smooth(method = "lm")
