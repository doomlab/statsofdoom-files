##load the library
library(ggplot2)

##theme coding
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"))

myplot = ggplot(newdata, aes(posemo, negemo))
myplot +
  theme +
  geom_point() +
  xlab("Positive Emotion") +
  ylab("Negative Emotion")

##just an example for RP4
newdata = subset(posemo_negemo, posemo > 0 | negemo > 0)

output = lm(negemo ~ posemo, data = newdata)
summary(output)

install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(output)

qt(.05/2, 35337, lower.tail = F)


##example multiple regression
output = lm(Clout ~ power + Authentic + affiliation + Tone, 
            data = multiple)
summary(output)

install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(output)

qt(.05/2, 58426, lower.tail = F)

##load the library
library(ggplot2)

##theme coding
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"))

##remember this was only for the class example
multiple = multiple[ 1:58431 ,]

myplot = ggplot(multiple, aes(output$fitted.values, Clout))
myplot +
  theme +
  geom_point() +
  xlab("Predicted Score") +
  ylab("Clout Scores")
