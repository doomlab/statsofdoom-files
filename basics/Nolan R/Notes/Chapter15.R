##load the library
library(ggplot2)

##theme coding
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"))

myplot = ggplot(chapter15, aes(Bars, Churches))
myplot +
  theme +
  geom_point() +
  xlab("Number of Bars") +
  ylab("Number of Churches")

cor.test(chapter15$Bars, chapter15$Churches)

qt(.05/2, 5, lower.tail = F)
