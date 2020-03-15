##use airquality data 
data("airquality")
head(airquality)

##factor the variabl - only run this once!
table(airquality$Month)

airquality$Month = factor(airquality$Month,
                          levels = c(5,6,7,8,9),
                          labels = c("May", "June", "July", "August", "September"))


##load the library
library(ggplot2)

##theme coding
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))


##scatterplots
myplot = ggplot(airquality, aes(Temp, Ozone))
myplot + 
  geom_point() + 
  cleanup +
  xlab("STUFF AND THINGS") +
  ylab("SUPER HOT STUFF")


##scatterplots with a line of best fit
myplot = ggplot(airquality, aes(Temp, Ozone))
myplot + 
  geom_point() + 
  cleanup +
  xlab("STUFF AND THINGS") +
  ylab("SUPER HOT STUFF") +
  geom_smooth(method = "lm")

##bar graph
myplot = ggplot(airquality, aes(Month, Temp))
myplot +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  cleanup +
  ylab("Temperature in F")


