####review exam 1 answers####
reviewdata <- read.csv("~/e_files/TEACHING/200 Statistics/nolan 3rd R/exams/review exam 1.csv")

##histogram of record sales
library(ggplot2)
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"))

myplot = ggplot(reviewdata, aes(Record.Sales))
myplot +
  theme +
  geom_histogram(binwidth = 50)

table(reviewdata$Chart.Weeks)

summary(reviewdata$Record.Sales)
IQR(reviewdata$Record.Sales, na.rm = T)

temp <- table(as.vector(reviewdata$Record.Sales))
names(temp)[temp == max(temp)]


summary(reviewdata$Chart.Weeks)
IQR(reviewdata$Chart.Weeks, na.rm = T)

temp <- table(as.vector(reviewdata$Chart.Weeks))
names(temp)[temp == max(temp)]

pop.var <- function(x) var(x) * (length(x)-1) / length(x) 
pop.sd <- function(x) sqrt(pop.var(x))
pop.var(reviewdata$Chart.Weeks)
pop.sd(reviewdata$Chart.Weeks)

var(reviewdata$Chart.Weeks, na.rm = T)
sd(reviewdata$Chart.Weeks, na.rm = T)

myplot = ggplot(reviewdata, aes(Record.Sales, Chart.Weeks))
myplot +
  theme +
  geom_point() +
  geom_smooth(method = "lm")

myplot = ggplot(reviewdata, aes(Type.of.Artist, Record.Sales))
myplot + 
  theme +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") + 
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  xlab("Type of Artist") + 
  ylab("Record Sales in 100k")

