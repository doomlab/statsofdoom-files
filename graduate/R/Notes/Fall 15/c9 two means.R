##set working directory
setwd("~/e_files/TEACHING/745 Grad Statistics/notes/fall 15")

longdata = read.csv("c9 invisible long.csv", header = T)

##calculate means and sds for descriptive statistics
##to use tapply you would need data in long format (either between or within)
##use melt to convert to long format if you need to
M = tapply(longdata$Mischief, longdata$Cloak, mean)
sd = tapply(longdata$Mischief, longdata$Cloak, sd)

##independent t 
#Y ~ X or DV ~ IV group
t.test(Mischief ~ Cloak, data = longdata, var.equal = TRUE, paired = FALSE)

##to get effect size, you'll need to group sizes
N = tapply(longdata$Mischief, longdata$Cloak, length)

##dependent t
t.test(Mischief ~ Cloak, data = longdata, var.equal = TRUE, paired = TRUE)

##chart
library(ggplot2)

theme = theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"))

bargraph = ggplot(longdata, aes(Cloak, Mischief))
bargraph +
  theme +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2, 
               position = "dodge") +
  xlab("Invisible Cloak Group") +
  ylab("Average Mischief Acts")
