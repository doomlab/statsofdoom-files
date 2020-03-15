##dr. b's working directory
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/10 Regression")

##import file
master = read.csv("dummy code.csv")

##run regression
output = lm(friends ~ group, data = master)
summary(output)

##refactor the variable
table(master$group)
master$group2 = factor(master$group,
                       levels = c("poor", "fair", "excellent"))
table(master$group2)

##rerun exact same regression with new variable
output = lm(friends ~ group2, data = master)
summary(output)

##comparison to ANOVA
library(ez)
master$partno = 1:nrow(master)
ezANOVA(data = master,
        wid = partno,
        between = group,
        dv = friends,
        type = 3)

library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

graph = ggplot(master, aes(group2, friends))
graph +
  cleanup +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  cleanup +
  xlab("Health Condition") +
  scale_x_discrete(labels = c("Poor", "Fair", "Excellent")) + 
  ylab("Friends") 
  
graph +
  cleanup +
  geom_point() +
  xlab("Health Condition") +
  scale_x_discrete(labels = c("Poor", "Fair", "Excellent")) + 
  ylab("Friends") 
  