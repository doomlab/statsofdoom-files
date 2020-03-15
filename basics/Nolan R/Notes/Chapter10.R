Chapter10_data <- read_csv("~/Downloads/Chapter10 data.csv")

difference = Chapter10_data$new - Chapter10_data$old

mean(difference)
sd(difference)
sd(difference) / sqrt(length(difference))

qt(.01/1, 6, lower.tail = F)

t.test(Chapter10_data$new,
       Chapter10_data$old,
       paired = T,
       alternative = "greater",
       conf.level = .99)

##dependent t differences
d.deptdiff(mdiff = 0.8571429, sddiff = 1.069045, 
           n = 7, a = .01, k = 2)

Chapter10_data_2 <- read_csv("~/Downloads/Chapter10 data 2.csv")

difference = Chapter10_data_2$after - Chapter10_data_2$before
mean(difference)
sd(difference)
sd(difference) / sqrt(length(difference))

qt(.05/1, 7, lower.tail = T)

t.test(Chapter10_data_2$after,
       Chapter10_data_2$before,
       paired = T,
       alternative = "less",
       conf.level = .95)

##dependent t differences
d.deptdiff(mdiff = -1.25, sddiff = 2.251983, 
           n = 8, a = .05, k = 2)
