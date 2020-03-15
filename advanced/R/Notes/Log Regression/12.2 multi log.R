##dr b's working directory, yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/12 Log Regression")

##import the file
master = read.csv("multi log.csv")

##accuracy
summary(master)

##factor the dv
master$diag = factor(master$diag,
                     levels = c(1:3),
                     labels = c("Anorexia", "Both", "Bulimia"))

##ratio of cases
summary(master)

##aditivity
correl = cor(master[ , -9])
symnum(correl)
correl

##use the multinomial logit package
#install.packages("mlogit")
library(mlogit)

##restructure to specific format
longdata = mlogit.data(master, 
                       choice = "diag", shape = "wide")

##run the log!
model = mlogit(diag ~ 1 | weight + fast + binge + vomit + 
                 purge + hyper + preo + body,
               data = longdata, reflevel = "Anorexia")
summary(model)

##look at correct classifications
##look at percent correct
correct = model$probabilities
binarycorrect = colnames(correct)[apply(correct,1,which.max)]
table(master$diag, binarycorrect)

##percents
92 / (92 + 3 + 2) * 100
8 / (8 + 11 + 17) * 100
29 / (29 + 22 + 5) * 100

(92 + 8 + 29) / nrow(master) * 100

##a cool dot plot
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              text = element_text(size=20), 
              legend.key = element_blank())

##here pick a category to reference by 
##changing what's after the $ in correct
correct = as.data.frame(correct)
hist = ggplot(master, aes(correct$Anorexia, color = diag, fill = diag))
hist +
  cleanup +
  geom_dotplot(binwidth = .01, position = "jitter") +
  coord_cartesian(xlim = c(0,1)) +
  xlab("Likelihood of Anorexia") +
  ylab("Frequency") +
  scale_color_manual(values = c("Blue", "Green", "Red"),
                     labels = c("Anorexia", "Both", "Bulimia"),
                     name = "Diagnosis") +
  scale_fill_manual(values = c("Blue", "Green", "Red"),
                    labels = c("Anorexia", "Both", "Bulimia"),
                    name = "Diagnosis")
