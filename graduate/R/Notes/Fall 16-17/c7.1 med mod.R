##install packages
install.packages(c("haven", "boot", "ggplot2"))

##set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 16")

#import the data
library(haven)
master = read_spss("c7.1 mediation.sav")

#data screening would go here 
summary(master)
master = na.omit(master) ##just because we are skipping data screening for lecture 

#path c
model1 = lm(exam ~ previous, data = master)
summary(model1)

#path a
model2 = lm(facebook ~ previous, data = master)
summary(model2)

#path b, c'
#put in X then M
model3 = lm(exam ~ previous + facebook, data = master)
summary(model3)

#aroian sobel
a = coef(model2)[2]
b = coef(model3)[3]
SEa = summary(model2)$coefficients[2,2]
SEb = summary(model3)$coefficients[3,2]
zscore = (a*b)/(sqrt((b^2*SEa^2)+(a^2*SEb^2)+(SEa*SEb)))
zscore
pnorm(abs(zscore), lower.tail = F)*2

#confidence intervals
total = coef(model1)[2] #cpath
direct = coef(model3)[2] #c' path
indirect = a*b

total; direct; indirect

####Bootstrapping the Mediation Effect####
##write a function that gives you the numbers you want
##we want the indirect effect
##do not change this part 
indirectsaved = function(formula2, formula3, dataset, random) {
  d = dataset[random, ] #randomize by row
  model2 = lm(formula2, data = d)
  model3 = lm(formula3, data = d)
  a = coef(model2)[2]
  b = coef(model3)[3]
  indirect = a*b
  return(indirect)
}

library(boot)

bootresults = boot(data = master,
                   statistic = indirectsaved,
                   formula2 = facebook ~ previous,
                   formula3 = exam ~ previous + facebook,
                   R = 1000)

bootresults
boot.ci(bootresults,
        conf = .95,
        type = "norm")

####moderation####

#import the data
library(haven)
master = read_spss("c7.1 moderation.sav")

#data screening would go here 
summary(master)

#center the X and M variable (NOT THE DV)
#when you create these use the final dataset (like no out)
master$zvid = scale(master$Vid_Games, scale = F) #mean center, not z score
master$zcal = scale(master$CaUnTs, scale = F)

#run the model to see if the moderation is significant
#use the z score variables!
#be sure to do X*M so the graph is right 
modmodel = lm(Aggression ~ zvid*zcal, data = master)
summary(modmodel)

#create the low and high z score variables 
master$zcallow = master$zcal + sd(master$zcal) #bring them up
master$zcalhigh = master$zcal - sd(master$zcal) #bring them down
summary(master)

#run the models
#be sure to X*M
modmodellow = lm(Aggression ~ zvid*zcallow, data = master)
modmodelhigh = lm(Aggression ~ zvid*zcalhigh, data = master)

#look at all three summaries
summary(modmodellow) #low slope
summary(modmodel) #average slope
summary(modmodelhigh) #high slope

####graph####
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

#use the z score variables
modgraph = ggplot(master, aes(zvid, Aggression))
modgraph + 
  xlab("Centered Video Games") + 
  geom_point(color = "gray") +
  
  ##this part here assumes you named the models the same as above
  ##and you did X*M where M is the low medium high variable 
  ##change the labels for the slopes in BOTH places 
  
  geom_abline(aes(intercept = modmodellow$coefficients[1],
                  slope = modmodellow$coefficients[2], 
                  linetype = "-1SD Cal"), size = 1) +
  geom_abline(aes(intercept = modmodel$coefficients[1],
                  slope = modmodel$coefficients[2], 
                  linetype = "Average Cal"), size = 1) +
  geom_abline(aes(intercept = modmodelhigh$coefficients[1],
                  slope = modmodelhigh$coefficients[2], 
                  linetype = "+1SD Cal"), size = 1) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD Cal", "Average Cal", "+1SD Cal"),
                        name = "Simple Slope") +
  cleanup 

  