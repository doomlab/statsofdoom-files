##Create an .R script file.
##Make a vector of numbers. 

##numeric vectors
vector1 = rep(5, 10)
vector2 = seq(0, 20, 1)
vector3 = c(3,4,5,2,3,4,5,9,0,1)
vector4 = c(rep(5,10), seq(0,20,1), c(4,3,4,5,6,7))

##length
length(vector1)
length(vector2)

##matrix
mymatrix = matrix(1:25, 5, 5, byrow = T)
length(mymatrix)
mymatrix[2,3]
##mymatrix[3,2] incorrect order

##data frames
data(USArrests)
head(USArrests)
length(USArrests)
summary(USArrests)

##working directory way
setwd("~/OneDrive - Missouri State University/TEACHING/751 SEM/class assignments/R")
mydata = read.csv("1 Intro R.csv")

##import dataset way
mydataset <- read.csv("~/OneDrive - Missouri State University/TEACHING/751 SEM/class assignments/R/1 intro R.csv")

##recode values
table(mydataset$whichhand)
mydataset2 = mydataset

mydataset2$whichhand = factor(mydataset2$whichhand,
                              levels = c("","left","Left",
                                         "right","Right","RIGHT",          
                                         "right ","Right ","right hand",    
                                         "right_left","Right.","with Cap letter"),
                              labels = c(NA, "LEFT", "LEFT",
                                         "RIGHT", "RIGHT", "RIGHT",
                                         "RIGHT", "RIGHT", "RIGHT", 
                                         "Ambedextrious", "RIGHT", NA))
levels(mydataset2$whichhand)
table(mydataset$whichhand)
table(mydataset2$whichhand)
mydataset2$whichhand = droplevels(mydataset2$whichhand)
summary(mydataset2$whichhand)

summary(mydataset2)

nomissing = na.omit(mydataset2)

lefties = subset(mydataset2, whichhand == "LEFT")
lefties2 = mydataset2[mydataset2$whichhand == "LEFT", ]

cor(lefties[ , c("LR_switch",
                 "finger_switch",
                 "rha",
                 "word_length",
                 "letter_freq")], 
    use = "pairwise.complete.obs")

names(lefties)
cor(lefties[ , 8:12 ], use = "pairwise.complete.obs")

##bonus
output = lm(rating~finger_switch, data=lefties)
output$coefficients ##both things
coef(output)
output$coefficients$finger_switch ##doesn't work 
output$coefficients["finger_switch"]
output$coefficients[2]
