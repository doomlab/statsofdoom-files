#Make a vector of numbers. 
numbers = rep(1,4)
numbers2 = seq(1,20,.5)
numbers3 = c(1,2,3,4,5)
numbers4 = 1:5

##length function
length(numbers)

##matrix
mymatrix = matrix(1:25, 5,5,byrow=TRUE)
mymatrix[2,3]
#mymatrix[3,2]

##datasets
data("USArrests")
head(USArrests)
summary(USArrests)

##import data
data <- read.csv("~/e_files/TEACHING/751 SEM/class assignments/R/a intro R.csv")
head(data)
ls(data)

summary(data$whichhand)
cheese = data[data$whichhand == "left", "whichhand"]
data[data$whichhand == "left", "whichhand"] = "Left"
data$whichhand = as.character(data$whichhand)
data[data$whichhand == "Left", "whichhand"] = "LEFT"
data[na.omit(data$whichhand) == "Left", "whichhand"] = "LEFT"
summary(data)
nomissing = na.omit(data)

lefties = subset(nomissing, whichhand == "LEFT")
lefties2 = nomissing[nomissing$whichhand == "LEFT", ]

#correlation
cor(lefties[ , c("LR_switch","finger_switch","rha", "word_length", "letter_freq")], use = "pairwise.complete.obs")

##bonus - subsetting a list
regression = lm(rating~finger_switch, data=lefties)
regression$coefficients[2]
regression$coefficients["finger_switch"]
