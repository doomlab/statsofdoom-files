##load the data
cheese <- read.csv("~/e_files/TEACHING/745 Grad Statistics/notes/fall 15/c2.1 cheese.csv")
summary(cheese)
##subset the data
blahs = subset(cheese, feelings == "blahs")
summary(blahs)
yummies = subset(cheese, feelings == "yummies")
summary(yummies)

##tapply
tapply(cheese$cheese, cheese$feelings, sd)
tapply(cheese$cheese, cheese$feelings, mean)
second = c(rep(1:2, 50))
tapply(cheese$cheese, list(cheese$feelings, second), mean)
tapply(cheese$cheese, cheese$feelings, length)

stdev = tapply(cheese$cheese, cheese$feelings, sd) 
N = tapply(cheese$cheese, cheese$feelings, length)
se = sd / sqrt(N)

M = tapply(cheese$cheese, cheese$feelings, mean)
M + 1.96*(se)
M - 1.96*(se)
