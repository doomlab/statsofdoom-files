mycolumn = c(8,11,19,10,22,13,16,12,9,15,8,
             14,12,14,15,15,16,8,15,11,15,14,19)
##get the mean
summary(mycolumn)
mean(mycolumn, na.rm = T)

##median
median(mycolumn, na.rm = T)

##mode
temp <- table(as.vector(mycolumn))
names(temp)[temp == max(temp)]

##outlier example
outlier = c(mycolumn, 200)
summary(outlier)
temp <- table(as.vector(outlier))
names(temp)[temp == max(temp)]

##range
max(mycolumn, na.rm = T) - min(mycolumn, na.rm = T)

##variance
var(mycolumn, na.rm = T)

##standard deviation
sd(mycolumn, na.rm = T)

##population formula
pop.var <- function(x) var(x) * (length(x)-1) / length(x) 
pop.sd <- function(x) sqrt(pop.var(x))

pop.var(mycolumn)
pop.sd(mycolumn)

##IQR
summary(mycolumn)
IQR(mycolumn, na.rm = T)


