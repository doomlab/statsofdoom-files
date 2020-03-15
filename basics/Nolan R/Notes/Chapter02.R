##load airquality dataset
data(airquality)
head(airquality)

##frequency table
table(Temp) ##incorrect
table(airquality$Temp)

##tricky length function
length(airquality$Temp)
length(airquality)

##percent frequency tables
table(airquality$Temp) / length(airquality$Temp) * 100

##grouped frequency stem and leaf
stem(airquality$Temp, scale = .2)

##install package
install.packages("ggplot2")

##load the package / library
library(ggplot2)

##basic histogram
myplot = ggplot(airquality, aes(Temp))
myplot + geom_histogram()
myplot + geom_histogram(binwidth = 5)

##frequency polygon
myplot + geom_freqpoly(binwidth = 5)
