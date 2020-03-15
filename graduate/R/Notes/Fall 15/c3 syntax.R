##Dr. B's computer
##setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 15")

install.packages("foreign")
install.packages("memisc")
install.packages("reshape")
library(foreign)
library(memisc)
library(reshape)

##read in a csv file
catterplot = read.csv(file.choose(), header=TRUE)

##read in an SPSS files with foreign
chick = read.spss(file.choose(), to.data.frame=TRUE)
##or the real coding method note that I set my working directory
##at the beginning so it would know where to find this file
chick = read.spss("c4 ChickFlick.sav", to.data.frame=TRUE)
summary(chick)

##read in an SPSS file with memisc
chick2 = as.data.set(spss.system.file(file.choose()))
chick2 = as.data.set(spss.system.file("c4 ChickFlick.sav"))
##note sometimes using a data.set doesn't work with an analysis
##so you change it over to a data.frame easily
chick2 = as.data.frame(chick2)

##simply add a new column
chick2$newname = chick2$gender
##rename the column replace the number with the offending column number
##generally this will be the first column with bad imports
colnames(chick2)[1] = "gender2"

##make some fake data
notfactor = rep(1:3, 50)
factored = factor(notfactor, levels=c(1,2,3), labels = c("swiss", "feta", "gouda"))

##reshaping data
##Load some data
cricket = read.csv("c4 Jiminy Cricket.csv", header=TRUE)
summary(cricket)

##wide to long
longcricket = melt(cricket, id = c("ID", "Strategy"), 
                   measured = c("Success_Pre", "Succcess_Post"))

##long to wide
widecricket = cast(longcricket, ID + Strategy ~ variable, value = "value")
