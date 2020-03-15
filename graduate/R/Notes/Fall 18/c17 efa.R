##install the things you don't have
install.packages("psych")
install.packages("GPArotation")

##working directory
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 16")

##load the data
master = read.csv("c17 efa raq.csv")

##libraries
library(psych)
library(GPArotation)

##be sure to reverse score your data
table(master$Q03)
##put in the columns you want to reverse, such as Q3
##formula is MAX value of scale + 1 - original scale values
##this is max scale possible, not the max in the data set
master[ , 3] = 6 - master[ , 3]
table(master$Q03)

##normal data screening (fake style) goes here
##screen all the items (but not demographics)

##correlation adequacy Bartlett's test
correlations = cor(master)
cortest.bartlett(correlations, n = nrow(master))

##sampling adequacy KMO test
KMO(correlations)

##how many factors?
nofactors = fa.parallel(master, fm="ml", fa="fa")
nofactors$fa.values
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a two factor model
fa(master, nfactors=2, rotate = "oblimin", fm = "ml")
fa(master[ , -c(23)], nfactors=2, rotate = "oblimin", fm = "ml")

##get cfi
finalmodel = fa(master[ , -c(23)], nfactors=2, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

##reliability
factor1 = c(1:7, 9:10, 12:16, 18:22)
factor2 = c(8, 11, 17)
##we use the psych::alpha to make sure that R knows we want the alpha function from the psych package.
##ggplot2 has an alpha function and if we have them both open at the same time
##you will sometimes get a color error without this :: information. 
psych::alpha(master[, factor1], check.keys = T)
psych::alpha(master[, factor2], check.keys = T)

##simple structure with a five factor model
##an example of OVERfactoring
fa(master, nfactors=5, rotate = "oblimin", fm = "ml")
fa(master[ , -c(15)], nfactors=5, rotate = "oblimin", fm = "ml")
##all items load but factor five only has two items
##try four factor model
fa(master[ , -c(15)], nfactors=4, rotate = "oblimin", fm = "ml")
fa(master[ , -c(3,14,15)], nfactors=4, rotate = "oblimin", fm = "ml")
fa(master[ , -c(3,7,10,14,15,18)], nfactors=4, rotate = "oblimin", fm = "ml")
##at this point you would get rid of 12, and then factor four
##only has two items ... this pattern indicates that you should 
##try the smaller numbers of factors

