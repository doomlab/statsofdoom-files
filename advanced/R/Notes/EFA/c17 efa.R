##install the things you don't have
install.packages("car")
install.packages("psych")
install.packages("GPArotation")

##working directory
setwd("~/e_files/TEACHING/745 Grad Statistics/notes/fall 15")

##load the data
efadata = read.csv("c17 efa raq.csv")

##libraries
library(psych)
library(GPArotation)
library(car)

##be sure to reverse score your data
##you will have to do each item one at a time
table(efadata$Q03)
efadata$Q03 = recode(efadata$Q03, "1=5; 2=4; 3=3; 4=2; 5=1")
table(efadata$Q03)

##normal data screening (fake style) goes here
##screen all the items (but not demographics)

##correlation adequacy Bartlett's test
correlations = cor(efadata)
cortest.bartlett(correlations, n = nrow(efadata))

##sampling adequacy KMO test
KMO(correlations)

##how many factors?
nofactors = fa.parallel(efadata, fm="ml", fa="fa")
nofactors$fa.values
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a two factor model
fa(efadata, nfactors=2, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(23)], nfactors=2, rotate = "oblimin", fm = "ml")

##get cfi
finalmodel = fa(efadata[ , -c(23)], nfactors=2, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

##reliability
factor1 = c(1:7, 9:10, 12:16, 18:22)
factor2 = c(8, 11, 17)
alpha(efadata[, factor1])
alpha(efadata[, factor2])

##simple structure with a five factor model
##an example of OVERfactoring
fa(efadata, nfactors=5, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(15)], nfactors=5, rotate = "oblimin", fm = "ml")
##all items load but factor five only has two items
##try four factor model
fa(efadata[ , -c(15)], nfactors=4, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(3,14,15)], nfactors=4, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(3,7,10,14,15,18)], nfactors=4, rotate = "oblimin", fm = "ml")
##at this point you would get rid of 12, and then factor four
##only has two items ... this pattern indicates that you should 
##try the smaller numbers of factors

