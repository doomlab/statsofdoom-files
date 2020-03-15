data <- read.csv("~/e_files/TEACHING/751 SEM/class assignments/R/b efa.csv")

##data screen
nomissing = na.omit(data)
final = nomissing[,-ncol(nomissing)]

##load packages
library(psych)
library(GPArotation)

##efa
##number of factors
##parallel analysis
parallel = fa.parallel(final, fm="ml", fa="fa")
#scree is two
#parallel is three
#eigenvalues (kaiser)
parallel$fa.values
##over 1 = 2
##over .7 = 3

#simple structure
twofactor = fa(final, nfactors=2, rotate = "oblimin", fm="ml")
twofactor2 = fa(final[,-c(3,11,15,16)], nfactors=2, rotate = "oblimin", fm="ml")

##adequate ? does it work?
#rmsr= .08 good
#rmsea = .094 adequate
#tli = .83 :( not good
#cfi = .88 not good

1 - ((twofactor2$STATISTIC - twofactor2$dof)/(twofactor2$null.chisq - twofactor2$null.dof))

##reliability
alpha(final[ , c(1, 2, 12, 14, 18)])
alpha(final[ , c(4, 5, 6, 7, 8, 9, 10, 13, 17, 19, 20)])
##.85 factor 1
##.82 factor 2

##name the factors
#factor 1 about the arts
#factor 2 thinking philosopy 
  