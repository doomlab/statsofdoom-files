##make sure you check the header part of the import process
evals <- read.csv("~/OneDrive - Missouri State University/TEACHING/751 SEM/class examples/a path/1 evals.csv")

###load library
library(lavaan)
library(semPlot)

##specify the model
model = '
#c ~ a + b
q4 ~ q12 + q2
#d ~ c + a
q1 ~ q4 + q12
'

#run the model
path = sem(model, data=evals)

#view the output
summary(path)
semPaths(path, whatLabels="par", layout="spring")
