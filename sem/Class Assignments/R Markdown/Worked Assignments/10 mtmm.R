library(lavaan)
library(semPlot)

##load the data
setwd("~/OneDrive - Harrisburg University/Teaching/MSU Courses/751 SEM/class assignments/R Markdown")
data <- read.csv("10 mtmm.csv")

##traits and methods cfa
model.traits = '
meaning =~ m1 + m2 + m5 + m10 + s1 + s9 + p4 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + s2 + s8 + p3 + p20
'

model.methods = '
mlq =~ m1 + m2 + m5 + m10 + m3 + m4 + m6 + m8 + m9
song =~ s1 + s9 + s2 + s8
pil =~ p4 + p17 + p3 + p20
'

##model 1
model1 = '
meaning =~ m1 + m2 + m5 + m10 + s1 + s9 + p4 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + s2 + s8 + p3 + p20
mlq =~ m1 + m2 + m5 + m10 + m3 + m4 + m6 + m8 + m9
song =~ s1 + s9 + s2 + s8
pil =~ p4 + p17 + p3 + p20

##cross covariances to zero
meaning ~~ 0*mlq
meaning ~~ 0*song
meaning ~~ 0*pil
purpose ~~ 0*mlq
purpose ~~ 0*song
purpose ~~ 0*pil
'

##model 2
model2 = '
mlq =~ m1 + m2 + m5 + m10 + m3 + m4 + m6 + m8 + m9
song =~ s1 + s9 + s2 + s8
pil =~ p4 + p17 + p3 + p20
'

##model 3
model3 = '
meaning =~ m1 + m2 + m5 + m10 + s1 + s9 + p4 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + s2 + s8 + p3 + p20
mlq =~ m1 + m2 + m5 + m10 + m3 + m4 + m6 + m8 + m9
song =~ s1 + s9 + s2 + s8
pil =~ p4 + p17 + p3 + p20

##cross covariances to zero
meaning ~~ 0*mlq
meaning ~~ 0*song
meaning ~~ 0*pil
purpose ~~ 0*mlq
purpose ~~ 0*song
purpose ~~ 0*pil

##perfect traits
meaning ~~ 1*purpose
'

##model 4
model4 = '
meaning =~ m1 + m2 + m5 + m10 + s1 + s9 + p4 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + s2 + s8 + p3 + p20
mlq =~ m1 + m2 + m5 + m10 + m3 + m4 + m6 + m8 + m9
song =~ s1 + s9 + s2 + s8
pil =~ p4 + p17 + p3 + p20

##cross covariances to zero
meaning ~~ 0*mlq
meaning ~~ 0*song
meaning ~~ 0*pil
purpose ~~ 0*mlq
purpose ~~ 0*song
purpose ~~ 0*pil

##methods covariances to zero
mlq ~~ 0*song
mlq ~~ 0*pil
song ~~ 0*pil
'

###test the models
traits.fit = cfa(model.traits, data=data, std.lv =TRUE)
summary(traits.fit, rsquare=TRUE, standardized =TRUE)
semPaths(traits.fit, whatLabels = "std", layout = "tree")

methods.fit = cfa(model.methods, data=data, std.lv=TRUE)
summary(methods.fit, rsquare=TRUE, standardized = TRUE)
semPaths(methods.fit, whatLabels = "std", layout="tree")

model1.fit = cfa(model1, data=data, std.lv=TRUE)
summary(model1.fit, rsquare=TRUE, standardized = TRUE)
fitmeasures(model1.fit)
semPaths(model1.fit, whatLabels = "std", layout="tree")

model2.fit = cfa(model2, data=data, std.lv=TRUE)
summary(model2.fit, rsquare=TRUE, standardized = TRUE)
fitmeasures(model2.fit)
semPaths(model2.fit, whatLabels = "std", layout="tree")

model3.fit = cfa(model3, data=data, std.lv=TRUE)
summary(model3.fit, rsquare=TRUE, standardized = TRUE)
fitmeasures(model3.fit)
semPaths(model3.fit, whatLabels = "std", layout="tree")

model4.fit = cfa(model4, data=data, std.lv=TRUE)
summary(model4.fit, rsquare=TRUE, standardized = TRUE)
fitmeasures(model4.fit)
semPaths(model4.fit, whatLabels = "std", layout="tree")
