library(memisc)
anovadata = as.data.set(spss.system.file("anova example.sav"))
anovadata = as.data.frame(anovadata)
anovadata = na.omit(anovadata)
anovadata$q2 = as.factor(anovadata$q2)
anovadata$q6 = as.factor(anovadata$q6)

##run with aov
output = aov(verbs ~ q2+q6+q2*q6, data = anovadata)
summary(output)

output2 = aov(verbs ~ q6+q2+q6*q2, data = anovadata)
summary(output2)

library(car)
Anova(output, type = "3")

options(contrasts = c("contr.sum", "contr.poly"))
output = aov(verbs ~ q2+q6+q2*q6, data = anovadata)
summary(output)
Anova(output, type = "3")

anovadata$partno = c(1:nrow(anovadata))
options(scipen = 999)
library(ez)
ezANOVA(data = anovadata,
        wid = partno, 
        dv = verbs,
        between = .(q2, q6),
        detailed = TRUE,
        type = 3)
