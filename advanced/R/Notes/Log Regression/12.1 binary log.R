##dr b's working directory, yours will be different
setwd("~/e_files/TEACHING/527 Adv Statistics/2016 - R/1 notes and examples/8 log regression")

##import the file
master = read.csv("binary log.csv")

##accuracy
summary(master)

##aditivity
correl = cor(master[ , c(3:9)])
symnum(correl)

##run the log!
model = glm(WORKSTAT ~ CHILDREN + RACE + CONTROL +
              ATTMAR + ATTROLE + SEL + ATTHOUSE +
              AGE + EDUC,
       family = binomial(link = 'logit'),
       data = master)
summary(model)

##overall model
options(scipen = 999)
chidiff = model$null.deviance - model$deviance
dfdiff = model$df.null - model$df.residual
chidiff
dfdiff
pchisq(chidiff, dfdiff, lower.tail = F)

##get r2
library(BaylorEdPsych)
PseudoR2(model)

table(master$WORKSTAT)

##look at percent correct
correct = model$fitted.values
binarycorrect = ifelse(correct > 0.5,1,0)
binarycorrect = factor(binarycorrect,
                       levels = c(0,1),
                       labels = c("Not working pred", "Working pred"))
table(master$WORKSTAT, binarycorrect)

##accuracy for each group
132 / (132 + 87) * 100
165 / (81 + 165) * 100
(132 + 165) / nrow(master) * 100

##a cool dot plot
library(ggplot2)
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              text = element_text(size=20), 
              legend.key = element_blank())

hist = ggplot(master, aes(correct, color = WORKSTAT, fill = WORKSTAT))
hist +
  theme +
  geom_dotplot(binwidth = .01, position = "jitter") +
  coord_cartesian(xlim = c(0,1)) +
  xlab("All Predictors in Model") +
  ylab("Frequency") +
  scale_color_manual(values = c("Maroon", "#2C3539"),
                     labels = c("Not Working", "Working"),
                     name = "Working Category")+
  scale_fill_manual(values = c("Maroon", "#2C3539"),
                    labels = c("Not Working", "Working"),
                    name = "Working Category") + 
  geom_vline(xintercept=c(.50), linetype="dotdash", size = 1)

