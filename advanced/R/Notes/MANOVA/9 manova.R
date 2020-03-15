##set working directory yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/9 MANOVA")

##import file
master = read.csv("9 manova.csv")

##drop the columns you don't need
master = master[ , c(1:4, 6, 9)]

##accuracy
summary(master)

##missing
percentmissing = function (x){ sum(is.na(x))/length(x) * 100}
missing = apply(master[ , -1], 1, percentmissing)
table(missing)

##exclude the missing data
nomiss = subset(master, missing <= 5)

##outliers
cutoff = qchisq(1-.001, ncol(nomiss[ , -c(1:3)]))
mahal = mahalanobis(nomiss[ , -c(1:3)],
                    colMeans(nomiss[ , -c(1:3)]),
                    cov(nomiss[ , -c(1:3)]))
cutoff ##cutoff score
ncol(nomiss[ , -c(1:3)]) ##df
summary(mahal < cutoff)

##exclude outliers
noout = subset(nomiss, mahal < cutoff)

##additivity
correl = cor(noout[ , -c(1:3)], use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
random = rchisq(nrow(noout), 7)
fake = lm(random~., data = noout[ , -1])
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##levene's test
library(car)
leveneTest(ESTEEM ~ FEM*MASC,
           data = noout, center = mean)
leveneTest(ATTROLE ~ FEM*MASC,
           data = noout, center = mean)
leveneTest(NEUROTIC ~ FEM*MASC,
           data = noout, center = mean)

##run the manova
DV = cbind(noout$ESTEEM, noout$ATTROLE, noout$NEUROTIC)
output = lm(DV ~ FEM*MASC, data = noout,
            contrasts=list(FEM=contr.sum, MASC=contr.sum))
manova_out = Manova(output, type = "III")
summary(manova_out, multivariate = T)

##run the anovas look at the significant effects only
library(ez)
ezANOVA(data = noout,
        dv = ESTEEM,
        wid = CASENO,
        between = .(FEM, MASC), 
        type = 3)

ezANOVA(data = noout,
        dv = ATTROLE,
        wid = CASENO,
        between = .(FEM, MASC), 
        type = 3)

ezANOVA(data = noout,
        dv = NEUROTIC,
        wid = CASENO,
        between = .(FEM, MASC), 
        type = 3)

##get the means
tapply(noout$ESTEEM, list(noout$FEM), mean)
tapply(noout$ESTEEM, list(noout$FEM), sd)

tapply(noout$ESTEEM, list(noout$MASC), mean)
tapply(noout$ESTEEM, list(noout$MASC), sd)

tapply(noout$ATTROLE, list(noout$FEM), mean)
tapply(noout$ATTROLE, list(noout$FEM), sd)

tapply(noout$ATTROLE, list(noout$MASC), mean)
tapply(noout$ATTROLE, list(noout$MASC), sd)

tapply(noout$NEUROTIC, list(noout$FEM), mean)
tapply(noout$NEUROTIC, list(noout$FEM), sd)

tapply(noout$NEUROTIC, list(noout$MASC), mean)
tapply(noout$NEUROTIC, list(noout$MASC), sd)

##refectored low and high, so that low was first
table(noout$FEM)
noout$FEM = factor(noout$FEM,
                   levels = c("Low", "High"))
noout$MASC = factor(noout$MASC,
                   levels = c("Low", "High"))

##graphs esteem
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"), 
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

bargraph = ggplot(noout, aes(FEM, ESTEEM, fill = MASC))
bargraph +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  cleanup +
  xlab("Femininity") +
  ylab("Self-Esteem") + 
  scale_fill_manual(name="Masculinity", 
                    values = c("Light Gray", "Dark Gray"))

bargraph2 = ggplot(noout, aes(FEM, ATTROLE, fill = MASC))
bargraph2 +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  cleanup +
  xlab("Feminiity") +
  ylab("Attitudes Toward Women") + 
  scale_fill_manual(name="Masculinity", 
                    values = c("Light Gray", "Dark Gray"))

bargraph3 = ggplot(noout, aes(FEM, NEUROTIC, fill = MASC))
bargraph3 +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position="dodge") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) + 
  cleanup +
  xlab("Feminiity") +
  ylab("Neuroticism") + 
  scale_fill_manual(name="Masculinity", 
                    values = c("Light Gray", "Dark Gray"))
