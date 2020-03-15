##set working directory
##yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 16")

longdata = read.csv("c9 invisible long.csv", header = T)

##calculate means and sds for descriptive statistics
##to use tapply you would need data in long format (either between or within)
##use melt to convert to long format if you need to
M = tapply(longdata$Mischief, longdata$Cloak, mean)
stdev = tapply(longdata$Mischief, longdata$Cloak, sd)
N = tapply(longdata$Mischief, longdata$Cloak, length)

M;stdev;N

####independent t ####
#Y ~ X or DV ~ IV group
t.test(Mischief ~ Cloak, 
       data = longdata, 
       var.equal = TRUE, 
       paired = FALSE)


##effect size
library(MOTE)
effect = d.ind.t(m1 = M[1], m2 = M[2],
        sd1 = stdev[1], sd2 = stdev[2],
        n1 = N[1], n2 = N[2], a = .05)

effect$d

##power
library(pwr)
pwr.t.test(n = NULL, d = effect$d, sig.level = .05,
           power = .80, type = "two.sample", alternative = "two.sided")

####dependent t####
##note note note this code requires the participants be in order 
##note note note if you melt data that will be true as long as you don't eliminate NAs
##therefore, datascreen in wide format, melt at the point you are doing the analysis
##there are lots of reasons here, do not melt then datascreen, that violates independence
t.test(Mischief ~ Cloak, 
       data = longdata, 
       var.equal = TRUE, 
       paired = TRUE)

##dept averages
##you need M, sd of each level, remember N is the same people
M
stdev
N

effect2 = d.dep.t.avg(m1 = M[1], m2 = M[2],
                 sd1 = stdev[1], sd2 = stdev[2],
                 n = N[1], a = .05)

effect2$d

##dept t differences
##create the mdiff and sddiff scores (melted data is a bit trickier)
table(longdata$Cloak)
cloak = subset(longdata, Cloak == "Cloak")
nocloak = subset(longdata, Cloak == "No Cloak")
diff = cloak$Mischief - nocloak$Mischief
mdiff = mean(diff, na.rm = T)
sddiff = sd(diff, na.rm = T)
N2 = length(diff)

mdiff; sddiff; N2

effect2.1 = d.dep.t.diff(mdiff = mdiff, sddiff = sddiff,
                         n = N2, a = .05)

effect2.1$d

pwr.t.test(n = NULL, d = effect2$d, sig.level = .05,
           power = .80, type = "paired", alternative = "two.sided")

####chart####
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

bargraph = ggplot(longdata, aes(Cloak, Mischief))
bargraph +
  cleanup +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2, 
               position = "dodge") +
  xlab("Invisible Cloak Group") +
  ylab("Average Mischief Acts")
