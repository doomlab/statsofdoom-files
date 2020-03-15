##import the data file
##you can import by clicking import dataset > follow instructions
##you can also use the handy file.choose() option
##note that window sometimes hides behind rstudio, so minimize to find it
datafile = read.csv(file.choose())
##don't run line 5 if you use the import option 

##install the packages if you don't have them
##you only have to install the package once, unless you update R 
install.packages("reshape")
install.packages("nlme")
install.packages("ggplot2")

##you will want to run this code every time you open R
##and definitely if it crashes
library(nlme)
library(reshape)
library(ggplot2)

##factor our categorical IV
table(datafile$intervention)
datafile$intervention = factor(datafile$intervention,
                               levels = c(0,1,2),
                               labels = c("None", "Type 1", "Type 2"))

##this particular data is half-wide, half-long
##each day/time has a row already, but we want to make all the DVs one column too
longdata = melt(datafile, 
                id = c("day","time","partno","intervention"),
                measured = c("thighs","sucked","stomach","compare","mirror","jiggle","spread", "weight"))

##one bad thing about melt is the naming of the columns 
##you can change them, but this code will assume you didn't
##variable = all the labels of the columns you put into measured 
##value = the actual scores from those columns (DV)

##intercept only model
##gls = generalized least squares
##ML = maximum likelihood
model1 = gls(value ~ 1, 
             data = longdata, 
             method = "ML", 
             na.action = "na.omit")
summary(model1)

##random intercept only model
##note we switched to LME function
model2 = lme(value ~ 1, 
             data = longdata, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model2)

##compare those two models
anova(model1, model2)

##if significant --> go on to test other random effects if necessary
##example adding more levels of nesting, higher order go first
model2.1 = lme(value ~ 1, 
             data = longdata, 
             method = "ML", 
             na.action = "na.omit",
             random = list(~1|time, ~1|partno))
summary(model2.1)
##if significant comparison --> go on to fixed effects
##if not --> ANOVA is A-OK. (in theory, not necessarily in practice)

##add fixed effects to your model
model3 = lme(value ~ intervention, 
             data = longdata, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model3)

##compare all the models
anova(model1, model2, model3)

##how would I add other IV variables?
model3.1 = lme(value ~ intervention + time, 
             data = longdata, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model3.1)

##random slope model
model4 = lme(value ~ intervention + time, 
             data = longdata, 
             method = "ML", 
             na.action = "na.omit",
             random = ~time|partno,
             control = lmeControl(msMaxIter = 200))
summary(model4)

##compare models
anova(model1,model2,model3,model4)

##I made a pretty graph of the data
##Please see graphs/scatterplots for how to make this picture
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line = element_line(color = "black"))

scatter = ggplot(longdata, aes(time, value, color = intervention, fill = intervention))
scatter +
  cleanup +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Time of Measurement") +
  ylab("Body Checking Behavior Count") +
  scale_fill_discrete(name = "Intervention Type") +
  scale_color_discrete(name = "Intervention Type")

scatter = ggplot(longdata, aes(time, value, color = as.factor(partno), fill = as.factor(partno)))
scatter +
  cleanup +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Time of Measurement") +
  ylab("Body Checking Behavior Count") +
  scale_fill_discrete(name = "Participant Number") +
  scale_color_discrete(name = "Participant Number")
  

