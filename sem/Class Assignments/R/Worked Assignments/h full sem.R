##enter the data
data.cov = lav_matrix_lower2full(c(169.00, 
                                   73.710,	182.2500,
                                   73.229,	88.4250,	171.6100,
                                   63.375,	72.5625,	127.7250,	156.2500,
                                   42.120,	67.4325,	122.0265,	123.1875,	182.2500,
                                   57.226,	63.2610,	117.1926,	154.4250,	138.0240,	201.6400,
                                   30.875,	32.0625,	60.9805,	62.9375,	76.9500,	79.5910,	90.2500,
                                   36.075,	38.9610,	61.0722,	58.2750,	65.9340,	70.9290,	81.1965,	123.2100,
                                   18.096,	21.1410,	26.2131,	39.1500,	44.6310,	46.9452,	48.7635,	56.0106,	75.6900))
rownames(data.cov) = colnames(data.cov) = c("class", "social", "learn", "chronic", 
                                            "physical", "sex", "dep", "anx", "stress")

##build 
model = '
uni <~ class + social + learn
health =~ chronic + physical + sex
psyc =~ dep + anx + stress
uni=~psyc + health
'

##test the model
model.fit = sem(model, sample.cov = data.cov, sample.nobs = 300)
summary(model.fit, rsquare=TRUE, standardized=TRUE)
semPaths(model.fit, whatLabels = "std", layout="spring")



