##power example

##enter numbers here
popmean = 80
popsd = 30
N = 5
alpha = .05
samplemean = 100
lower = F

##auto calculate
popse = popsd / sqrt(N)
mneed = popmean + popse*qnorm(alpha, lower.tail = lower)
z = (mneed - samplemean)/popse
mneed #mneed
z #Z difference
pnorm(z, lower.tail = lower) * 100 ##power

