
#Priors for model of mean
hist(rnorm(10000000,0,100),main="Prior for mu",xlab="Value",breaks=100)

hist(rnorm(10000000,0,100),main="Prior for mu",xlab="Value",xlim=c(-50,50),breaks=100)

sdvals=rcauchy(10000000,0,5)
sdvals=subset(sdvals,sdvals>0)
hist(sdvals,main="Prior for sigma",xlab="Value",xlim=c(0,1E3),breaks=1000000)

