#####CSTAT Workshop###############
#####Exercise 1###################
#####Estimate mean and SD#########
#####File provided by instructor##
##################################

#load libraries
library(rstan)
#Set working directory to source file locations
#This directory must have all data files and Stan model code needed.

#clear workspace
rm(list=ls())

#set random number seed for consistent data
set.seed(14568) 
#Generate data
#Known paramters
mu = 10
sd = 2

#sample size
nobs = 500

#Generate random data
y = rnorm(n = nobs, mean = mu, sd = sd)
hist(y)

#Package data, initialization values, and send to Stan
#Data in dataList(can use any name you want) must match values in Stan data block

#specify number of chains, used to initialize values and specify chains
nchains = 4

dataList = list(
  n=nobs,
  y=y
)

#Initialize values
#convergence can be improved by setting reasonable starting values
#i.e, range of observations from 1-20, don't intialize mean at 100000
#Use different starting values for each chain
initslst <- lapply(1:nchains,function(i) {
  list(
    sigma=runif(1,1,10),
    mu=runif(1,min(y),max(y))
  )
})

#send everything to Stan
fit1 <- stan(file = 'Ex1_est_mean_sd.stan',
             data = dataList , 
             init = initslst,
             chains = nchains,
             iter = 1000 ,
             warmup = 500 , 
             thin = 1 )

#View traceplots
traceplot(fit1)
#view results
fit1

#extract results
est_mean=rstan::extract(fit1,"mu")$mu
est_sd=rstan::extract(fit1,"sigma")$sigma

#plot results
par(mfrow=c(1,2))
hist(est_mean,breaks=50);abline(v=mu,lwd=5);
hist(est_sd,breaks=50);abline(v=sd,lwd=5);
par(mfrow=c(1,1))

#Check Rhat and n_eff
#Rhat determines convergence, if all chains are exploring the same regions Rhat<1.1
#n_eff, if N_eff / N < 0.001 then convergence is suspect


