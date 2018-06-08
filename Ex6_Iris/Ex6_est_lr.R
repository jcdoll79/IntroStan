#####CSTAT Workshop###############
#####Exercise 5###################
#####Analysis of Iris data########
#####File provided by instructor##
##################################

#install/load the rstan package
require(rstan)

#Set working directory to source file locations
#In Rstudio 
#This directory must have all data files and Stan model code needed.
#If you receive an error, manually set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#clear workspace
rm(list=ls())

#Generate simulated data
#Load Iris data

iris = iris

#specify number of chains, used to initialize values and specify chains
nchains = 3

# Specify data:
dataList = list(
  'n'=nrow(iris),
  'x'=iris$Sepal.Length,
  'y'=iris$Sepal.Width
)

#Initialize values
#convergence can be improved by setting reasonable starting values
#i.e, range of observations from 1-20, don't intialize mean at 100000
#Use different starting values for each chain
initslst <- lapply(1:nchains,function(i) {
  list(
    alpha = rnorm(1,0,1),
    beta = rnorm(1,0,1),
    sigma=runif(1,1,10)
  )
})

#send everything to Stan
fit2 <- stan(file = 'Ex5_est_lr.stan',
             data = dataList , 
             init = initslst,
             chains = nchains,
             iter = 1000 ,
             warmup = 500 , 
             thin = 1 )

#View traceplots
traceplot(fit2)
#view results
fit2

#extract results
est_alpha=rstan::extract(fit2,"alpha")$alpha
est_beta=rstan::extract(fit2,"beta")$beta
est_sd=rstan::extract(fit2,"sigma")$sigma

#plot results
par(mfrow=c(1,3))
hist(est_alpha,breaks=20);
hist(est_beta,breaks=20);
hist(est_sd,breaks=20);
par(mfrow=c(1,1))

#Check Rhat and n_eff
#Rhat determines convergence, if all chains are exploring the same regions Rhat<1.1
#n_eff, if N_eff / N < 0.001 then convergence is suspect

