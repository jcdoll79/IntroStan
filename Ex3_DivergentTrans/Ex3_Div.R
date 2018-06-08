#####CSTAT Workshop###################
####Exercise 3: Divergent transitions#
#####From: "Optimizing Stan Code######
#####for Efficiency"##################
#####File provided by instructor######
######################################

#load libraries
library(rstan)
library(shinystan)

#Set working directory to source file locations
#This directory must have all data files and Stan model code needed.

#clear workspace
rm(list=ls())

#View data in model
set.seed(52498)
y  = rnorm(1000,0,3);
x = rnorm(1000,0,exp(y/2));
plot(y~x)

#specify number of chains, used to initialize values and specify chains
nchains = 3

#Initialize values
initslst <- lapply(1:3,function(i) {
  list(
    y=runif(1,0,1),
    x=runif(9,0,1)
  )
})


#Centered Example with Divergent Transitions
Centered <- stan(file = 'Ex3_Cent.stan',
                  init = initslst,
                  chains = nchains,
                  iter = 4000 ,
                  warmup = 2000 , 
                  thin = 1 )

#View traceplots
traceplot(Centered,pars=c("y","x[1]"))
#view results
Centered


#specify number of chains, used to initialize values and specify chains
nchains = 3

#Initialize values
initslst <- lapply(1:3,function(i) {
  list(
    y=runif(1,0,1),
    x=runif(9,0,1)
  )
})

#NOncentered Example without Divergent transitions
non_centered <- stan(file = 'Ex3_Non_Cent.stan',
                       init = initslst,
                       chains = nchains,
                       iter = 4000 ,
                       warmup = 2000 , 
                       thin = 1 )
#View traceplots
traceplot(non_centered,pars=c("y","x[1]"))
#view results
non_centered


#Convert results to data frames
cent_df = data.frame(as.matrix(Centered))
noncent_df = data.frame(as.matrix(non_centered))

#Compare y values
par(mfrow=c(2,1))
 hist(cent_df$y,breaks=25,xlim=c(-10,10), main = c("Centered"))
 hist(noncent_df$y,breaks=25,xlim=c(-10,10), main = c("Non-Centered"))
par(mfrow=c(1,1))


#Diagnose with Shinystan
launch_shinystan(Centered)

launch_shinystan(non_centered)
