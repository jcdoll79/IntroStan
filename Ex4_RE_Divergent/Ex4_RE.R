#####CSTAT Workshop#################
#####Example 4 Random Effects Model#
#####From: "Optimizing Stan Code####
#####for Efficiency"################
#####File provided by instructor####
####################################

#load libraries
library(rstan)
library(ggplot2)
library(shinystan)

#Set working directory to source file locations
#This directory must have all data files and Stan model code needed.

#clear workspace
rm(list=ls())

#set random number seed for consistent data
set.seed(14898) 
#Generate data
gp=4 #Number of groups/individuals
n=10 #Number of measurements per group/individual

group = rep (1:gp, each = n)  #vector of code for group/individual
mu_true = 50 # True mean
sigma_y = 40 # Population SD
sigma_mu = 20 # Group SD
beta = 10     #slope
x = runif(n,0,25)     #values of covariate, year

mu_ind = rnorm(gp, mu_true, sigma_mu)
y = rnorm (gp * n, mean = mu_ind[group] + beta *x , sd = sigma_y)

data1 = data.frame(y, group, x = x)

ggplot(data = data1, aes(x = x, y = y, color = group)) +       
  geom_line(aes(group = group)) + geom_point()

#Package data, initialization values, and send to Stan
#Data in dataList(can use any name you want) must match values in Stan data block

#specify number of chains, used to initialize values and specify chains
nchains = 3

dataList = list(
  n= (gp*n),
  y = data1$y,
  x=data1$x,
  gp = data1$group,
  ngps = gp
)
#Initialize values
#convergence can be improved by setting reasonable starting values
#i.e, range of observations from 1-20, don't intialize mean at 100000
#Use different starting values for each chain
initslst <- lapply(1:nchains,function(i) {
  list(
    sigma_y=runif(1,1,10),
    sigma_mu=runif(1,1,10),
    mu=runif(1,min(y),max(y)),
    beta=runif(1,-10,10),
    gpmu = rnorm(gp,0,1)
  )
})

#Centered Example with Divergent Transitions
Center_RE <- stan(file = 'Ex4_Cent_RE.stan',
             data = dataList , 
             init = initslst,
             chains = nchains,
             iter = 1000 ,
             warmup = 500 , 
             thin = 2,
             control = list(adapt_delta=0.99))

traceplot(Center_RE,pars=c("sigma_mu"))
#Shinystan
launch_shinystan(Center_RE)

#Noncentered Example without Divergent transitions####
#specify number of chains, used to initialize values and specify chains
nchains = 3
dataList = list(
  n= (gp*n),
  y = data1$y,
  x=data1$x,
  gp = data1$group,
  ngps = gp
)

#Initialize values
#convergence can be improved by setting reasonable starting values
#i.e, range of observations from 1-20, don't intialize mean at 100000
#Use different starting values for each chain
initslst <- lapply(1:nchains,function(i) {
  list(
    sigma_y=runif(1,1,10),
    sigma_mu=runif(1,1,10),
    mu=runif(1,min(y),max(y)),
    beta=runif(1,-10,10),
    gpmu_raw = rnorm(gp,0,1)
  )
})

#Noncentered Example without Divergent transitions
non_Center_RE <- stan(file = 'Ex4_Non_Cent_RE.stan',
                  data = dataList , 
                  init = initslst,
                  chains = nchains,
                  iter = 1000 ,
                  warmup = 500 , 
                  thin = 2,
                  control = list(adapt_delta=0.99))

traceplot(non_Center_RE,pars=c("sigma_mu"))
#Shinystan
launch_shinystan(non_Center_RE)

