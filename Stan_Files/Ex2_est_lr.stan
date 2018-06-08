data { 
  int<lower=0> n;  //number of observations
  vector[n] x;     //observed x values, predictors
  vector[n] y;     //observed y values, response
}

parameters { 
  real<lower=0> sigma; //standard deviation
  real alpha;          //y-intercept
  real beta;           //slope
} 

model { 
  
  //reference priors
  alpha ~ normal(0,100);
  beta ~ normal(0,100);
  sigma ~ cauchy(0,5);
  
  //likelihood, loop through number of observations
  for(i in 1:n){
    y[i] ~ normal(alpha + beta * x[i], sigma); 
  }
  
}
