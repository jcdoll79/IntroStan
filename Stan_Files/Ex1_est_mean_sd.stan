
data{
  int<lower=0> n; //number of obserations
  vector[n] y;    //observations as a vector
}

parameters {
  real<lower=0> sigma;  //standard deviation to be estimated
  real mu;
}


model {
  
  //reference priors
  sigma ~ cauchy(0,5);
  mu ~ normal(0,100);
  
  //likelihood, loop through number of observations
  for(i in 1:n){
    y[i] ~ normal(mu, sigma);
  }
  
}

