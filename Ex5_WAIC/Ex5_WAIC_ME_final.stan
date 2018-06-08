data{
  int<lower=0> n; //number of obserations
  vector[n] y;    //observations as a vector
  vector[n] x;     //observed x values, predictors
  int gp[n];      //subject indicator
  int ngps;       //number of groups
}

parameters{
  real<lower=0> sigma_y;   //individual standard deviation to be estimated
  real<lower=0> sigma_mu;  //group standard deviation to be estimated
  real gpmu_raw[ngps];     //Group mean_raw
  real mu;                 //Global mean across all groups
  real beta;               //slope
}

transformed parameters{
  real gpmu[ngps];              //Group mean  
  
  for(k in 1:ngps){
    gpmu[k] = mu + gpmu_raw[k] * sigma_mu;
  }
}

model {
  //reference priors
  sigma_y ~ cauchy(0,5);
  sigma_mu ~ cauchy(0,5);
  beta ~ normal(0,100);
  mu ~ normal(0,100);
  
  for(k in 1:ngps){
    gpmu_raw[k] ~ normal(0,1);//implies gpmu[k] ~ normal(mu,sigma_mu); //individual group mean
  }
  
  //likelihood, loop through number of observations
  for(i in 1:n){
    y[i] ~ normal(gpmu[gp[i]] + beta * x[i], sigma_y);
  }
}

generated quantities{
    vector[n] log_lik;
    
    for ( i in 1:n ) {
        log_lik[i] = normal_lpdf( y[i] | gpmu[gp[i]] + beta * x[i], sigma_y);
    }
}
