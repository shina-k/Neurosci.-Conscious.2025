data {
  int<lower=1> I;   //Number of all data
  int<lower=1> N;   //Number of subject
  int<lower=1> Ti;  //number of time
  real minY;
  real maxY;
  array[I] int<lower=1, upper=N> PersonID; 
  array[I] int<lower=1, upper=Ti> TimeID;
  array[N, Ti] real<lower=minY, upper=maxY> Y; 
}

transformed data {
  real log_unif = -log(Ti);
}

parameters {
  array[N] real<lower=0> s; // observation error
  
  real<lower=0> s_d1; // drift range
  real<lower=0> s_d2; 
  
  array[Ti] real delta_err1;  //drift change value
  array[Ti] real delta_err2;
  
  array[N] real<lower=minY, upper=maxY> mu0;
}

transformed parameters{
  array[N] vector[Ti] lp;
  
  array[N,Ti] real mu1;
  array[N,Ti] real mu2;
  
  array[Ti] real delta1;
  array[Ti] real delta2;
  
  //reparameter blocks
  delta1[1] = s_d1 * delta_err1[1];
  delta2[1] = s_d2 * delta_err2[1];
  
  array[N] vector[Ti + 1] lp_e;
  array[N] vector[Ti + 1] lp_l;
  
  for (n in 1:N){
    mu1[n,1] = mu0[n];
    mu2[n,1] = mu0[n];
  
    for (t in 2:Ti){
      delta1[t] = delta1[t-1] + s_d1 * delta_err1[t];
      delta2[t] = delta2[t-1] + s_d2 * delta_err2[t];
      
      mu1[n,t] = mu1[n,t-1] + delta1[t-1];
      mu2[n,t] = mu2[n,t-1] + delta2[t-1];
      }
    }

  //dynamic 
  {
      for(n in 1:N){
        lp_e[n,1] = 0;
        lp_l[n,1] = 0;
        for (t in 1:Ti) {
          lp_e[n,t + 1] = lp_e[n,t] + normal_lpdf(Y[n,t] | mu1[n,t], s[n]);
          lp_l[n,t + 1] = lp_l[n,t] + normal_lpdf(Y[n,t] | mu2[n,t], s[n]);
       }
       lp[n,] = rep_vector(log_unif + lp_l[n,Ti + 1], Ti)
           + head(lp_e[n,], Ti) - head(lp_l[n,], Ti);
      }
      
    }
}

model {
    for(n in 1:N){
      target += log_sum_exp(lp[n,]);
    }
  
    target += cauchy_lpdf(s|0,5);
    
    target += normal_lpdf(delta_err1[1:Ti]|0,1);
    target += normal_lpdf(delta_err2[1:Ti]|0,1);
    
    target += cauchy_lpdf(s_d1|0,2.5);
    target += cauchy_lpdf(s_d2|0,2.5);
  
    target += normal_lpdf(mu0|mean(to_vector(Y)),10);
}


generated quantities {
  real delta1_mu;
  real delta2_mu;
  real diff;
  real BP;
  
  delta1_mu = 0;
  delta2_mu = 0;
  
  vector[N] ts;
  vector[Ti] lps;
  
  for(n in 1:N){
    lps = lp[n,];
    ts[n] = categorical_logit_rng(lps);
  }
  
  BP = sum(ts)* 1.0 /N;

    for(t in 1:Ti){
      if (t < BP){
        delta1_mu = delta1_mu + delta1[t];
      }
      else if (BP < t){
        delta2_mu = delta2_mu + delta2[t];
        }
    }
    delta1_mu = delta1_mu / BP;
    delta2_mu = delta2_mu / (Ti - BP);
    diff = delta1_mu - delta2_mu;
}

