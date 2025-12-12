functions {
  // calculates for a given country the model outputs as column
  // vectors. These are saved as an array with the outputs of (1)
  // E_deaths, (2) prediction, (3) Rt, (4) R_adj
  array[] vector country_model(real mu_local,
                         real y_local,
                         vector alpha_local,
                         matrix X_local,
                         real ifr_noise_local,
                         int N0,
                         int N2,
                         vector SI_rev,
                         real pop_local,
                         vector f_rev_local
                         ) {
    vector[N2] prediction = rep_vector(0.0, N2);
    vector[N2] E_deaths = rep_vector(0.0, N2);
    vector[N2] Rt = rep_vector(0.0, N2);
    vector[N2] Rt_adj = rep_vector(0.0, N2);
    vector[N2] cumm_sum = rep_vector(0.0, N2);
    
    // learn the number of cases in the first N0 days
    prediction[1:N0] = rep_vector(y_local, N0); 
    cumm_sum[2:N0] = cumulative_sum(prediction[2:N0]);
    
    Rt = mu_local * exp(X_local * alpha_local);
    Rt_adj[1:N0] = Rt[1:N0];
    
    for (i in (N0+1):N2) {
      real convolution = dot_product(head(prediction, i-1), tail(SI_rev, i-1));
      
      cumm_sum[i] = cumm_sum[i-1] + prediction[i-1];
      Rt_adj[i] = ((pop_local-cumm_sum[i]) / pop_local) * Rt[i];
      prediction[i] = Rt_adj[i] * convolution;
    }

    E_deaths[1]= 1e-15 * prediction[1];
    for (i in 2:N2) {
      E_deaths[i] = ifr_noise_local * dot_product(head(prediction, i-1), tail(f_rev_local, i-1));
    }

    return({ E_deaths, prediction, Rt, Rt_adj });
}
  
  // this is the partial sum function which calculates for
  // a subset of countries the log-lik contribution
  // so it computes for the countries m= start...end the
  // log lik
  real country_lpdf(array[] real mu_slice,
                    int start, int end,
                    array[] real y,
                    vector alpha,
                    matrix X,
                    real phi,
                    real ifr_noise,
                    array[] int N,
                    int N0,
                    int N2,
                    vector SI_rev,
                    array[] real pop,
                    array[] vector f_rev,
                    array[,] int deaths,
                    array[] int EpidemicStart
                    ) {
    // log-lik of this subset
    real log_lik = 0.0;
    
    for (m in start:end) {
      int m_slice = m - start + 1;
      vector[N2] E_deaths = country_model(
          mu_slice[m_slice],
          y[m],
          alpha,
          X,
          ifr_noise,
          N0,
          N2,
          SI_rev,
          pop[m],
          f_rev[m])[1];
      // vector[N[m] - EpidemicStart[m] + 1] evec = E_deaths[EpidemicStart[m]:N[m]];
      // for (i in 1:rows(evec)) {
      //   if (evec[i] < 0) {
      //     print("mu: ",mu_slice[m_slice], " ifr: ", ifr_noise[m], " y: ", y[m]);
      //   }
      // }
      
      log_lik += neg_binomial_2_lpmf(deaths[EpidemicStart[m]:N[m], m] |
                                     E_deaths[EpidemicStart[m]:N[m]], phi );
    }

    return(log_lik);
  }
}

data {
  int <lower=1> M; // number of countries
  int <lower=1> N0; // number of days for which to impute infections
  int <lower=1> P;
  array[M] int<lower=1> N; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
  matrix[N2,P] X;
  array[N2,M] int deaths; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[N2, M] f; // h * s
  array[M] int EpidemicStart;
  array[M] real pop;
  array[N2] real SI; // fixed pre-calculated SI using emprical data from Neil
}

transformed data {
  array[M] real mu = rep_array(1,M);
  array[M] real y = rep_array(1,M);
  vector[N2] SI_rev; // SI in reverse order
  array[M] matrix[N2, P] X_local;
  array[M] vector[N2] f_rev; // f in reversed order
  
  for(i in 1:N2)
    SI_rev[i] = SI[N2-i+1];
    
  for(m in 1:M){
    for(i in 1:N2) {
     f_rev[m, i] = f[N2-i+1,m];
    }
    X_local[m] = X;
  }
}
parameters {
  vector[P] alpha_glob;
  real<lower=0> phi;
  real<lower=-1,upper=1> rho;
  real<lower=0> sd_alpha;
}
model {
  phi ~ normal(0,5);
  alpha_glob[1] ~ normal(0, 10);
  alpha_glob[2] ~ normal(0, sd_alpha / sqrt(1 - rho^2));
  for (p in 3:P) {
    alpha_glob[p] ~ normal(rho * alpha_glob[p-1], sd_alpha);
  }
  sd_alpha ~ normal(0, 2);
  
  
  target += reduce_sum(
      country_lpdf, mu, 2,
      y,
      alpha_glob,
      X,
      phi,
      1,
      N,
      N0,
      N2,
      SI_rev,
      pop,
      f_rev,
      deaths,
      EpidemicStart
                        );
  
  
}

generated quantities {
  matrix[N2, M] E_deaths;
  matrix[N2, M] prediction;
  matrix[N2, M] Rt;
  matrix[N2, M] Rt_adj;
  matrix[N2, M] E_deaths0;
  matrix[N2, M] prediction0;

  for(m in 1:M) {
   array[4] vector[N2] local
        = country_model(
            mu[m],
            y[m],
            alpha_glob,
            X,
            1,
            N0,
            N2,
            SI_rev,
            pop[m],
            f_rev[m]);
   array[4] vector[N2] local0
        = country_model(
            mu[m],
            y[m],
            alpha_glob,
            X,
            1,
            N0,
            N2,
            SI_rev,
            pop[m],
            f_rev[m]);

    E_deaths[:,m] = local[1];
    prediction[:,m] = local[2];
    Rt[:,m] = local[3];
    Rt_adj[:,m] = local[4];
    
    E_deaths0[:,m] = local0[1];
    prediction0[:,m] = local0[2];
  }
}

