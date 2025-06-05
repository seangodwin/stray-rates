data {
  int<lower=1> N;              // Number of fish observations
  int<lower=0, upper=1> y[N];  // Stray indicator (1 = strayed, 0 = homed)

  // Watershed info
  int<lower=1> K;              // Total number of watersheds (including isolated)
  int<lower=1, upper=K> ws[N]; // Watershed index for each fish (1 to K)

  // Temporal info
  int<lower=1> T;              // Number of unique years
  int<lower=1, upper=T> year[N]; // Year index for each fish

  // Spatial adjacency info (for ICAR part of BYM2)
  // These define which watershed pairs are connected
  int<lower=1> n_edges;                 // Number of edges in the spatial graph
  int<lower=1, upper=K> node1[n_edges]; // Index of one watershed in edge pair
  int<lower=1, upper=K> node2[n_edges]; // Index of other watershed in edge pair
  
  // Other covariates
  // Latitude per watershed (centroid for now)
  vector[K] lat; 
}

parameters {
  // Time trend (RW1)
  vector[T] f;                        // Year effects (time trend)
  real<lower=0> tau_f;                // Step size SD for RW1
  
  // BYM2 spatial components
  vector[K] u;                // Structured (ICAR) spatial effect for each ws
  vector[K] v;                // Unstructured (i.i.d.) spatial effect
  real<lower=0, upper=1> phi; // Mixing param: 1=all ICAR, 0=all unstructured
  real<lower=0> sigma_u;      // Overall standard deviation of spatial effects

  // Other covariates
  real beta_lat;              // Coefficient for latitude effect

  // Residual individual-level noise
  real<lower=0> sigma_eps;            // SD for individual-level residual noise
  vector[N] eps;                      // Individual-level noise

  // Global mean (spatial intercept average)
  real mu_alpha;  // Global average intercept (mean log-odds of straying)
}

transformed parameters {
  // Final watershed-level spatial effects after blending ICAR + unstructured (BYM2)
  // Even isolated watersheds (no neighbors) get v[k], 
      // and u[k] is unconstrained for them
  // All watersheds get an unstructured random effect v[k] (like a random inT)
  // For watersheds with neighbors, u[k] is smoothed using the ICAR prior
  // For isolated watersheds (no edges in node1/node2), 
      // u[k] not constrained by neighbors,
      // but is still weakly centered via sum(u) ~ normal(0, ...).
  // So alpha[k] still estimated for isolated basins,
      // it just relies more on v[k] rather than spatial borrowing
  vector[K] alpha;
  for (k in 1:K)
    alpha[k] = mu_alpha 
               + beta_lat * lat[k]
               + sigma_u * (sqrt(phi) * u[k] + sqrt(1 - phi) * v[k]);

  // Log-odds of straying for each fish
  vector[N] eta;
  for (n in 1:N)
    eta[n] = alpha[ws[n]] + f[year[n]] + sigma_eps * eps[n];
}

model {
  // Random Walk (RW1) prior for year effects
  f[1] ~ normal(0, 1);        // weak prior on first year 
  for (t in 2:T)
    f[t] ~ normal(f[t - 1], tau_f);      // temporal smoothness
  tau_f ~ normal(0, 1);                  // SD for RW1 steps
  
  // ICAR prior for structured spatial effect
  // Only applies to connected watersheds (defined in node1/node2)
  // Isolated watersheds have no edges, so are unaffected here
  for (e in 1:n_edges)
    target += -0.5 * square(u[node1[e]] - u[node2[e]]);  
  
  // Soft sum-to-zero constraint ensures identifiability of u
  // For isolated watersheds, this keeps u[k] loosely centered
  sum(u) ~ normal(0, 0.001 * K);                         
  
  // Prior for unstructured spatial effect (applies to all watersheds)
  v ~ normal(0, 1);      
  
  // Priors for spatial hyperparameters
  sigma_u ~ normal(0, 1);     // SD of total spatial effect
  phi ~ beta(0.5, 0.5);       // mixing: structured vs unstructured

  // Priors for other parameters
  beta_lat ~ normal(0, 1);    // latitude

  // Priors for residual variation
  eps ~ normal(0, 1);
  sigma_eps ~ normal(0, 1);

  // Global mean stray tendency
  mu_alpha ~ normal(0, 2);

  // Likelihood
  y ~ bernoulli_logit(eta);
}

generated quantities {
  // Posterior predictive replication of stray events
  int y_rep[N];
  for (n in 1:N)
    y_rep[n] = bernoulli_logit_rng(eta[n]);
}
  
