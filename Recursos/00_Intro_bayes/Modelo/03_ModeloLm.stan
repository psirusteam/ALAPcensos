data {
  int<lower=0> n;   // Número de observaciones
  int<lower=0> K;   // Número de predictores
  matrix[n, K] x;   // Matrix de predictores
  vector[n] y;      // Vector respuesta
}
parameters {
  vector[K] beta;       
  real<lower=0> sigma2;
}
model {
  to_vector(beta) ~ normal(0, 1000);
  sigma2 ~ inv_gamma(0.0001, 0.0001); 
  y ~ normal(x * beta, sqrt(sigma2));  // likelihood
}
generated quantities {
    real ypred[n];  // vector de longitud n
    ypred = normal_rng(x * beta, sqrt(sigma2));
}
