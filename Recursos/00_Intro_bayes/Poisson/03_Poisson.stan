data {
  int<lower=0> n;      // Número de áreas geograficas 
  int<lower=0> y[n];   // Conteos por area
  real<lower=0> alpha;
  real<lower=0> beta;
}
parameters {
  real<lower=0> theta;
}
model {
  y ~ poisson(theta);
  theta ~ gamma(alpha, beta);
}
generated quantities {
    real ypred[n];                    // vector de longitud n
    for(ii in 1:n){
    ypred[ii] = poisson_rng(theta);
    }
}
