data {
  int<lower=0> N;
  int<lower=0> nbetas;
  matrix[N,nbetas] X; // Design matrix
  vector[N] y; // Vector of response values
}

transformed data {
matrix[N, nbetas] Q_ast;
matrix[nbetas, nbetas] R_ast;
matrix[nbetas, nbetas] R_ast_inverse;
// thin and scale the QR decomposition
Q_ast = qr_thin_Q(X) * sqrt(N - 1);
R_ast = qr_thin_R(X) / sqrt(N - 1);
R_ast_inverse = inverse(R_ast);
}


parameters {
  vector[nbetas] theta; // vector of real theta 
  real<lower=0> sigma; // STD
}
model {
  y ~ normal( Q_ast * theta, sigma ); // mvn mean Xbeta
  theta ~ normal(0,100);// mvn 
  sigma ~ gamma(1,1); // univariate
}

generated quantities {
  vector[nbetas] beta;
  beta = R_ast_inverse * theta; // coefficients on x
}
