library(tidyverse)
library(mvtnorm)
library(Matrix)
library(pammtools)


# function to simulate correlated variables
simulate_correlated_variables <- function(n_variables, n_samples) {

  cor_mat <- matrix(0.1, nrow = n_variables, ncol = n_variables)
  diag(cor_mat) <- 1
  cor_mat[upper.tri(cor_mat)] <- seq(0.1, 0.5, length.out = sum(upper.tri(cor_mat)))
  cor_mat[lower.tri(cor_mat)] <- t(cor_mat)[lower.tri(cor_mat)]

  # Fix the correlation matrix to be positive semidefinite
  cor_mat_fixed <- as.matrix(nearPD(cor_mat, corr = TRUE)$mat)
  df_noise <- mvtnorm::rmvnorm(
    n, 
    mean = rep(0, ncol(cor_mat_fixed)), 
    sigma = cor_mat_fixed)
  colnames(df_noise) <- paste0("v", 1:n_variables)
  df_noise

}


## Time-varying effects 
set.seed(160932)

n = 2000
time_grid  = seq(0, 10, by = 0.1)

# covariates
df <- tibble::tibble(
    x0 = sample(c(-1,1), n, .3),
    x1 = runif(n, -3, 3),
    x2 = runif(n, -3, 3),
    x3 = runif(n, -3, 3)
)



# baseline hazard
f0 <- function(t, x0) { x0 * dgamma(t, 8, 2) * 6 }
# linear effect
fx1 <- function(x1, t) { -0.1 * x1}
# non-linear decaying time-varying effect of x2
fx2 <- function(x2, t) { dnorm(x2) * 10 * (1/(t + 1)) }
# non-linear time-varying effect of x3 (periodical)
fx3 <- function(x3, t) {2 * x3 * cos(t / max(time_grid) * pi)}

surv_df <- pammtools::sim_pexp(
    formula = ~ -3.5 + f0(t, x0)  + fx1(x1, t) + fx2(x2, t) + fx3(x3, t),
    data    = df,
    cut     = time_grid)


# set number of observations/subjects
n <- 2000
# create data set with variables which will affect the hazard rate.
df <- cbind.data.frame(x1 = runif (n, -3, 3), x2 = runif (n, 0, 6)) %>%
 as_tibble()
# the formula which specifies how covariates affet the hazard rate
f0 <- function(t) {
 (t < 4)*dgamma(t, 8, 2) *6 + (t>4)*0
}
form <- ~ -3.5 + f0(t)
set.seed(24032018)
sim_df <- sim_pexp(form, df, 1:10)

