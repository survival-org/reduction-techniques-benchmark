library(tidyverse)
library(mvtnorm)
library(Matrix)
library(pammtools)
library(flexsurv)
library(simsurv)


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

df = cbind(df, simulate_correlated_variables(n_variables = 20, n_samples = n))

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
surv_df$cens = runif(nrow(surv_df), 0, 20)
surv_df <- surv_df %>%
  mutate(
    status = if_else(time > cens, 0, 1), 
    time = if_else(status == 0, cens, time)) %>%
  select(-cens)



saveRDS(surv_df, "datasets/synthetic-tve.Rds")




# genetic data with high-dim interactions

data("eHGDP", package = "adegenet")
synth_df_genetic <- na.omit(as.data.frame(eHGDP@tab)[, 1:100])
# synth_df_genetic = synth_df_genetic[, colSums(synth_df_genetic) > 50]
synth_df_genetic <- cbind(synth_df_genetic, hdi = Reduce("*", synth_df_genetic[, c("loc-6.175", "loc-8.183", "loc-8.187")]))
colnames(synth_df_genetic) = gsub("-", "_", colnames(synth_df_genetic))


# specify model with two main effects and independent high-dim interaction
betas = rep(0, ncol(synth_df_genetic))
names(betas) = colnames(synth_df_genetic)
betas[c("loc_6.159", "loc_2.288")] = c(-.2, .2)
betas["hdi"] = -.4
synth_surv_genetic = simsurv(dist = "weibull", lambdas = .1, gammas = 1.5, maxt=5, betas = betas, x = synth_df_genetic)
# summary(synth_surv_genetic$eventtime)
# table(synth_surv_genetic$status)
# synth_df_genetic = cbind(synth_df_genetic, synth_surv_genetic[ c("status", "eventtime")])
# m = survival::survreg(Surv(eventtime, status) ~ ., data = synth_df_genetic, dist = "weibull")
# summary(m)
saveRDS(synth_df_genetic, "datasets/synthetic-hdi.Rds")




# break point data 

n <- 2000
# create data set with variables which will affect the hazard rate.
df <- simulate_correlated_variables(n_variables = 20, n_samples = n)
# the formula which specifies how covariates affet the hazard rate
f0 <- function(t) {
 dgamma(t, 8, 2) *6
}
form <- ~ -3.5 + f0(t) + (v2 > 0)*(v1<0)*(f0(t)*(t<4) + (t>4)*0) + .2 *v3 -.4 * v4
set.seed(24032018)
sim_df <- sim_pexp(form, df, 1:10)

saveRDS(sim_df, "datasets/synthetic-breakpoint.Rds")
