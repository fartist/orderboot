library(tidyverse)
library(rlang)
library(Rfast)
library(tidymodels)
boot_lm <- function(formula, data) {
  
  formula <- enexpr(formula)
  boot_data <- data[sample(nrow(data), replace = TRUE), , drop = FALSE]
  
  lm_call <- expr(lm(!!formula, data = boot_data))
  lm_env <- child_env(caller_env(), boot_data = boot_data)
  eval_bare(lm_call, lm_env)
}
asd <- map_dfr(1:100, ~boot_lm(y ~ 0 + ., data = data)$coef)
bootstraps(mtcars, times = 2000, apparent = TRUE)
pmap_dbl(list(c_hat_order, 1:K), ~(..1 + beta_hat_star) %>% sort_unique %>% `[`(..2)) - beta_order
imap_dbl(c_hat_order, 1:K, ~(.x + beta_hat_star) %>% sort_unique %>% `[`(.y))
colMeans(asd)
beta
bias_reduce_lm_order_0518 <- function(y, X, r = .15, n_boot = 1000, beta){
  N <- length(y)
  K <- ncol(X)
  # standard approach
  fit_std <- lm(y ~ 0 + X)
  beta_hat <- coef(fit_std)
  # (beta_max <- max(beta_hat))
  beta_order <- as.double(sort(beta_hat))

  results = matrix(data = NA, nrow = n_boot, ncol = K) ## vector to hold results
  c_hat_order <- map(beta_order, ~(1 - N^(r - .5))*(.x - beta_hat))
  
  b = 1    
  for(b in 1:n_boot){
    m = sample(x = 1:N, size = N, replace = TRUE) ## sample indices
    yy <- y[m]
    XX <- X[m, ]
    fit <- lm(yy ~ 0 + XX)
    beta_hat_star <- coef(fit)
    T_b_star <- map2_dbl(c_hat_order, 1:K, ~(.x + beta_hat_star) %>% sort_unique %>% `[`(.y)) - beta_order
    results[b, ] = T_b_star ## store results
  }
  est = sweep(-results, MARGIN = 2, -beta_order) %>% colmeans()
  beta_order_boot_sample <- sweep(-results, MARGIN = 2, -beta_order) %>% as.data.frame()
  ((beta_order_boot_sample - th) > 0) %>% colMeans()
  cr = map(beta_order_boot_sample, get_ci_1) %>% map2_lgl(.x = ., .y = 1:K, .f = ~is_cover_1(lower = .x[1], point = beta[.y]))
  return(list(est = est, cr = cr))
}

bias_reduce_lm_order_0518(y, X, r = .1, n_boot = 200, beta)

bias_reduce_lm_order_0526 <- function(y, X, r = .15, n_boot = 1000, beta, th = .2){
  N <- length(y)
  K <- ncol(X)
  # standard approach
  fit_std <- lm(y ~ 0 + X)
  beta_hat <- coef(fit_std)
  # (beta_max <- max(beta_hat))
  beta_order <- as.double(sort(beta_hat))
  
  results = matrix(data = NA, nrow = n_boot, ncol = K) ## vector to hold results
  c_hat_order <- map(beta_order, ~(1 - N^(r - .5))*(.x - beta_hat))
  
  b = 1    
  for(b in 1:n_boot){
    m = sample(x = 1:N, size = N, replace = TRUE) ## sample indices
    yy <- y[m]
    XX <- X[m, ]
    fit <- lm(yy ~ 0 + XX)
    beta_hat_star <- coef(fit)
    T_b_star <- map2_dbl(c_hat_order, 1:K, ~(.x + beta_hat_star) %>% sort_unique %>% `[`(.y)) - beta_order
    results[b, ] = T_b_star ## store results
  }
  est = sweep(-results, MARGIN = 2, -beta_order) %>% colmeans()
  beta_order_boot_sample <- sweep(-results, MARGIN = 2, -beta_order) %>% as.data.frame()
  M = ((((beta_order_boot_sample - th) >= 0) %>% colMeans()) > .05) %>% sum()
  cr = map(beta_order_boot_sample, get_ci_1) %>% map2_lgl(.x = ., .y = 1:K, .f = ~is_cover_1(lower = .x[1], point = beta[.y]))
  return(list(est = est, cr = cr, M = M))
}

bias_reduce_lm_order_0526(y, X, r = .1, n_boot = 200, beta, th = 0)


library(rlang)
library(purrr)
lm3 <- function(formula, data, env = caller_env(), ...) {
  formula <- enexpr(formula)
  data <- enexpr(data)
  
  lm_call <- expr(lm(!!formula, data = !!data))
  lm_call$haha <- "haha"
  expr_print(lm_call)
  eval(lm_call, env)
}

lm3(mpg ~ disp, mtcars)
#> lm(mpg ~ disp, data = mtcars)
#> 
#> Call:
#> lm(formula = mpg ~ disp, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)         disp  
#>     29.5999      -0.0412
start_time <- Sys.time()

b = 1
asd <- map(1:n_boot, ~{
  m = sample(x = 1:N,
             size = N,
             replace = TRUE) ## sample indices
  yy <- y[m]
  XX <- X[m,]
  fit <- lm(yy ~ 0 + XX)
  beta_hat_star <- coef(fit)
  map2_dbl(c_hat_order,
             1:K,
             ~ (.x + beta_hat_star) %>% sort_unique %>% `[`(.y)) - beta_order}
) %>% do.call("rbind", .)
asd %>% colmeans()

results %>% colmeans()
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
b = 1
for (b in 1:n_boot) {
  m = sample(x = 1:N,
             size = N,
             replace = TRUE) ## sample indices
  yy <- y[m]
  XX <- X[m,]
  fit <- lm(yy ~ 0 + XX)
  beta_hat_star <- coef(fit)
  T_b_star <-
    map2_dbl(c_hat_order,
             1:K,
             ~ (.x + beta_hat_star) %>% sort_unique %>% `[`(.y)) - beta_order
  results[b,] = T_b_star ## store results
}
end_time <- Sys.time()
end_time - start_time

x <- rnorm(1000) %>% mean
boot_lm <- function(formula, data) {
  formula <- enexpr(formula)
  boot_data <-
    data[sample(nrow(data), replace = TRUE), , drop = FALSE]

  lm_call <- expr(lm(!!formula, data = boot_data))
  lm_env <- child_env(caller_env(), boot_data = boot_data)
  eval_bare(lm_call, lm_env)
}

x = rnorm(1000, mean = 2)

boot_coef <- map_dbl(1:1000, ~boot_lm(x~1, data = data.frame(x))$coef)
boot_coef %>% mean


BSDA::z.test(x,sigma.x=1, alternative = "less", mu = 2)
mean(boot_coef > 2)
