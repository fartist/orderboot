library(tidyverse)
# library(rlang)
library(Rfast)
library(SimDesign)

# # quick boot
# boot_lm <- function(formula, data) {
#   formula <- enexpr(formula)
#   boot_data <-
#     data[sample(nrow(data), replace = TRUE), , drop = FALSE]
#   
#   lm_call <- expr(lm(!!formula, data = boot_data))
#   lm_env <- child_env(caller_env(), boot_data = boot_data)
#   eval_bare(lm_call, lm_env)
# }
is_cover_1 <- function(lower, point){
  return((point >= lower))
}
get_ci_1 <- function(X){
  return(quantile(X, c(.05)))
}
bias_reduce_mirt <-
  function(data,
           r = .15, # tuning parameter
           n_boot = 10,
           beta, # true beta
           th = .2) { # cutoff - "delta"
    
    N = nrow(data)
    K <- length(beta)
    # standard approach
    fit_std <- 
      mirt(data, 1, 'Rasch', SE = T)
    
    beta_hat <- coef(fit_std, IRTpars=F, simplify=T)$items[, "d"]
    result <- boot.mirt(fit_std, R=n_boot)
    #result <- bootMer(fit_std, FUN, nsim = n_boot)
    naive_SE <- coef(fit_std, printSE = T, as.data.frame = T)[, 2] %>% na.omit() %>% as.double 
    
    #c_hat_order <- map(beta_order, ~ (1 - N ^ (r - .5)) * (.x - beta_hat))
    naive_lower <- map_dbl(1:K, ~beta_hat[.x] - 1.645*naive_SE[-(K + 1)][.x])[rank(beta_hat)]
    beta_order <- as.double(sort(beta_hat))
    c_hat_order <-
      map(beta_order, ~ (1 - N ^ (r - .5)) * (.x - beta_hat))
    results <-
      map(1:n_boot, function(index)
      {
        map2_dbl(c_hat_order,
                 1:K,
                 ~ (.x + result$t[index, -(K + 1)]) %>%
                   sort %>%
                   `[`(.y))
      } - beta_order) %>%
      do.call("rbind", .)
    est  <-  sweep(-results, 2,-beta_order) %>% colMeans() %>% sort
    est_naive <- beta_order
    beta_order_boot_sample <-
      sweep(-results, 2,-beta_order) %>% as.data.frame()
    M <-  ((((
      beta_order_boot_sample - th
    ) >= 0) %>% colMeans()) > .05) %>% sum()
    boot_lower <- map_dbl(beta_order_boot_sample, get_ci_1)
    cr  <- boot_lower %>% map2_lgl(
      .,
      1:K,
      ~ is_cover_1(lower = .x[1], point = beta[.y])
      
    )
    cr_naive  <-  naive_lower %>% map2_lgl(
      .,
      1:K,
      ~ is_cover_1(lower = .x, point = beta[.y])
      
    )
    est_bias = est - beta
    est_naive_bias = est_naive - beta
    return(list(est = est, cr = cr, M = M, est_naive = est_naive, cr_naive = cr_naive,
                est_bias = est_bias,
                est_naive_bias = est_naive_bias,
                boot_lower = boot_lower,
                naive_lower = naive_lower))
  }
bias_reduce_mirt(dat,
                  r = .15, # tuning parameter
                  n_boot = 100,
                  beta, # true beta
                  th = .2)

# bias_reduce_lm_order_0526(y,
#                           X,
#                           r = .1,
#                           n_boot = 200,
#                           beta,
#                           th = 0)

# -----simulation-------------------------------------
Design <-
  createDesign(beta = list(
    #c(0, .5, 1, 2, 3, 4),
    # c(0, 0),
    # c(0, 0.05),
    # c(0, 1),
    c(0, 0, 0, 0, 0, 0),
    c(0, .1, .2, .3, .4, .5),
    c(0, 1, 2, 3, 4, 5),
    rep(0, 20),
    c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, 7.5, .8, .85, .9, .95),
    0:19
    #c(0, 0, 0, 0, 1, 2),
    #c(0, 0, 1, 2, 3, 4),
    #c(0, 0.2, .4, .8, 1, 1.2)
  ),
  r = seq(0, .5, length.out = 10),
  th = 0,
  N = c(100),
  u_sd = c(1))

Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition) 
  K = length(beta)
  #max_beta_true <- max(beta)
  Theta <- matrix(rnorm(N, sd = u_sd))
  #items and response data
  a <- matrix(1, K); d <- as.matrix(beta)
  data <- simdata(a, d, N, itemtype = '2PL', Theta=Theta)
  dat = list(beta = beta, r = r, data = data, th = th)
  dat
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
  Attach(dat) # make objects X, M, and Y directly accessible
  bias_reduce_lm_fit <- bias_reduce_mirt(data = data, n_boot = 1, beta = beta, r = r, th = th)
  ret <- c(est = bias_reduce_lm_fit$est, 
           est_naive = bias_reduce_lm_fit$est_naive,
           est_bias = bias_reduce_lm_fit$est_bias, 
           est_naive_bias = bias_reduce_lm_fit$est_naive_bias,
           boot_lower = bias_reduce_lm_fit$boot_lower, 
           naive_lower = bias_reduce_lm_fit$naive_lower,
           cr_naive = bias_reduce_lm_fit$cr_naive,
           cr = bias_reduce_lm_fit$cr) 
  ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
  ret <- colMeans(results)
  ret
}

#---------------------------------------------------------------------

### Run the simulation
res <-
  runSimulation(
    Design,
    replications = 100,
    verbose = T,
    parallel = F,
    generate = Generate,
    analyse = Analyse,
    summarise = Summarise,
    packages = c("tidyverse", "mirt"),
    save_results = T
  )

# results
res %>% view

# coverage
res %>% filter(sigma == 1) %>% 
  mutate(beta = as.character(beta)) %>% 
  pivot_longer(cols = est_bias1:est_naive_bias6,
               names_to = "order",
               values_to = "CR") %>% 
  mutate(order = recode(order, 
                        "cr.V1" = 'beta[(1)]',
                        "cr.V2" = 'beta[(2)]',
                        "cr.V3" = 'beta[(3)]',
                        "cr.V4" = 'beta[(4)]',
                        "cr.V5" = 'beta[(5)]',
                        "cr.V6" = 'beta[(6)]',))%>% 
  ggplot(., aes(x = r, y = CR, color = order)) + 
  facet_grid(vars(beta), vars(order), labeller = label_parsed) + 
  geom_hline(yintercept = .95) + 
  geom_point()

res %>% View
# set inference 
res %>% 
  mutate(beta = as.character(beta)) %>% 
  pivot_longer(cols = cr.V1:cr.V6,
               names_to = "order",
               values_to = "CR") %>% 
  ggplot(., aes(x = th, 
                y = M)) + 
  facet_grid(vars(r), vars(beta)) + 
  geom_hline(yintercept = .95) + 
  geom_point() + 
  scale_x_continuous(breaks = c(0, 0.3, 1.2)) + xlab("threshold") + ylab("prop_correct")