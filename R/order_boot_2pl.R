# install.packages("lmeresampler")
library(lmeresampler)
library(lme4)
library(Rfast)
mixed <- glmer(incidence/size ~ period + (1|herd),
               weights=size, data=cbpp, family=binomial)
fixef(mixed)
FUN <- function(fit) {
  return(fixef(fit))
}
# cbpp$herd
# result <- bootMer(mixed, FUN, nsim = 100)
#
# ?gl
# dat<-data.frame(x=runif(100,-2,2),ind=gl(n=10,k=10))
# dat$y<-1+2*dat$x+rnorm(10,0,1.2)[dat$ind]+rnorm(100,0,0.5)
# m<-lmer(y~x+(1|ind),dat)
# b_par<-bootMer(x=m,FUN=fixef,nsim=200)
# boot.ci(b_par,type="perc",index=1)
#
# pl
#
#
# library(ltm)
# dsc <- descript(Abortion)
# dsc
#
#
# abo_long <-
#   Abortion %>% mutate(person = 1:n()) %>%
#   pivot_longer(cols = starts_with("Item"),
#                           names_to = "item",
#                           values_to = "score")
#
#
# mixed <- glmer(score ~ 0 + item + (1|person),
#                data=abo_long,
#                family=binomial, nAGQ = 20)
# beta_hat <- fixef(mixed)
# fit <- mixed
# r = .1
# N = dim(abo_long)[1]
#
# beta_order <- as.double(sort(beta_hat))
# c_hat_order <- map(beta_order, ~ (1 - N ^ (r - .5)) * (.x - beta_hat))
# FUN <- function(fit) {
#   return(fixef(fit))
# }
# timestamp()
# result <- bootMer(mixed, FUN, nsim = 500)
# timestamp()
# n_boot = 500
# results <-
#   map(1:n_boot, function(index)
#   {map2_dbl(c_hat_order,
#             1:K,
#             ~ (.x + result$t[index, ]) %>%
#               sort_unique %>%
#               `[`(.y))} - beta_order) %>%
#   do.call("rbind", .)
# (est  <-  sweep(-results, 2, -beta_order) %>% colMeans())
#
# tibble(beta_order, est) %>% knitr::kable("latex")
# beta_order_boot_sample <-
#   sweep(-results, 2,-beta_order) %>% as.data.frame()
# M <-  ((((
#   beta_order_boot_sample - th
# ) >= 0) %>% colMeans()) > .05) %>% sum()
# cr  <-  map(beta_order_boot_sample, get_ci_1) %>% map2_lgl(
#   .,
#   1:K,
#   ~ is_cover_1(lower = .x[1], point = beta[.y])
# )
#
#
# # do it for each row of result (replace beta_hat_star with each row of beta_hat_star)
# map2_dbl(c_hat_order,
#          1:K,
#          ~ (.x + beta_hat_star) %>%
#            sort_unique %>%
#            `[`(.y)) - beta_order
# ) %>% do.call("rbind", .)
#
#
# result$t
# map2(c_hat_order,
#          1:K,
#          ~ (.x + result$t[2, ]) %>%
#            sort_unique %>%
#            `[`(.y))
#
#
#
# # - beta_order
#
# est  <-  sweep(-result$t, 2, -beta_order) %>% colMeans()
# beta_order_boot_sample <-
#   sweep(-result$t, 2,-beta_order) %>% as.data.frame()
# M <-  ((((
#   beta_order_boot_sample - th
# ) >= 0) %>% colMeans()) > .05) %>% sum()
# cr  <-  map(beta_order_boot_sample, get_ci_1) %>% map2_lgl(
#   .,
#   1:K,
#   ~ is_cover_1(lower = .x[1], point = beta[.y])
# )
library(mirt)
library(lme4)
library(tidyverse)
# illustration of bias
J = 30
I = 10
Theta <- matrix(rnorm(N, sd = 1))
#items and response data
a <- matrix(1, I); d <- matrix(1.5, I)
dat <- simdata(a, d, J, itemtype = '2PL', Theta=Theta)


sim_rim <- function(I = 10, J = 300, u_sd = 1) {
  Theta <- matrix(rnorm(J, sd = u_sd))
  #items and response data
  a <- matrix(1, I); d <- matrix(1.5, I)
  dat <- simdata(a, d, J, itemtype = '2PL', Theta=Theta)
  return(dat)
}

# original_mat <- matrix(nrow = 500, ncol = 10)
# boot_mat <- matrix(nrow = 500, ncol = 10)
original_mat[1:500, ] %>% colMeans()
boot_mat[1:500, ] %>% colMeans()

n_boot = 200

iter_i = 1
for (iter_i in 1:500){
  data <- sim_rim(J = 300)
  mod0 <- mirt(data, 1, '2PL')
  mod0_coef <- coef(mod0, IRTpars=F, simplify=T)
  original_mat[iter_i,] <- mod0_coef$items[, "d"] %>% sort
  timestamp()
  result <- boot.mirt(mod0, R=n_boot)
  timestamp()

  K = 10
  beta_hat <- mod0_coef$items[, "d"]
  r = .01
  N = J
  beta_order <- as.double(sort(beta_hat))
  c_hat_order <-
    map(beta_order, ~ (1 - N ^ (r - .5)) * (.x - beta_hat))
  results <-
    map(1:n_boot, function(index)
    {
      map2_dbl(c_hat_order,
               1:K,
               ~ (.x + result$t[index, seq(2, 20, by = 2)]) %>%
                 sort_unique %>%
                 `[`(.y))
    } - beta_order) %>%
    do.call("rbind", .)
  est  <-  sweep(-results, 2,-beta_order) %>% colMeans() %>% sort
  boot_mat[iter_i,] <- est
}
