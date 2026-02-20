library(dplyr)

set.seed(123)

# settings
n <- 100              # number of X_i in the sum
mu <- 100              # mean of each X_i
var_target <- 200     # target Var(X_i) for Normal and NB2
nsim <- 1000      # number of Monte Carlo replicates

# NB2 dispersion parameter such that Var = mu + mu^2/phi = var_target
phi <- mu^2 / (var_target - mu)
mu + mu^2/phi

# simulate sums
sim0 <- tibble(rep = 1:1000) |>
  mutate(
    sum_normal = replicate(1000, sum(rnorm(1, mean = mu, sd = sqrt(var_target)))),
    sum_poisson = replicate(1000, sum(rpois(1, lambda = mu))),
    sum_nb = replicate(1000, sum(rnbinom(1, mu = mu, size = phi)))
  )
sim0 %>% 
  pivot_longer(cols=c("sum_normal","sum_poisson","sum_nb"),values_to = "y",names_to = "dist") %>% 
  ggplot(aes(x=dist,y=y))+
    geom_point()


sim <- tibble(rep = 1:nsim) |>
  mutate(
    sum_normal = replicate(nsim, sum(rnorm(n, mean = mu, sd = sqrt(var_target)))),
    sum_poisson = replicate(nsim, sum(rpois(n, lambda = mu))),
    sum_nb = replicate(nsim, sum(rnbinom(n, mu = mu, size = phi)))
  )

sim %>% 
  pivot_longer(cols=c("sum_normal","sum_poisson","sum_nb"),values_to = "y",names_to = "dist") %>% 
  ggplot(aes(x=dist,y=y))+
  geom_point()

# summary statistics
results <- sim |>
  pivot_longer(cols=c("sum_normal","sum_poisson","sum_nb"),values_to = "y",names_to = "dist") %>% 
  group_by(dist) %>% 
  summarise(
    mean_sum = mean(y),
    var_sum = var(y),
    sd_sum = sd(y),
    rel_sd_sum = sd_sum/mean_sum,
    q2.5 = quantile(y, 0.025),
    q97.5 = quantile(y, 0.975)
  )

sim0 |>
  pivot_longer(cols=c("sum_normal","sum_poisson","sum_nb"),values_to = "y",names_to = "dist") %>% 
  group_by(dist) %>% 
  summarise(
    mean_sum = mean(y),
    var_sum = var(y),
    sd_sum = sd(y),
    rel_sd_sum = sd_sum/mean_sum,
    q2.5 = quantile(y, 0.025),
    q97.5 = quantile(y, 0.975)
  )

results
