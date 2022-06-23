v <- mean_vars[2]
imp <- fiveday_means[[v]]
obs <- na.omit(data_fivehour_filled[[v]])

# varlnorm <- function(x) log(1 + var(x)/(mean(x)^2))
# 
# vlnimp <- varlnorm(imp)
# vlnobs <- varlnorm(obs)
# 
# 1/sum(exp(mean(log(imp)) + var(log(imp))) + exp(mean(log(obs[obs>0])) + var(log(obs[obs>0]))))
# 
# S <- function(x) exp(mean(log(x)) + var(log(x)))
# 
# log(1/S(obs[obs>0]) - S(imp))
# 

library(lognorm)
library(ggplot2)

target_sd <- sd(log(obs[obs>0]))

candidate_sd_noise <- seq(0.01, 1, by = 0.01)

imputed_sd_with_noise <- sapply(candidate_sd_noise, \(sd_noise) estimateSumLognormal(mu=c(mean(log(imp)), 0), sigma=c(var(log(imp)), sd_noise))['sigma'])

ggplot(data.frame(candidate_sigma_noise, observed_noise), aes(x=candidate_sigma_noise, y=observed_noise)) + geom_point() +
  geom_hline(yintercept = target_sigma)
