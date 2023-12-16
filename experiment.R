group <- c(rep(0, 1000), rep(1, 1000))
success_probability <- c(rbeta(1000,1,2),rbeta(1000,2,1)) # decrease shape parameters
successes <- rbinom(2000, size = 1, prob = success_probability)
g1 <- glm(successes ~ group, family = quasibinomial)
summary(g1)

sum(resid(g1, type = 'pearson')**2) / g1$df.residual


group1 <- c(rep('0', floor(n1/2)), rep('1', ceiling(n1/2)), rep('2', floor((2000 - n1)/2)), rep('3', ceiling((2000 - n1)/2)))
sam <- sample(1:10, 2000, replace=TRUE)
group1 <- sample(sam)
table(group1)
# group2 <- c(rep('0', 500), rep('1', 500), rep('0',500), rep('1', 500))
# success_probability <- c(rbeta(1000,3,20), rbeta(1000,40,3))
# successes <- rbinom(2000, size = 1, prob = success_probability)
trials = 1
df <- data.frame(s = successes, n = trials, group1 = group1, group2)
g1 <- glm(s ~ group2, data = df, family = quasibinomial)
summary(g1)

sum(resid(g1, type = 'pearson')**2) / g1$df.residual

# Aggregating the data
aggregated_df <- aggregate(cbind(s, n) ~ group1 + group2, df, FUN = sum)

# Fitting the binomial model on the aggregated data
g2 <- glm(cbind(s, n - s) ~ group2  , family = quasibinomial, data = aggregated_df)
print(summary(g2))

sum(resid(g2, type = 'pearson')**2) / g2$df.residual

# to_int = names(da)[!(names(da) %in% c('tokencount','censored','not_censored'))]
# factors = combn(to_int, 2, FUN = function(x) paste0(x,collapse=':'))
# factors = c(to_int, factors)
# form = paste0(factors, collapse = '+')
# form = formula(paste0('censored | trials(censored + not_censored) ~ tokencount + ', form))


