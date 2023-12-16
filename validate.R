################################################################################
# 1. observations are independent of each other.
# 2. no multicollinearity among the independent variables.
# 3. logistic regression assumes linearity of independent variables and log odds.
# 4. no overdispertion
# 5. large sample size, 10*(no. factors)/p(outcome)
################################################################################
# 1. Observations independent.
# auto-correlation?

# 2. Multicollinearity
# all predictors are orthogonal - no multicollinearity

# 2. Check for linearity
(
  ggplot(da)
  + aes(x=predicted,y=residual)
  + geom_point(alpha= .2)
  + xlab('Predicted Values')
  + ylab('Deviance Residuals')
  + labs(title="Linearity Assumption: Residuals Vs Predicted")
  + theme(plot.title = element_text(hjust = 0.5))
)

# 3. Check or overdispertion
# we are protecting against overdispertion by using quasibinomial

# 4. Large Sample size
10*(13)/(sum(da$censored)/sum(da$N)) < sum(da$N)

# 1. observations are independent of each other.
# 2. no multicollinearity among the independent variables.
# 3. logistic regression assumes linearity of independent variables and log odds.
# 4. no overdispertion
# 5. large sample size, 10*(no. factors)/p(outcome)