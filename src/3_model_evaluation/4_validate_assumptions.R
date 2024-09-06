################################################################################
# 1. observations are independent of each other.
# 2. no multicollinearity among the independent variables.
# 3. logistic regression assumes linearity of independent variables and log odds.
# 4. no overdispertion
# 5. large sample size, 10*(no. factors)/p(outcome)
################################################################################

# Load necessary libraries


# Load the model and data (assuming they're saved as in 2_compute_residuals.R)
setwd("~/Desktop/working8/Thesis")
source("./src/utils/helper_functions.R")
connectbB()

name = 'global'
table = 'presence'
link = 'logit'
model_path = file.path('models', paste0(name,'_',table,'_',link,'.model'))
model <- readRDS(model_path)

# 1. Observations independent. 
# Auto-correlation check (this is more relevant for time series data)
# If your data is time-ordered, you can use the Durbin-Watson test
# dwtest(model)
# CHECK PLOT!!!

# 2. Multicollinearity DONE! ALL LESS THEN 10
# Calculate VIF for each predictor

# 3. Check for linearity of independent variables and log odds DONE!
# This involves creating interaction terms between each predictor and its log
# All predictors are binary - linearity assumption satisfied

# 4. Check for overdispersion -- NOT DONE!!!
summary.speedglm(model)
# residuals df: 226841165; residuals deviance: 1462284;
1462284 / 226841165

# 5. Large Sample size DONE!
N <- dbGetQuery(conn, "SELECT COUNT(*) as N FROM presence")$N
s <- dbGetQuery(conn, "SELECT SUM(permission_denied) as s FROM presence")$s
n_factors <- length(coef(model)) - 1  # Subtract 1 for intercept
outcome_proportion <- s / N # 86084 / 226841249
required_sample_size <- 10 * n_factors / outcome_proportion # 2187145.5400539

print(paste("Required sample size:", required_sample_size))
print(paste("Actual sample size:", N))
print(paste("Sample size adequate:", N > required_sample_size))

# Clean up
disconnectdB()
