library(brms)
options(mc.cores = 4)

dir.create('models/aggregatedbayes/residuals',showWarnings = FALSE)
dir.create('models/aggregatedbayes/plots',showWarnings = FALSE)

ll = list()
for(file in list.files('models/aggregatedbayes', full.names = TRUE)){
  model <- readRDS(file)
  ll[[file]] = bridge_sampler(model)$logml
  break
}


file <- "models/aggregatedbayes/highest.model"
model <- readRDS(file)

resid_dev <- data.frame(residuals(model, type = "ordinary"))$Estimate
fitted_values <- fitted(model)

plot(fitted_values, resid_dev, main = "Deviance Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Deviance Residuals")


pp_check(model)

plot(marginal_effects(model))

pp_check(model)
