setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
source("5.5_binomial_corrected.R")

connectbB()

path <- 'models/presence/'
dir.create('models',showWarnings = FALSE)
dir.create(path,showWarnings = FALSE)

for(name in names(models)) {
  print(name)
  time()
  
  dir.create(paste0(path, name),showWarnings = FALSE)
  
  response <- c('censored','N')
  predictors <- models[[name]]
  datfun <- make.data(response, 
                    predictors = predictors, 
                    table = name)
  
  form <- make.formula('I(censored/N)', predictors)
  trace <- ifelse(name == 'lowest',TRUE,FALSE)
  model <- shglm2(form,
                  weights.fo=~N,
                  datafun = datfun,
                  family = binomial(),
                  trace=trace
                  )
  
  saveRDS(model, paste0(path, name, '/', name,'.agg_logit'))
  time()
  print('Finished')
}


disconnectdB()
