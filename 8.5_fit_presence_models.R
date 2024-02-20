setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
source("5.5_binomial_corrected.R")

connectbB()

path <- 'models/presence/'
dir.create('models',showWarnings = FALSE)
dir.create(path,showWarnings = FALSE)

models[['global']] = global

for(name in names(models)) {
  if(name != 'global') next
  
  print(name)
  time()
  dir.create(paste0(path, name),showWarnings = FALSE)
  
  response <- c('permission_denied')
  predictors <- models[[name]]
  dat <- make.data(response, 
                   predictors = predictors, 
                   table = 'presence')
  
  form <- make.formula(response, predictors)
  model <- shglm2(form, 
                    datafun = dat,
                    family = binomial(link='logit'))
  
  saveRDS(model, paste0(path, name, '/', name,'.logit'))
  time()
  print('Finished')
  
}

disconnectdB()
