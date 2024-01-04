setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

connectbB()

path <- 'models/presence/'
dir.create('models',showWarnings = FALSE)
dir.create(path,showWarnings = FALSE)

for(name in names(models)) {
  print(name)
  time()
  dir.create(paste0(path, name),showWarnings = FALSE)
  
  predictors = models[[name]]
  data <- make.data('permission_denied', 
                    predictors = predictors, 
                    table = 'presence')
  
  form <- make.formula('permission_denied', predictors)
  model <- shglm(form, 
                 datafun = data, 
                 family = binomial(link='logit'))
  
  saveRDS(model, paste0(path, name, '/', name,'.logit'))
}

disconnectdB()
