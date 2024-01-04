setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

library(speedglm)

connectbB()

path <- 'models/presence/'
dir.create('models',showWarnings = FALSE)
dir.create(path,showWarnings = FALSE)

name <- 'global'
form <- make.formula('permission_denied', global)

time()
for(table in c('presence')){
  data <- make.data('permission_denied',
                    predictors = global,
                    table = table)
  
  for(link in c('cloglog','logit')){
    model <- shglm(form,
                   datafun = data,
                   family = binomial(link=link),
                   trace = TRUE)
    
    saveRDS(model, paste0(path, name, '.', table, link))
    
    print('Summary')
    time()
    print(summary(model))
  }
}

disconnectdB()