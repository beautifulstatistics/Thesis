setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")
time()

connectbB()

dir.create('models', showWarnings = FALSE)
response <- c('permission_denied')

for(table in c('all_data','presence')){
  for(name in names(models)){
    predictors <- models[[name]]
    form <- make.formula(response, predictors)
    
    for(link in c('logit','cloglog','cauchit')){
      model_path = file.path('models', paste0(name,'_',table,'_',link,'.model'))
      if (file.exists(model_path)){
        print(paste('finished:', model_path))
        next
      }
      
      print(paste('starting:', model_path))
      
      dat <- make.data(response = response,
                       predictors = predictors,
                       table = table,
                       chunksize = 2*10^6)

      tryCatch({
        model <- shglm(form,
                       datafun = dat,
                       family = binomial(link=link),
                       trace = TRUE)

        
        
        attr(model, "terms") <- terms(form)
        
        saveRDS(model, model_path)
        print(paste('finished:', model_path))
      }, error = function(e) {
        print(paste("Error in model fitting:", model_path))
        print(e)

        mock_model <- create_mock_model(predictors, form, link)

        saveRDS(mock_model, model_path)
        print(paste('Saved mock model for:', model_path))
      })
    }
  }
}

print('Finished')
disconnectdB()
time()