setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")
source("./src/utils/binomial_corrected.R")

connectdB()

name = 'global'
table = 'aggregated'
link = 'logit'

response <- "permission_denied"
predictors <- models[[name]]

da <- make.data(response = response,
                predictors = c(predictors,'N'),
                table = table,
                chunksize = 5 * 10^6)

m2 <- readRDS(file.path('models','decisiontree','global_presence_logit_rpart_residualSensitive.model'))[[1]]
m2 <- prune.cp(m2)

interactions = rpart.get_leaf_variables(m2)
interactions = paste0(interactions,collapse = '+')
print(paste0('Interactions:',interactions))
predictors <- c(predictors, interactions)
form <- make.formula(response, predictors)
model <- shglm2(form,
                  datafun = da,
                  family = binomial(link=link),
                  weights=~N,
                  trace = TRUE,
                  sparse = FALSE)
  
attr(model, "terms") <- terms(form)
  
model_path = file.path('.', paste0(name,'_',table,'_',link,'_adjustedADTLeafPaths.model'))
saveRDS(model, model_path)

print('Finished.')

disconnectdB()
