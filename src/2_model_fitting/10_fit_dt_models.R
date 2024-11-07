setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")
source("./src/utils/binomial_corrected.R")

connectdB()
on.exit(disconnectdB())

name = 'dtinteractions'
table = 'aggregated'
link = 'logit'

response <- "permission_denied"

inter_all = c("image","power","negemo","bio",
              "work","posemo","home","tokencount",
              "affiliation","you","interrog","i")

for (inter_index in 9:length(inter_all)){
  inter_vars = inter_all[1:inter_index]
  predictors <- paste0(inter_vars, collapse = '*')
  print(paste0('Interactions: ',predictors))
  
  da <- make.data(response = response,
                  predictors = c(inter_vars,'N'),
                  table = table,
                  chunksize = 2 * 10^6)
  
  form <- make.formula(response, predictors)
  t1 <- Sys.time()
  model <- shglm2(form,
                  datafun = da,
                  family = binomial(link=link),
                  weights = ~N,
                  trace = TRUE,
                  sparse = FALSE)
  
  t2 <- Sys.time() - t1
  cat(t2,attr(t2,'units'))
  
  attr(model, "terms") <- terms(form)
  
  model_path = file.path("models",paste0(name,'_',table,'_',link,"_",inter_index,".model"))
  
  saveRDS(model, model_path)
}

print('Finished.')
