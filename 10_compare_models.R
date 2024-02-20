setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

ll = list()
for(name in names(models)){
  file <-  paste0('models/presence/', name,'/',name,'.agg_logit')
  model <- readRDS(file)
  ll[[name]] = model$aic
  print(paste0(name,' ',model$RSS/model$df))
}

ll <- unlist(ll)
sort(ll - min(ll))

