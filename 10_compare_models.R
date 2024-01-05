setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

ll = list(global = readRDS('models/presence/global.presencelogit')$aic)
for(name in c(names(models))){
  file <-  paste0('models/presence/', name,'/',name,'.logit')
  model <- readRDS(file)
  ll[[name]] = model$aic
  print(model$RSS/model$df)
}

ll <- unlist(ll)
sort(ll - min(ll))