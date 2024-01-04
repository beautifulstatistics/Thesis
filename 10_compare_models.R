setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

# FOR BAYES MODELS

# library(brms)

# ll = list()
# for(name in names(models)){
#   file <-  paste0('models/aggregatedbayes/',name,'/',name,'.model')
#   model <- readRDS(file)
#   
#   print(summary(model))
#   ll[[name]] = bridge_sampler(model)$logml
# 
#   hyps = rownames(fixef(model))
#   hyps = hyps[hyps != 'Intercept']
#   hyps = paste0(hyps,' = 0')
#   print(hypothesis(model, hyps))
# }
# 
# ll <- unlist(ll)
# rev(sort(ll/sum(ll)))

# FOR CLASSIC MODELS

# ll = list()
# for(name in names(models)){
#   file <-  paste0('models/aggregated/',name,'/',name,'.model')
#   model <- readRDS(file)
#   ll[[name]] = model$aic
# }
# 
# ll <- unlist(ll)
# sort(ll)

# FOR PRESENCE MODELS

ll = list(global = readRDS('models/presence/global.presencelogit')$aic)
for(name in c(names(models))){
  file <-  paste0('models/presence/', name,'/',name,'.logit')
  model <- readRDS(file)
  ll[[name]] = model$aic
  print(model$RSS/model$df)
}

ll <- unlist(ll)
sort(ll - min(ll))



