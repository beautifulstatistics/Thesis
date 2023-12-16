library(brms)
options(mc.cores = 4)

ll = list()
for(file in list.files('models/aggregatedbayes', full.names = TRUE)){
  model <- readRDS(file)
  ll[[file]] = bridge_sampler(model)$logml
  
  print(file)
  hyps = rownames(fixef(model))
  hyps = hyps[hyps != 'Intercept']
  hyps = paste0(hyps,' = 0')
  print(hypothesis(model, hyps))
}

ll <- unlist(ll)
sort(ll/sum(ll))
