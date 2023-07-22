library(brms)
options(mc.cores = 4)

dir.create('models/aggregatedBRMs/residuals',showWarnings = FALSE)
dir.create('models/aggregatedBRMs/plots',showWarnings = FALSE)

ll = list()
for(file in list.files('models/aggregatedbayes', full.names = TRUE)){
  model <- readRDS(file)
  ll[[file]] = bridge_sampler(model)$logml
  break
}

pp_check(model)
