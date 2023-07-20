setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
library(brms)
options(mc.cores = 4)

connectbB()

tables = dbListTables(conn)
tables = tables[!(tables %in% c('presence','all_data'))]

dir.create('models',showWarnings = FALSE)
dir.create('models/aggregatedbayes',showWarnings = FALSE)

counter = 0
for(name in tables){
  da = dbGetQuery(conn, paste0("SELECT * FROM ",name))
  
  to_int = names(da)[!(names(da) %in% c('tokencount','censored','not_censored'))]
  factors = combn(to_int, 2, FUN = function(x) paste0(x,collapse=':'))
  factors = c(to_int, factors)
  form = paste0(factors, collapse = '+')
  form = formula(paste0('censored | trials(censored + not_censored) ~ tokencount + ', form))
  
  prior = prior(normal(0,1), class = b)
  model <- brm(form, 
               data=da, 
               family=beta_binomial(),
               sample_prior = "yes",
               save_pars = save_pars(all = TRUE),
               prior = prior)

  saveRDS(model, file = paste0('models/aggregatedbayes/',name,'.model'))
  print(paste0(name, ' finished.'))
}

disconnectdB()

