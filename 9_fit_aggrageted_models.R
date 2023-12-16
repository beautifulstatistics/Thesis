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
  
  prior = prior(double_exponential(0,1), class = b)
  model <- brm(censored | trials(censored + not_censored) ~ ., 
               data=da, 
               family=beta_binomial(),
               sample_prior = "yes",
               save_pars = save_pars(all = TRUE),
               prior = prior)

  saveRDS(model, file = paste0('models/aggregatedbayes/',name,'.model'))
  counter = counter + 1
  print(paste0(name, ' finished.'))
  print(length(tables) - counter)
}

disconnectdB()

