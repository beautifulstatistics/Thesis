setwd("~/Desktop/working8/Thesis")
source("./scripts/helper/functions.R")

connectbB()

for(name in names(models)){
  t1 <- Sys.time()
  preds <- models[[name]]
  aggregate_predictors(preds,name)
  print(Sys.time()-t1)
}

print(dbListTables(conn))

disconnectdB()
