setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
library(ggplot2)

connectbB()

name = 'lowest'
path <- paste0('./models/presence/', name,'/')
file <- paste0(path,'/lowest','.logit')
model <- readRDS(file)

print('Getting Response...')
actual <- dbGetQuery(conn, "SELECT permission_denied FROM presence")$permission_denied

print('Starting calculations...')
da <- make.data('permission_denied', models[[name]], table='presence')
predictedProp = all_predict.shglm(model, da)

print("saving preds")
write.csv(data.frame(actual,predictedProp),file=paste0(path,'/preds.csv'))

print('Starting quantile resid...')
resid = bin.residuals.quantile(predictedProp, actual)
print("Saving quantile resid...")
write.csv(resid,file=paste0(path,'/binned_quantile_logit.csv'))

print('Starting range resid...')
resid = bin.residuals.range(predictedProp, actual)
print("Saving range resid..")
write.csv(resid,file=paste0(path,'/binned_range_logit.csv'))

disconnectdB()