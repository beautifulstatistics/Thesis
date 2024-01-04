setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

connectbB()

actual <- dbGetQuery(conn, "SELECT permission_denied FROM presence limit 1000")

path <- 'models/presence/'

for(name in names(models)) {
  print(name)
  file <- paste0(path, name, '/', name, '.logit')
  
  model <- readRDS(file)
  da <- make.data('permission_denied', predictors, table='presence', chunksize = 50*10^6)
  actual$predicted = all_predict.shglm(model, da)
  
  actual$residual <- with(actual, PEARSON_RESID(permission_denied, predicted))
  model$dispersion <- with(actual, DISP(model,residual))
  
  saveRDS(model, paste0(path, name, '/', name,'.logit'))
  
  df <- with(actual, bin.residuals(predicted, permission_denied))
  
  p = (
    ggplot(df)
    + aes(x=predicted,y=residuals)
    + geom_point()
    + xlab('Predicted Values')
    + ylab('Binned Residuals')
    + labs(title=paste0(name," Binned Residuals Vs Predicted"))
    + theme(plot.title = element_text(hjust = 0.5))
  )
  
  ggsave(paste0(path, name,'/',name,'.png'), plot = p)
  break
}

disconnectdB()