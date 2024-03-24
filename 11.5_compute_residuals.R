setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
library(ggplot2)

connectbB()

name = 'lowest'
response = c('censored','N')
predictors = models[[name]]

checks <- dbGetQuery(conn, paste0("SELECT censored,N FROM ",name))

path <- paste0('models/presence/', name,'/',name)
file <- paste0(path,'.agg_logit')
model <- readRDS(file)

print('Starting calculations...')
da <- make.data(response, predictors, table=name)
checks$predictedLogit = all_linear_predict.shglm(model, da)
checks$predictedProp = all_predict.shglm(model, da)
checks$actualLogit = with(checks, p2logit(censored,N))
checks$actualProp = checks$censored/checks$N
checks$residualDeviance = with(checks, deviance_resid(actualProp,predictedProp,N))

write.csv(checks,file=paste0(path,'.csv'))

disconnectdB()