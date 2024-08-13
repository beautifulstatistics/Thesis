setwd("~/Desktop/working8/Thesis")
source("./src/utils/helper_functions.R")
library(ggplot2)

connectbB()

name = 'lowest'
response = c('censored','N')
predictors = models[[name]]

checks <- dbGetQuery(conn, paste0("SELECT censored,N FROM ",name))

path <- file.path('results', 'models', 'presence')
file <- file.path(path, name, name,'.agg_logit')
model <- readRDS(file)

p2logit <- function(top,bottom){
  log(top) - log(bottom) - log(bottom - top) + log(bottom)
}

da <- make.data(response, predictors, table=name)
checks$predictedLogit = all_linear_predict.shglm(model, da)
checks$actualLogit = with(checks, p2logit(censored,N))
checks$predictedProp = all_predict.shglm(model, da)
checks$actualProp = checks$censored/checks$N
checks$actualLogit = with(checks, p2logit(censored,N))
checks$residualDeviance = with(checks, deviance_resid(actualProp,predictedProp,N))

che = checks[checks$censored != 0,]

p = (
  ggplot(che)
  + aes(x=predictedLinear,y=actualLogit)
  + geom_point()
  + geom_abline(slope=1,intercept = 0)
  + xlab('Predicted Values')
  + ylab('Deviance Residuals')
  + labs(title=paste0(name," Deviance Residuals Vs Linear Predicted"))
  + theme(plot.title = element_text(hjust = 0.5))
)

ggsave(paste0(path, name,'/',name,'.png'), plot = p)

disconnectdB()