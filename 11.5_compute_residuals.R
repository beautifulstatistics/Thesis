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

print('Starting Plotting...')
p = (
  ggplot(checks)
  + aes(x=predictedLogit,y=residualDeviance)
  + geom_point()
  + geom_abline(slope=1,intercept = 0)
  + xlab('Predicted Logit')
  + ylab('Deviance Residuals')
  + labs(title=paste0(name," Deviance Residuals Vs Linear Predicted"))
  + theme(plot.title = element_text(hjust = 0.5))
)

ggsave(paste0(path,'_Rdeviance_by_Plogit','.png'), plot = p)
print("Rdeviance Saved...")

p = (
  ggplot(checks)
  + aes(x=predictedLogit,y=actualProp)
  + geom_point()
  + geom_abline(slope=1,intercept = 0)
  + xlab('Predicted Values')
  + ylab('Actual Proportion')
  + labs(title=paste0(name," Actual Proportion Vs Linear Predicted"))
  + theme(plot.title = element_text(hjust = 0.5))
)

ggsave(paste0(path,'_Aprop_by_PLogit','.png'), plot = p)
print('Apropr by Plogit Saved...')

long_data <- do.call(rbind,
                     lapply(predictors, function(name) {
                       data.frame(Predictor = name,
                                  residualDeviance = checks$residualDeviance[checks[[name]] == 1])
                     }))

p <- ggplot(checks_long, aes(x = Predictor, y = residualDeviance)) +
  geom_boxplot() +
  xlab('Predictor') +
  ylab('Deviance Residuals') +
  labs(title = "Deviance Residuals vs. Predictors") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(path, 'All_Predictors_Deviance_Residuals.png'), plot = p)

print("All Plots Saved.")

disconnectdB()