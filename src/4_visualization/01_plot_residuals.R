setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
library(ggplot2)

name = 'all_data'
predictors = models[[name]]

path <- paste0('models/presence/', name,'/')
checks <- read.csv(paste0(path,name,'.csv'))

print('Starting Plotting...')
p = (
  ggplot(checks)
  + aes(x=predictedLogit,y=residualDeviance)
  + geom_point()
  + xlab('Predicted Logit')
  + ylab('Deviance Residuals')
  + labs(title=paste0(name," Deviance Residuals Vs Linear Predicted"))
  + theme(plot.title = element_text(hjust = 0.5))
)

ggsave(paste0(path,'_Rdeviance_by_Plogit','.png'), plot = p)
print("Rdeviance Saved...")

# 
# p = (
#   ggplot(checks)
#   + aes(x=predictedLogit,y=actualProp)
#   + geom_point()
#   + geom_abline(slope=1,intercept = 0)
#   + xlab('Predicted Values')
#   + ylab('Actual Proportion')
#   + labs(title=paste0(name," Actual Proportion Vs Linear Predicted"))
#   + theme(plot.title = element_text(hjust = 0.5))
# )

# ggsave(paste0(path,'_Aprop_by_PLogit','.png'), plot = p)
# print('Apropr by Plogit Saved...')

# connectbB()
# 
# for(predictor in predictors){
#   checks$predictor <- dbGetQuery(conn, sprintf("SELECT %s FROM %s", predictor, name))[[1]]
#   checks$predictor_name <- predictor
# 
#   checks_sam <- checks[sample(1:dim(checks)[1],10000,replace=F),]
# 
#   p <- ggplot(checks_sam, aes(x = factor(predictor), y = residualDeviance)) +
#     geom_boxplot() +
#     labs(x = 'Predictor', y = 'Deviance Residuals', title = paste("Deviance Residuals vs.", predictor)) +
#     theme(plot.title = element_text(hjust = 0.5))
# 
#   ggsave(filename = paste0(path, 'breakdown/' ,predictor, '_deviance_residuals.png'), plot = p, width = 8, height = 6)
# }
# 
# print("All Plots Saved.")
# 
disconnectdB()