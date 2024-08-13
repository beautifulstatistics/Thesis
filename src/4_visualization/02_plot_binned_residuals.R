setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
library(ggplot2)

checks <- read.csv('./models/presence/lowest/binned_quantile_logit.csv')

print('Starting Plotting...')
p = (
  ggplot(checks)
  + aes(x=predicted,y=residuals)
  + geom_point()
  + xlab('Predicted')
  + ylab('Binned Residuals')
  + labs(title="Residuals Vs Predicted")
  + theme(plot.title = element_text(hjust = 0.5))
)

p


ggsave(paste0(path,'_binned_RvsP','.png'), plot = p)
print("Rdeviance Saved...")

checks <- read.csv('./models/presence/lowest/binned_range_logit.csv')

print('Starting Plotting...')
p = (
  ggplot(checks)
  + aes(x=predicted,y=residuals)
  + geom_point()
  + xlab('Predicted')
  + ylab('Binned Residuals')
  + labs(title="Residuals Vs Predicted")
  + theme(plot.title = element_text(hjust = 0.5))
)

p



##############################################


path <- paste0('./models/presence/', name)
checks <- read.csv(paste0(path,'/preds.csv'))

path <- paste0(path,'/binned_plots/preds')
for(i in c(25,200,1000,5000,10000,15000,20000)){
  resid <- bin.residuals.range(checks$predictedProp, checks$actual,nbin=i)
  p = (
    ggplot(resid)
    + aes(x=predicted,y=residuals)
    + geom_point()
    + xlab('Predicted')
    + ylab('Binned Residuals')
    + labs(title=paste0("Residuals Vs Predicted: ",i,' bins'))
    + theme(plot.title = element_text(hjust = 0.5))
  )
  
  ggsave(paste0(path,i,'.png'), plot = p, width = 8, height = 6, units = "in")
  print(i)
}
