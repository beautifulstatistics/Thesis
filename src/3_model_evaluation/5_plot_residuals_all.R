setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")

library(ggplot2)

connectdB()

folder <- 'evaluationADT'
predicted <- 'predictedPropADT'

preds <- make.data.all(predicted,'permission_denied',table='presence')
dir.create(folder, showWarnings = FALSE)
default <- floor(sqrt(nrow(preds)))
for(i in c(default,seq(1000, 20000, by = 1000))){
  print(i)
  df <- bin.residuals(predicted = preds$predictedProp,
                     actual = preds$permission_denied,
                     nbins = i)
  
  p <- (
    ggplot(df)
    + aes(x=predicted,y=residuals)
    + geom_point()
    + ggtitle(i)
  )
  
  fp = file.path(folder,paste0(i,'.png'))
  ggsave(fp,p)
}

print('Finished.')

disconnectdB()






