setwd("~/Desktop/working8/Thesis")
source("./src/utils/helper_functions.R")

library(ggplot2)
connectbB()

da <- make.data('predictedProp','permission_denied',table='presence')
preds <- make.data.all(da)

for(i in c(0,seq(1000, 20000, by = 1000))){
  print(i)
  df = bin.residuals(predicted = preds$predictedProp,
                     actual = preds$permission_denied,
                     nbins = i)
  
  
  p = (
    ggplot(df)
    + aes(x=predicted,y=residuals)
    + geom_point()
    + ggtitle(i)
  )
  
  fp = file.path('evaluation',paste0(i,'.png'))
  ggsave(fp,p)
}

print('Finished.')

disconnectdB()

# USING 16000

df <- dbGetQuery(conn,"SELECT * FROM presence WHERE predictedProp > .009")









