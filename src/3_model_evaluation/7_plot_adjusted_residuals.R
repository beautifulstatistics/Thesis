setwd("/home/kenneywl/Desktop/Thesis")
source("./src/utils/helper_functions.R")

library(ggplot2)
connectbB()
table = 'presence'

preds <- make.data.all('ApredictedProp','permission_denied',table=table)
df <- bin.residuals(predicted = preds$ApredictedProp,
                      actual = preds$permission_denied)
  
p <- (
    ggplot(df)
    + aes(x=predicted,y=residuals)
    + geom_point()
    + ggtitle("Adjusted")
  )
fp = file.path('evaluation',paste0("adjusted",'.png'))
ggsave(fp,p)

disconnectdB()
