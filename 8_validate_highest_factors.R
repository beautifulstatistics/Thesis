setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(speedglm)
library(ggplot2)

connectbB()

######### COMPUTE VIF
predictors <- c("function_","othergram","social","percept","persconc",'drives','affect')
predictors <- c(predictors, c("cogproc","bio","relativ","informal","image"))

print(vif(predictors, table = 'all_data', limit = NULL))

######### LINEARITY IN PREDICTORS
library(arm)

# DO Box-Tidwell

predictors_bt = paste0(paste0(predictors,paste0('*log(1+', predictors,')')),collapse=' + ')
response = 'permission_denied'

da = make.data(response, predictors, table = 'all_data')
form <- prep.formula(response, predictors_bt)

t1 = Sys.time()
glm1 <- shglm(form, data=da, family=binomial(), maxit=10**6, trace=TRUE)
print(Sys.time()-t1)
summary(glm1)

saveRDS(glm1,file='models/highest_factors_Box-Tidwell.model')

######### 
model <- readRDS("models/highest_factors.model")
data <- dbGetQuery(conn, "SELECT * FROM all_data ORDER BY RANDOM() LIMIT 10000")
data$lp = linear_predictors.shglm(model, data)
data$predicted <- predict.shglm(model, data)
data$residual <- data$predicted - data$permission_denied
br <- bin.residuals(data$predicted,data$permission_denied, nbins = 10)

for(name in predictors){
  p1 <- (
    ggplot(data)
    + aes(x=function_)
    + geom_point(aes(y=permission_denied))
  )
  print(p1)
  
  ggsave(filename = name, plot = p1)
}



disconnectdB()



