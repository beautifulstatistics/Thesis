setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")

connectdB()

name <- 'global'
table <- 'presence'
link <- 'rpart'

response <- "permission_denied"
predictors <- models[[name]]

# vars <- c(response,c(predictors,'N'))
vars <- c(response,c(predictors))
dat <- make.data(vars, table=table,chunksize=ceiling(226841249/10)) # 22.7 * 10^6 = 226841249 --- floor(72571493/2)
response_form <- 'permission_denied' # SELECT AVG(predictedProp - permission_denied), count(*) FROM numeric GROUP BY bin
form <- make.formula(response_form, predictors)

rmodels = list()
i = 0
dat(TRUE)
da = dat()
while(!is.null(da)){
  i = i + 1
  print(paste0('Rpart Partition: ',i))
  rmodels[[i]] <- rpart(form, data=da, method='class', y=FALSE)
  rm(da)
  da = dat()
}

print('models fit')
file <-  file.path('models',paste0(name,'_',table,'_',link,'.model'))
saveRDS(rmodels,file=file)
print('models saved.')

disconnectdB()
