setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")

time()
connectbB()

name <- 'global'
table <- 'presence'

vs <- vif(models[[name]], table)

print(vs)

disconnectdB()
time()
