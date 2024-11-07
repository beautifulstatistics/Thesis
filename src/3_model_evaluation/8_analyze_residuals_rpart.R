setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")

library(rpart)
library(ggplot2)

m1 <- readRDS(file.path('models','decisiontree','global_numeric_logit_rpart_residual.model'))[[1]]
m2 <- readRDS(file.path('models','decisiontree','global_presence_logit_rpart_residualSensitive.model'))[[1]]
rpart.getimp(m1)

# work      drives    persconc       anger      adverb      negate        they       swear      nonflu affiliation 
# 0.2418609   0.4827035   0.6930005   0.8732550   0.9038007   0.9323101   0.9526723   0.9724120   0.9915290   1.0000000 

m2 <- readRDS(file.path('models','decisiontree','global_presence_logit_rpart_residualSensitive.model'))[[1]]
m2c <- prune.cp(m2)
rpart.getimp(m2c)

# negemo      drives  tokencount        they     cogproc       swear       anger      social        work      posemo 
# 0.1760431   0.3473073   0.5000504   0.6278362   0.7149260   0.7624849   0.8040989   0.8437234   0.8744384   0.8950114 
# image     specart      reward affiliation    persconc          we      differ      female       ipron      negate 
# 0.9126463   0.9296011   0.9433891   0.9547001   0.9654703   0.9754191   0.9824885   0.9878487   0.9924805   0.9962946 
# hear 
# 1.0000000 

df <- data.frame(residuals=residuals(m2c),predicted=predict(m2c))

p <- (
  ggplot(df)
  + aes(x=predicted,y=residuals)
  + geom_point()
)
p


rpart.get_leaf_variables(m2c)

# > get_leaf_variables(m2c)
# [1] "negemo*work*posemo*affiliation*female*cogproc"
# [2] "negemo*swear*we*ipron"                        
# [3] "negemo*swear*drives"
