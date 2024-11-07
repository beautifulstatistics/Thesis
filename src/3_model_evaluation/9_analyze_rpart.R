setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")

library(rpart)

models <- readRDS(file.path('models','decisiontree','global_presence_rpart.model'))

for (i in 1:length(models)){
  print(paste0("Model: ",i))
  vars = rpart.get_leaf_variables(models[[i]])
  print(vars)
}