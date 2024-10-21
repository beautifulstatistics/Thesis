setwd("~/Desktop/working8/Thesis")
source("./src/utils/helper_functions.R")

connectbB()

name = 'global'
table = 'presence'
link = 'logit'

model_path = file.path('models', paste0(name,'_',table,'_',link,'_adjustedA99.model'))
model <- readRDS(model_path)

predictors <- attr(model$tf,'variables')
predictors <- paste0(predictors)[3:length(predictors)]

preds <- make.data.all(predictors,table=table,where="predictedProp > .009")

inter <- names(preds)[colMeans(preds) > .99]
inter <- paste0(inter,collapse = '*')
print(inter) 

# .99  "tokencount*functions*social*drives*cogproc"
# .985 "tokencount*functions*social*drives*affect*cogproc*informal*adverb*power"
# .98  "tokencount*functions*social*drives*affect*cogproc*informal*adverb*particle*power*negemo"

all_vars = c("tokencount","functions","social","drives","affect","cogproc","informal","adverb","particle","power","negemo")
inter <- combn(all_vars, 2, paste, collapse=":")
inter

disconnectbB()
