setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
library(speedglm)

functions_pronoun = c('ppron','ipron','tokencount')

response = c('censored','not_censored')
da = make.data(response,functions_pronoun, table='functions_pronoun')
form <- make.formula(response,functions_pronoun)

model1 <- glm(form, data=data,family=binomial_corrected)
logLik(model1)

model <- speedglm(form, data=data,family=binomial())
logLik(model)



summary(model)
logLik(model)
AIC(model)

path = 'models/presence/functions_pronoun/functions_pronoun.logit'
mo = readRDS(path)
summary(mo)
logLik(mo)
AIC(mo)

speedglm:::ll.speedglm

mo$aic
model$aic

