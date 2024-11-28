setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")
source("./src/utils/binomial_corrected.R")

connectdB()

dbExecute(conn, "DROP TABLE IF EXISTS test_full")
predictors = c('affect','functions','drives','tokencount')
da <- make.data('permission_denied',predictors,table='presence', limit = 1000)
da(T)
df = da(F)
df$permission_denied = sample(c(0,1), nrow(df), replace=TRUE)
dbWriteTable(conn, "test_full", df)
aggregate.predictors.binomial('permission_denied',predictors,'test_agg', source_table='test_full')

dfull <- make.data('permission_denied',predictors, table='test_full')
dfull(T)
dfullall = dfull(F)

daggdup <- make.data('permission_denied',c(predictors,'N'), table='test_agg_dup')
daggdup(T)
daggdupall = daggdup(F)

dagg <- make.data(c('censored','not_censored'),c(predictors,'N','censored_proportion'), table='test_agg')
dagg(T)
daggall = dagg(F)

aggregate.predictors.duplicates(c('permission_denied',predictors),
                                'test_agg_dup','test_full')


form <- make.formula('permission_denied',predictors)
g1 <- glm(form, dfullall, family = binomial())
g2 <- speedglm(form, dfullall, family = binomial())
g3 <- shglm(form, dfull, family = binomial())

g4 <- glm(form, daggdupall, family= binomial(), weights=N)
g5 <- speedglm(form, daggdupall, family= binomial_corrected, weights=N)
g6 <- shglm2(form, daggdup, family = binomial(), weights.fo=~N)

form <- make.formula('censored_proportion', predictors)
g7 <- glm(form, daggall, family= binomial_corrected, weights=N)
g8 <- speedglm(form, daggall, family= binomial_corrected, weights=N)
g9 <- shglm2(form, dagg, family = binomial(), weights.fo=~N)


logLik(g1)
logLik(g2)
logLik(g3)
logLik(g4)
logLik(g5)
logLik(g6)
logLik(g7)
logLik(g8)
logLik(g9)


disconnectdB()
###############



