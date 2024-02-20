setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")
source('5.5_binomial_corrected.R')

connectbB()

dbExecute(conn, "DROP TABLE IF EXISTS test_full")
predictors = c('affect','functions','drives')
da <- make.data('permission_denied',predictors,table='presence', limit = 10**6)
da(T)
df = da(F)
df$permission_denied = sample(c(0,1), nrow(df), replace=TRUE)
dbWriteTable(conn, "test_full", df)
aggregate_predictors(predictors,'test_agg', source_table='test_full')

dffd <- make.data('permission_denied',predictors, table='test_full')
dffd(T)
dff = dffd(F)

dffa <- make.data(c('censored','not_censored'),c(predictors,'N','censored_proportion'), table='test_agg')
dffa(T)
dfa = dffa(F)

form <- make.formula('permission_denied',predictors)
g1 <- glm(form, dff, family = binomial())
g2 <- speedglm(form, dff, family = binomial())
g3 <- shglm(form, dffd, family = binomial())

form <- make.formula('censored_proportion', predictors)
g4 <- glm(form, dfa, family= binomial_corrected, weights=N)
g5 <- speedglm(form, dfa, family= binomial_corrected, weights=N)
g6 <- shglm2(form, dffa, family = binomial(), weights.fo=~N)

logLik(g1)
logLik(g2)
logLik(g3)
logLik(g4)
logLik(g5)
logLik(g6)

disconnectdB()
###############
