setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")
library(RSQLite)

connectdB()

response <- 'permission_denied'
predictors <- models[['global']]
table = 'presence'

aggregate.predictors.binomial(response,
                              predictors,
                              'aggregated_binomial',
                              table)

# aggregate.predictors.duplicates(c(response,predictors), 
#                                 'aggregated', 
#                                 table)

disconnectdB()

print('Table aggregated created')
print('Finished')