setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")
library(RSQLite)

connectdB()
on.exit(disconnectdB())

response <- 'permission_denied'
predictors <- models[['global']]
table = 'presence'

aggregate.predictors.binomial(response,
                              predictors,
                              'aggregated_binomial',
                              table)

print('Table aggregated created')

aggregate.predictors.duplicates(c(response,predictors),
                                'aggregated',
                                table)

print('Table aggregated_binomial created')
print('Finished')