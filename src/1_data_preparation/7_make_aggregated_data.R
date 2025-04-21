setwd("/home/kenneywl/Desktop/Thesis")
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

dbExecute(conn, "CREATE TABLE aggregated_binomial_test AS
WITH 
positives AS (SELECT * FROM aggregated_binomial WHERE POSITIVE > 0),
negatives AS (SELECT * FROM aggregated_binomial WHERE POSITIVE <= 0 ORDER BY TOTAL DESC LIMIT (SELECT COUNT(*) FROM positives))

SELECT * FROM positives
UNION ALL 
SELECT * FROM negatives
          ")


print('Table aggregated binomial and test created')

aggregate.predictors.duplicates(c(response,predictors),
                                'aggregated',
                                table)

print('Table aggregated created')
print('Finished')