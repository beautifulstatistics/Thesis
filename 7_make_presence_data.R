setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(RSQLite)

connectbB()

predictors <- c("tokencount", "function_", "othergram", "social", "percept", "persconc", 
                'drives', 'affect', 'cogproc', 'bio', 'relativ', 'informal', 'image')

query <- "SELECT tokencount FROM all_data"
data <- dbGetQuery(conn, query)
median_value <- median(data$tokencount)

print("Median of tokencount found.")

query <- "CREATE TABLE presence_hf AS SELECT"

query <- paste(query, sprintf("
    CASE
        WHEN tokencount < %s THEN 0
        ELSE 1
    END as tokencount,", median_value))

for (var in predictors) {
  query <- paste(query, sprintf("
    CASE
        WHEN %s = 0 THEN 0
        ELSE 1
    END as %s,", var, var))
}

query <- paste(query, "permission_denied FROM all_data")
dbExecute(conn, query)

print('Table presence_hf made.')

query <- "
  CREATE TABLE aggregated_hf AS 
  SELECT tokencount, function_, othergram, social, percept, persconc, 
           drives, affect, cogproc, bio, relativ, informal, image, 
         SUM(permission_denied) as censored,
         COUNT(*) - SUM(permission_denied) as not_censored 
  FROM presence_hf
  GROUP BY tokencount, function_, othergram, social, percept, persconc, 
           drives, affect, cogproc, bio, relativ, informal, image
"

dbExecute(conn, query)

print('Table aggregated_hf made.')

####################

disconnectdB()




