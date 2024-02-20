setwd("~/Desktop/working8/Thesis")
source("5_functions.R")
library(RSQLite)

connectbB()

query <- "SELECT tokencount FROM all_data"
data <- dbGetQuery(conn, query)
median_value <- median(data$tokencount)

schema <- dbGetQuery(conn, "SELECT * FROM all_data LIMIT 0")
schema <- names(schema)
schema <- schema[!(schema %in% c('tokencount', 'permission_denied','text','text_raw'))]

dbExecute(conn, "DROP TABLE IF EXISTS presence")
query <- "CREATE TABLE presence AS SELECT"
query <- paste(query, sprintf("
      CASE
          WHEN tokencount < %s THEN 0
          ELSE 1
      END as tokencount,", median_value))

for (var in schema) {
  query <- paste(query, sprintf("
      CASE
          WHEN %s = 0 THEN 0
          ELSE 1
      END as %s,", var, var))
}

query <- paste(query, "permission_denied FROM all_data")
dbExecute(conn, query)

print('Table presence made.')

disconnectdB()

