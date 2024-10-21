setwd("~/Desktop/working8/Thesis")
source("./src/utils/helper_functions.R")
library(RSQLite)

connectdB()

response <- 'permission_denied'
predictors <- models[['global']]

dbExecute(conn, "DROP TABLE IF EXISTS aggregated")

create_query <- "CREATE TABLE aggregated (
    ID INTEGER PRIMARY KEY AUTOINCREMENT,
    N INTEGER"

schema <- c(response,predictors)
for (var in schema) {
  create_query <- paste0(create_query, sprintf(", %s INTEGER", var))
}
create_query <- paste0(create_query, ")")
dbExecute(conn, create_query)

insert_query <- "INSERT INTO aggregated ("
insert_query <- paste0(insert_query, paste0(schema, collapse = ", "), ", N)")
insert_query <- paste0(insert_query,
                       paste0(" SELECT ",
                       paste0(schema,collapse = ', '),
                       ", COUNT(*) AS N FROM presence GROUP BY ",
                       paste0(schema,collapse = ', '), " ORDER BY RANDOM()"
                       ))
dbExecute(conn, insert_query)

disconnectdB()

print('Table aggregated created')
print('Finished')