setwd("/home/kenneywl/Desktop/Thesis")
source("./src/utils/helper_functions.R")
library(RSQLite)

connectdB()

response <- 'permission_denied'
predictors <- models[['global']]

dbExecute(conn, "DROP TABLE IF EXISTS numeric")

create_query <- "CREATE TABLE numeric (
    ID INTEGER PRIMARY KEY AUTOINCREMENT"

schema <- c(response,predictors)
for (var in schema) {
  create_query <- paste0(create_query, sprintf(", %s INTEGER", var))
}
create_query <- paste0(create_query, ")")
dbExecute(conn, create_query)

insert_query <- "INSERT INTO numeric ("
insert_query <- paste0(insert_query, paste0(schema, collapse = ", "), ")")
insert_query <- paste0(insert_query, paste0(" SELECT ", paste0(schema,collapse = ', ') ," FROM all_data"))
dbExecute(conn, insert_query)

disconnectdB()

print('Table numeric created')
print('Finished')