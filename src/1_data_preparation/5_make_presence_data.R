setwd("~/Desktop/working8/Thesis")
source("./src/utils/helper_functions.R")
library(RSQLite)

connectbB()

query <- "SELECT tokencount FROM all_data"
data <- dbGetQuery(conn, query)
median_value <- median(data$tokencount)

schema <- dbListFields(conn, 'all_data')
schema <- schema[!(schema %in% c('tokencount', 'permission_denied','text','text_raw'))]

dbExecute(conn, "DROP TABLE IF EXISTS presence")

create_query <- "CREATE TABLE presence (
    ID INTEGER PRIMARY KEY AUTOINCREMENT,
    tokencount INTEGER,
    permission_denied INTEGER"

for (var in schema) {
  create_query <- paste(create_query, sprintf(", %s INTEGER", var))
}
create_query <- paste(create_query, ")")
dbExecute(conn, create_query)

insert_query <- "INSERT INTO presence (tokencount, "
insert_query <- paste(insert_query, paste(schema, collapse = ", "), ", permission_denied)")
insert_query <- paste(insert_query, "SELECT")

insert_query <- paste(insert_query, sprintf("
    CASE
        WHEN tokencount < %s THEN 0
        ELSE 1
    END,", median_value))

for (var in schema) {
  insert_query <- paste(insert_query, sprintf("
    CASE
        WHEN %s = 0 THEN 0
        ELSE 1
    END,", var))
}

insert_query <- paste(insert_query, " permission_denied FROM all_data")
dbExecute(conn, insert_query)

disconnectdB()

print('Table presence created')
print('Finished')