setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(RSQLite)
library(ggplot2)

connectbB()

######### Look at levels

predictors <- c("function_", "othergram", "social", "percept", "persconc", 
                'drives', 'affect', 'cogproc', 'bio', 'relativ', 'informal', 'image')

response = 'permission_denied'

name = 'function_'

query =  paste0("SELECT ",
                name, ", ",
                "SUM(permission_denied) as ", name, "C, ",
                "COUNT(*) - SUM(permission_denied) as ", name, "NC ",
                "FROM all_data ",
                "GROUP BY ", name)

data = dbGetQuery(conn,query)
data

######## MAKE PRESENCE MODEL

query <- "CREATE TABLE presence_hf AS SELECT"

for (var in predictors) {
  query <- paste(query, sprintf("
    CASE
        WHEN %s = 0 THEN 0
        ELSE 1
    END as %s,", var, var))
}

query <- paste(query, "permission_denied FROM all_data")
dbExecute(conn, query)

query <- "
  CREATE TABLE aggregated_hf AS 
  SELECT function_, othergram, social, percept, persconc, 
           drives, affect, cogproc, bio, relativ, informal, image 
         SUM(permission_denied) as censored,
         COUNT(*) - SUM(permission_denied) as not_censored 
  FROM presence_hf
  GROUP BY function_, othergram, social, percept, persconc, 
           drives, affect, cogproc, bio, relativ, informal, image
"

dbExecute(conn, query)

####################













