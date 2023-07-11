setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(RSQLite)
# library(ggplot2)

connectbB()

######### Look at levels

# excluding tokencount
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