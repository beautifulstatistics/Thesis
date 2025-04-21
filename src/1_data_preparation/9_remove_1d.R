# build_query <- function(preds, path){
#   query1 <- paste0(
#     "\n\nWITH\n",
#     "subset AS (SELECT ", paste0(preds, collapse = ', '), ', ', response, ", N FROM ", table)
#   
#   if (length(path) > 0){
#     query1 <- paste0(query1, " WHERE ", paste0(path, collapse = ' AND '))
#   }
#   
#   query1 <- paste0(query1, "),\n\n")
#   
#   query2 <- paste0(
#     preds, " AS (SELECT '", preds, "' AS name, ",
#     preds, " as child, ", response,
#     " as response, SUM(N) AS count, count(*) as rows FROM subset GROUP BY ",
#     preds, ", ", response, ")",
#     collapse = ',\n'
#   )
#   
#   query <- paste0(query1, query2, '\n\n')
#   final_query <- paste0(query, paste0("SELECT * FROM ", preds, collapse = "\nUNION ALL\n"))
#   
#   return(final_query)
# }
# 
# 
# setwd("/home/kenneywl/Desktop/Thesis")
# source("./src/utils/helper_functions.R")
# 
# connectdB()
# on.exit(disconnectdB())
# 
# table <- "aggregated"
# response <- 'permission_denied'
# predictors <- models[['global']]
# 
# q <- build_query(predictors,c())
# 
# data <- dbGetQuery(conn,q)
# 
# saveRDS(data,file="./data.RDS")
# data <- readRDS("./data.RDS")
# 
# data$rows <- NULL
# 
# st = list()
# for (na in unique(data$name)){
#   da <- subset(data, name == na)
#   
#   observed <- matrix(da$count, nrow = 2, byrow = TRUE)
#   
#   row_totals <- rowSums(observed)
#   col_totals <- colSums(observed)
#   grand_total <- sum(observed)
#   
#   expected <- outer(row_totals, col_totals) / grand_total
#   
#   G_stat <- 2 * sum(observed * log(observed / expected), na.rm = TRUE)
#   
#   df <- (nrow(observed) - 1) * (ncol(observed) - 1)
#   p_value <- 1 - pchisq(G_stat, df)
#   
#   stat = list(G_statistic = G_stat, degrees_of_freedom = df, p_value = p_value)
#   st[[na]] = stat$p_value
# }

setwd("/home/kenneywl/Desktop/Thesis")
source("./src/utils/helper_functions.R")

connectdB()
on.exit(disconnectdB())

table <- "presence"
response <- 'permission_denied'
predictors <- models[['global']]

preds = c("feel","sad")

predictors = c(setdiff(predictors,preds),response)

aggregate.predictors.duplicates(predictors, "aggregated_1dremoved", table = table)

print('Finished')
