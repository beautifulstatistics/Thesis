validate_make_data <- function(data_func, response, predictors, max_chunks = Inf) {
  data_func(reset = TRUE)  # Reset the data function
  chunk_num <- 0
  total_rows <- 0
  issues <- list()
  
  while(TRUE) {
    chunk <- data_func()
    if (is.null(chunk)) break
    chunk_num <- chunk_num + 1
    total_rows <- total_rows + nrow(chunk)
    
    # Check if we've reached the maximum number of chunks to check
    if (chunk_num > max_chunks) break
    
    # 1. Check if all expected columns are present
    expected_cols <- c(response, predictors)
    missing_cols <- setdiff(expected_cols, names(chunk))
    if (length(missing_cols) > 0) {
      issues[[length(issues) + 1]] <- paste("Chunk", chunk_num, "is missing columns:", paste(missing_cols, collapse = ", "))
    }
    
    # 2. Check for NAs in response variable
    na_response <- sum(is.na(chunk[[response]]))
    if (na_response > 0) {
      issues[[length(issues) + 1]] <- paste("Chunk", chunk_num, "has", na_response, "NAs in response variable")
    }
    
    # 3. Check data types
    for (col in names(chunk)) {
      if (is.character(chunk[[col]])) {
        issues[[length(issues) + 1]] <- paste("Chunk", chunk_num, "column", col, "is character type")
      }
      if (!is.numeric(chunk[[col]]) && !is.factor(chunk[[col]]) && !is.logical(chunk[[col]])) {
        issues[[length(issues) + 1]] <- paste("Chunk", chunk_num, "column", col, "is not numeric, factor, or logical")
      }
    }
    
    # 4. Check for constant columns
    for (col in names(chunk)) {
      if (length(unique(chunk[[col]])) == 1) {
        issues[[length(issues) + 1]] <- paste("Chunk", chunk_num, "column", col, "is constant")
      }
    }
    
    # 5. Check for extremely high cardinality in factor columns
    for (col in names(chunk)) {
      if (is.factor(chunk[[col]]) || is.character(chunk[[col]])) {
        cardinality <- length(unique(chunk[[col]]))
        if (cardinality > 0.5 * nrow(chunk)) {
          issues[[length(issues) + 1]] <- paste("Chunk", chunk_num, "column", col, "has very high cardinality:", cardinality, "unique values")
        }
      }
    }
    
    # Print progress
    cat("Processed chunk", chunk_num, "\n")
  }
  
  # Reset the data function again
  data_func(reset = TRUE)
  
  # Return summary
  list(
    total_chunks = chunk_num,
    total_rows = total_rows,
    issues = issues
  )
}

setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")
time()

connectbB()
name <- 'highest'
table = 'all_data'

response <- c('permission_denied')
predictors <- models[[name]]

# Usage
dat <- make.data(response, predictors = predictors, table = table, chunksize = 1000000)
validation_result <- validate_make_data(dat, response, predictors)

# Print summary
cat("Processed", validation_result$total_chunks, "chunks,", validation_result$total_rows, "total rows\n")
if (length(validation_result$issues) == 0) {
  cat("No issues found.\n")
} else {
  cat("Found", length(validation_result$issues), "issues:\n")
  for (issue in validation_result$issues) {
    cat("- ", issue, "\n")
  }
}

disconnectdB()


##################

# setwd("~/Desktop/workingfast/Thesis")
# source("./src/utils/helper_functions.R")
# time()
# 
# connectbB()
# 
# name <- 'highest'
# table = 'all_data'
# 
# response <- c('permission_denied')
# predictors <- models[[name]]
# 
# select = c(response, predictors)
# select = paste0(select, collapse = ',')
# query = c("SELECT ", select," FROM ", table)
# query = paste(query, collapse = '')
# result <- dbSendQuery(conn, query)
# 
# len = 10
# count = 0
# while(len > 0){
#   rval <- dbFetch(result, n = 1000000)
#   len <- nrow(rval)
#   count <- count + 1
#   print(count)
# }
# 
# dbClearResult(result)
# 
# disconnectdB()
