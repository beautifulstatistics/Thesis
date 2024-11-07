# Function to calculate deviance
calculate_deviance <- function(counts) {
  if (length(counts) < 2 || sum(counts) == 0) return(0)
  p <- counts / sum(counts)
  eps <- 1e-10
  -2 * sum(counts * ifelse(p > 0, p * log(p + eps), 0))
}

# Function to get the best column for splitting with added response storage
get_best_column <- function(response, predictors, table, path = c()) {
  on.exit(disconnectdB())
  
  build_query <- function(preds, path){
    query1 <- paste0(
      "\n\nWITH\n",
      "subset AS (SELECT ", paste0(preds, collapse = ', '), ', ', response, ", N FROM ", table)
    
    if (length(path) > 0){
      query1 <- paste0(query1, " WHERE ", paste0(path, collapse = ' AND '))
    }
    
    query1 <- paste0(query1, "),\n\n")
    
    query2 <- paste0(
      preds, " AS (SELECT '", preds, "' AS name, ", 
      preds, " as child, ", response, 
      " as response, SUM(N) AS count, count(*) as rows FROM subset GROUP BY ", 
      preds, ", ", response, ")",
      collapse = ',\n'
    )
    
    query <- paste0(query1, query2, '\n\n')
    final_query <- paste0(query, paste0("SELECT * FROM ", preds, collapse = "\nUNION ALL\n"))
    
    return(final_query)
  }
  
  tryCatch({
    connectdB()
    query <- build_query(predictors, path)
    
    time_start <- Sys.time()
    data <- dbGetQuery(conn, query)
    time_taken <- Sys.time() - time_start
    
    if (nrow(data) == 0) {
      stop("Query returned no data")
    }
    
    first_feature_data <- subset(data, name == data$name[1])
    parent <- aggregate(count ~ response, first_feature_data, sum)
    parent_deviance <- calculate_deviance(parent$count)
    
    results <- data.frame()
    for (feature in unique(data$name)) {
      fdata <- subset(data, name == feature)
      
      deviances <- 0
      for (value in unique(fdata$child)){
        sdata <- subset(fdata, child == value)
        sdeviance <- calculate_deviance(sdata$count)
        deviances <- deviances + sdeviance * sum(sdata$count)
      }
      
      split_deviance <- deviances / sum(fdata$count)
      
      results <- rbind(results, data.frame(
        feature = feature,
        deviance = split_deviance,
        deviance_improvement = parent_deviance - split_deviance
      ))
    }
    
    results <- results[order(results$deviance),]
    best_column <- results[1, 'feature']
    unique_values <- unique(subset(data, name == best_column)$child)
    split_deviance <- results[1, 'deviance']
    deviance_improvement <- results[1, 'deviance_improvement']
    subdata <- subset(data, name == best_column)
    
    responses_by_split <- aggregate(count ~ child + response, subdata, sum)
    split_responses <- aggregate(count ~ response, subdata, sum)
    
    return(
      list(
        best_column = best_column,
        unique_values = unique_values,
        parent_deviance = parent_deviance,
        split_deviance = split_deviance,
        deviance_improvement = deviance_improvement,
        time = time_taken,
        path = path,
        subdata = subdata,
        response = split_responses,
        responses_by_split = responses_by_split,
        intermediate = list(
          query = query,
          fetched_data = data,
          results_table = results
        ),
        is_leaf = FALSE
      )
    )
  }, error = function(e) {
    error_msg <- paste("Error in get_best_column:", e$message)
    warning(error_msg)
    
    return(list(
      error = TRUE,
      message = error_msg,
      time = difftime(Sys.time(), time_start, units = "secs")
    ))
  })
}

make_leaf <- function(best_column, env){
  first_feature_data <- subset(best_column$subdata, name == best_column$best_column)
  parent <- aggregate(count ~ response, first_feature_data, sum)
  parent_deviance <- calculate_deviance(parent$count)
  leaves <- list()
  
  for (value in best_column$unique_values){
    path_this <- paste0(best_column$best_column, " = ", value)
    
    subunique <- unique(best_column$subdata$child)
    sdata <- subset(best_column$subdata, child == value)
    leaf_data <- aggregate(count ~ response, sdata, sum)
    
    # Update processed samples count and print progress
    env$processed_samples <- env$processed_samples + sum(sdata$rows)
    progress <- (env$processed_samples / env$total_samples)
    time_left <- (Sys.time() - env$start_time)*(1/progress - 1)
    unit <- paste0(" ",attr(time_left,'unit'))
    print(paste0("Processed: ",env$processed_samples," Progress: ",round(progress*100,2)," Time Left: ",round(time_left,2),unit,collapse=''))
    
    leaf_deviance <- calculate_deviance(leaf_data$count)
    deviance_improvement <- parent_deviance - leaf_deviance
    
    leaves[[path_this]] = list(
      best_column='<leaf>',
      unique_values=subunique,
      parent_deviance=leaf_deviance,
      deviance=NULL,
      deviance_improvement=NULL,
      time=0,
      path=c(best_column$path,path_this),
      subdata=NULL,
      response=leaf_data,
      is_leaf=TRUE
    )
  }
  
  return(leaves)
}

build_tree <- function(response, predictors, table, path = c(), total_samples = NULL, env = NULL){
  
  if (is.null(env)) {
    env <- new.env()
    env$total_samples <- NULL
    env$processed_samples <- 0
    env$start_time <- Sys.time()
  }
  
  best_column <- get_best_column(response, predictors, table, path)
  
  if (is.null(env$total_samples)) {
    env$total_samples <- sum(best_column$subdata$rows)
    print(paste0("Total samples to process: ", env$total_samples,collapse=''))
    
  }
  
  remaining_predictors <- setdiff(predictors, best_column$best_column)
  
  tree <- list(NODE = best_column)
  
  if (length(remaining_predictors) == 0 || length(best_column$unique_values) == 0){
    leaf <- make_leaf(best_column, env)
    for (i in 1:length(leaf)){
      path_this <- leaf[[i]]$path[length(leaf[[i]]$path)]
      tree[[path_this]] = list(NODE = leaf[[i]])
    }
    return(tree)
  }
  
  for (value in best_column$unique_values){
    path_this <- paste0(best_column$best_column, " = ", value)
    path_values <- c(path, path_this)
    
    value_response <- best_column$responses_by_split[best_column$responses_by_split$child == value,]
    
    split_best_column <- list(
      best_column = best_column$best_column,
      unique_values = best_column$unique_values,
      parent_deviance = best_column$split_deviance,
      split_deviance = calculate_deviance(value_response$count),
      deviance_improvement = best_column$deviance_improvement,
      time = best_column$time,
      path = path_values,
      subdata = best_column$subdata[best_column$subdata$child == value,],
      response = value_response[, c("response", "count")],
      responses_by_split = best_column$responses_by_split
    )
    
    tree[[path_this]] <- build_tree(response, remaining_predictors, table, 
                                    path_values, total_samples, env)
  }
  
  return(tree)
}

extract_tree_structure <- function(tree) {
  nodes <- list()
  node_counter <- 1
  
  process_node <- function(current, path = "", depth = 0, node_id = 1) {
    node <- current$NODE
    
    if (is.null(node)) {
      return()
    }
    
    is_leaf <- if (!is.null(node$is_leaf)) node$is_leaf else FALSE
    
    node_info <- list(
      node_id = node_id,
      depth = depth,
      is_leaf = is_leaf,
      path = if (is.null(path)) "" else path,
      split_var = if (is.null(node$best_column)) "<leaf>" else node$best_column,
      left_child = NA_integer_,
      right_child = NA_integer_,
      parent_deviance = if (is.null(node$parent_deviance)) NA_real_ else node$parent_deviance,
      split_deviance = if (is.null(node$split_deviance)) NA_real_ else node$split_deviance,
      deviance_improvement = if (is.null(node$deviance_improvement)) NA_real_ else node$deviance_improvement,
      n_samples = NA_integer_,
      n_samples_0 = NA_integer_,
      n_samples_1 = NA_integer_
    )
    
    if (!is.null(node$response) && !is.null(node$response$count)) {
      node_info$n_samples <- sum(node$response$count)
      response_counts <- node$response$count
      node_info$n_samples_0 <- response_counts[1]
      node_info$n_samples_1 <- response_counts[2]
    }
    
    if (!is_leaf) {
      child_names <- names(current)[!names(current) %in% c("NODE")]
      
      if (length(child_names) > 0) {
        split_info <- strsplit(child_names[1], " = ")[[1]]
        node_info$split_var <- split_info[1]
        
        left_path <- paste0(path, if (path != "") "/" else "", split_info[1], "=0")
        left_name <- paste(split_info[1], "=", 0)
        if (left_name %in% child_names) {
          node_counter <<- node_counter + 1
          left_id <- node_counter
          node_info$left_child <- left_id
          process_node(current[[left_name]], left_path, depth + 1, left_id)
        }
        
        right_path <- paste0(path, if (path != "") "/" else "", split_info[1], "=1")
        right_name <- paste(split_info[1], "=", 1)
        if (right_name %in% child_names) {
          node_counter <<- node_counter + 1
          right_id <- node_counter
          node_info$right_child <- right_id
          process_node(current[[right_name]], right_path, depth + 1, right_id)
        }
      }
    }
    
    nodes[[as.character(node_id)]] <<- node_info
  }
  
  process_node(tree)
  
  if (length(nodes) == 0) {
    stop("No valid nodes found in the tree structure")
  }
  
  result <- do.call(rbind, lapply(nodes, function(x) {
    data.frame(
      node_id = as.integer(x$node_id),
      depth = as.integer(x$depth),
      is_leaf = as.logical(x$is_leaf),
      path = as.character(x$path),
      split_var = as.character(x$split_var),
      left_child = as.integer(x$left_child),
      right_child = as.integer(x$right_child),
      parent_deviance = as.numeric(x$parent_deviance),
      split_deviance = as.numeric(x$split_deviance),
      deviance_improvement = as.numeric(x$deviance_improvement),
      n_samples = as.integer(x$n_samples),
      n_samples_0 = as.integer(x$n_samples_0),
      n_samples_1 = as.integer(x$n_samples_1),
      stringsAsFactors = FALSE
    )
  }))
  
  result[order(result$node_id), ]
}

rpart.sql <- function(response, predictors, table, ...) {
  t_start <- Sys.time()
  tree <- build_tree(response, predictors, table, ...)
  t_end <- Sys.time()
  total_time <- t_end - t_start
  
  cat("\nTree building completed!\n")
  
  node <- extract_tree_structure(tree)
  
  tree <- structure(tree,
                    time = total_time,
                    nodes = node)
  
  return(tree)
}


setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")

connectdB()

table <- "aggregated"
response <- 'permission_denied'
predictors <- models[['global']]
m <- rpart.sql(response, predictors, table)

saveRDS(m,file='dt.model')
disconnectdB()