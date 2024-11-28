calculate_deviance <- function(counts) {
  if (length(counts) < 2 || sum(counts) == 0) return(0)
  p <- counts / sum(counts)
  -2 * sum(counts * ifelse(p > 0, p * log(p), 0))
}

get_best_column <- function(response, predictors, table, path = c()) {
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
  query <- build_query(predictors, path)

  time_start <- Sys.time()
  data <- dbGetQuery(conn, query)
  time_taken <- Sys.time() - time_start

  parent_data <- aggregate(count ~ response, data, sum)
  parent_prob <- parent_data$count / sum(parent_data$count)
  parent_deviance <- -2 * sum(parent_data$count) * sum(parent_prob * log(parent_prob))
  
  # parent_deviance <- calculate_deviance(parent_data$count)
  
  results <- data.frame()
  for (feature in unique(data$name)) {
    fdata <- subset(data, name == feature)

    # split_deviance <- 0
    # total_count <- sum(fdata$count)
    # for (value in unique(fdata$child)){
    #   sdata <- subset(fdata, child == value)
    #   sdeviance <- calculate_deviance(sdata$count)
    #   weight <- sum(sdata$count) / total_count
    #   split_deviance <- split_deviance + weight * sdeviance
    # }
    
    split_deviance <- 0
    for (value in unique(fdata$child)) {
      sdata <- subset(fdata, child == value)
      sprob <- sdata$count / sum(sdata$count)
      sdeviance <- -2 * sum(sdata$count) * sum(sprob * log(sprob))
      split_deviance <- split_deviance + sdeviance
    }

    deviance_improvement <- parent_deviance - split_deviance

    num_splits <- length(unique(fdata$child))
    num_responses <- length(unique(fdata$response))
    df <- (num_splits - 1) * (num_responses - 1)

    p_value <- 1 - pchisq(deviance_improvement, df)

    results <- rbind(results, data.frame(
      feature = feature,
      deviance = split_deviance,
      deviance_improvement = deviance_improvement,
      degrees_of_freedom = df,
      p_value = p_value
    ))
  }

  results <- results[order(results$p_value),]
  best_column <- results$feature[1]
  unique_values <- unique(subset(data, name == best_column)$child)
  split_deviance <- results$deviance[1]
  deviance_improvement <- results$deviance_improvement[1]
  p_value <- results$p_value[1]
  df <- results$degrees_of_freedom[1]
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
      df = df,
      p_value = p_value,
      p_value_thresh = NULL,
      time = time_taken,
      path = path,
      subdata = subdata,
      response = split_responses,
      responses_by_split = responses_by_split,
      query = query,
      fetched_data = data,
      results_table = results,
      ntests = length(unique(data$name)),
      is_leaf = FALSE
    )
  )
}


make_last_leaf <- function(best_column, env){
  first_feature_data <- subset(best_column$subdata, name == best_column$best_column)
  parent <- aggregate(count ~ response, first_feature_data, sum)
  parent_deviance <- calculate_deviance(parent$count)
  leaves <- list()
  
  for (value in best_column$unique_values){
    path_this <- paste0(best_column$best_column, " = ", value)
    
    subunique <- unique(best_column$subdata$child)
    sdata <- subset(best_column$subdata, child == value)
    leaf_data <- aggregate(count ~ response, sdata, sum)
    
    env$processed_samples <- env$processed_samples + sum(sdata$rows)
    progress <- (env$processed_samples / env$total_samples)
    time_left <- (Sys.time() - env$start_time)*(1/progress - 1)
    unit <- paste0(" ",attr(time_left,'unit'))
    print(paste0("Processed: ",env$processed_samples," Progress: ",
                 round(progress*100,2),"% Time Left: ",round(time_left,2),unit,collapse=''))
    
    leaf_deviance <- calculate_deviance(leaf_data$count)
    
    leaves[[path_this]] = list(
      best_column='<endleaf>',
      unique_values=subunique,
      parent_deviance=leaf_deviance,
      split_deviance=NULL,
      deviance_improvement=NULL,
      df = NULL,
      p_value = NULL,
      ntests = NULL,
      time=0,
      path=c(best_column$path,path_this),
      subdata=NULL,
      response=leaf_data,
      is_leaf=TRUE
    )
  }
  
  return(leaves)
}

make_leaf <- function(best_column, env){
  leaf_data <- aggregate(count ~ response, best_column$subdata, sum)
  leaf_deviance <- calculate_deviance(leaf_data$count)
  
  env$processed_samples <- env$processed_samples + sum(best_column$subdata$rows)
  progress <- (env$processed_samples / env$total_samples)
  time_left <- (Sys.time() - env$start_time)*(1/progress - 1)
  unit <- paste0(" ", attr(time_left, 'units'))
  print(paste0("Processed: ", env$processed_samples, 
               " Progress: ", round(progress * 100, 2), 
               "% Time Left: ", round(time_left, 2), unit))
  
  return(list(
    best_column = '<stopleaf>',
    unique_values = NULL,
    parent_deviance = leaf_deviance,
    split_deviance = NULL,
    deviance_improvement = NULL,
    df = NULL,
    p_value = NULL,
    ntests = NULL,
    time = 0,
    path = best_column$path,
    subdata = NULL,
    response = leaf_data,
    is_leaf = TRUE
  ))
}



build_tree <- function(response, predictors, table, p_value, path = c(), env = NULL){

  if (is.null(env)) {
    env <- new.env()
    env$total_predictors <- length(predictors)
    env$total_samples <- NULL
    env$processed_samples <- 0
    env$start_time <- Sys.time()
  }
  
  best_column <- get_best_column(response, predictors, table, path)
  
  if (is.null(env$total_samples)) {
    env$total_samples <- sum(best_column$subdata$rows)
    print(paste0("Total samples to process: ", env$total_samples,collapse=''))
    
  }
  
  lpred = length(predictors)
  lpath = length(path)
  
  ntests = 0
  for (i in 0:lpath){
    ntests = ntests + (env$total_predictors-i)*2^i
  }


  best_column$p_value_thresh <- p_value/ntests
  
  tree <- list(NODE = best_column)
  
  remaining_predictors <- setdiff(predictors, best_column$best_column)
  if (length(remaining_predictors) == 0 || length(best_column$unique_values) == 0){
    leaf <- make_last_leaf(best_column, env)
    for (i in 1:length(leaf)){
      path_this <- leaf[[i]]$path[length(leaf[[i]]$path)]
      tree[[path_this]] = list(NODE = leaf[[i]])
    }
    return(tree)
  }
  
  
  if (best_column$p_value > best_column$p_value_thresh){
    leaf <- make_leaf(best_column, env)
    if (length(leaf$path) > 0){
      path_this <- leaf$path[length(leaf$path)]
    } else {
      path_this <- "ROOT"
    }
    
    tree[[path_this]] <- leaf
    return(tree)
  }
  
  for (value in best_column$unique_values){
    path_this <- paste0(best_column$best_column, " = ", value)
    path_values <- c(path, path_this)
    
    tree[[path_this]] <- build_tree(response, remaining_predictors, table, 
                                    p_value, path_values, env)
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
      split_var = node$best_column,
      left_child = NA_integer_,
      right_child = NA_integer_,
      parent_deviance = if (is.null(node$parent_deviance)) NA_real_ else node$parent_deviance,
      split_deviance = if (is.null(node$split_deviance)) NA_real_ else node$split_deviance,
      deviance_improvement = if (is.null(node$deviance_improvement)) NA_real_ else node$deviance_improvement,
      df = if (is.null(node$df)) NA_real_ else node$df,
      p_value = if (is.null(node$p_value)) NA_real_ else node$p_value,
      p_value_thresh = if (is.null(node$p_value_thresh)) NA_real_ else node$p_value_thresh,
      ntests = if (is.null(node$ntests)) NA_real_ else node$ntests,
      n_samples = NA_integer_,
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
      df = as.numeric(x$df),
      p_value = as.numeric(x$p_value),
      p_value_thresh = as.numeric(x$p_value_thresh),
      ntests = as.integer(x$ntests),
      n_samples = as.integer(x$n_samples),
      n_samples_1 = as.integer(x$n_samples_1),
      stringsAsFactors = FALSE
    )
  }))
  
  result[order(result$node_id), ]
}

filter_maximal_sets <- function(str_vector) {
  split_lists <- strsplit(str_vector, ":")
  
  keep <- rep(TRUE, length(split_lists))
  
  for(i in seq_along(split_lists)) {
    for(j in seq_along(split_lists)) {
      if(i != j) {
        if(all(split_lists[[i]] %in% split_lists[[j]])) {
          if(length(split_lists[[i]]) < length(split_lists[[j]])) {
            keep[i] <- FALSE
            break
          }
        }
      }
    }
  }
  
  return(str_vector[keep])
}

rpart.sql <- function(response, predictors, table, p_value, ...) {
  t_start <- Sys.time()
  tree <- build_tree(response, predictors, table, p_value, ...)
  t_end <- Sys.time()
  total_time <- t_end - t_start
  
  cat("\nTree building completed!\n")
  
  node <- extract_tree_structure(tree)
  
  print(node)
  
  vars <- c()
  for (i in node[node$is_leaf,]$path){
    variables <- unlist(strsplit(i, "=[01]/"))
    variables[length(variables)] <- sub("=[01]$", "", variables[length(variables)])
    variables <- paste0(sort(variables),collapse = ':')
    vars <- c(vars,variables)
  }
  
  terms <- unique(vars)
  largets <- terms
  
  if (!is.null(terms)){
    largest <- filter_maximal_sets(terms)
  }
  
  tree <- structure(tree,
                    time = total_time,
                    nodes = node,
                    terms = terms,
                    largest_terms = largest)
  
  return(tree)
}


setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")

connectdB()
on.exit(disconnectdB())

table <- "aggregated"
response <- 'permission_denied'
predictors <- models[['global']]

m <- rpart.sql(response, predictors, table, 0.05)

saveRDS(m,file='dt.model')