build_decision_tree <- function(conn, table_name, categorical_features, continuous_features, label_column,
                                weights = NULL, max_depth = 5, num_splits = 10, trace = FALSE, progress = FALSE) {
  # Combine categorical and continuous features
  features <- c(categorical_features, continuous_features)
  
  # Function to sanitize values to prevent SQL injection
  sanitize_value <- function(value) {
    value <- gsub("'", "''", value)
    return(value)
  }
  
  # Function to compute weighted label counts at the current node
  compute_label_counts <- function(conditions) {
    condition_str <- paste(conditions, collapse = " AND ")
    if (is.null(weights)) {
      # Use unweighted counts
      query <- paste0("SELECT ", label_column, ", COUNT(*) as count FROM ", table_name)
    } else {
      # Use weighted counts
      query <- paste0("SELECT ", label_column, ", SUM(", weights, ") as count FROM ", table_name)
    }
    if (condition_str != "") {
      query <- paste0(query, " WHERE ", condition_str)
    }
    query <- paste0(query, " GROUP BY ", label_column)
    
    if (trace) {
      start_time <- Sys.time()
      cat("Starting query at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Executing query:\n", query, "\n")
    }
    
    res <- dbGetQuery(conn, query)
    
    if (trace) {
      end_time <- Sys.time()
      duration <- end_time - start_time
      cat("Query completed in:", round(duration, 3), "seconds\n\n")
    }
    
    return(res)
  }
  
  # Function to compute Gini impurity using weighted counts
  gini_impurity <- function(counts) {
    total <- sum(counts)
    probs <- counts / total
    impurity <- 1 - sum(probs^2)
    return(impurity)
  }
  
  # Function to compute counts for left and right splits
  compute_split_counts <- function(conditions, split_condition) {
    # Left subset
    left_conditions <- c(conditions, split_condition)
    left_counts <- compute_label_counts(left_conditions)
    
    # Right subset
    right_conditions <- c(conditions, paste0("NOT (", split_condition, ")"))
    right_counts <- compute_label_counts(right_conditions)
    
    return(list(left = left_counts, right = right_counts))
  }
  
  # Function to compute impurity reduction
  compute_impurity_reduction <- function(parent_counts, left_counts, right_counts) {
    total_count <- sum(parent_counts$count)
    parent_impurity <- gini_impurity(parent_counts$count)
    
    left_total <- sum(left_counts$count)
    right_total <- sum(right_counts$count)
    
    left_impurity <- gini_impurity(left_counts$count)
    right_impurity <- gini_impurity(right_counts$count)
    
    weighted_impurity <- (left_total / total_count) * left_impurity +
      (right_total / total_count) * right_impurity
    
    impurity_reduction <- parent_impurity - weighted_impurity
    
    return(impurity_reduction)
  }
  
  # Function to get distinct values of a feature
  get_feature_values <- function(feature, conditions) {
    condition_str <- paste(conditions, collapse = " AND ")
    query <- paste0("SELECT DISTINCT ", feature, " FROM ", table_name)
    if (condition_str != "") {
      query <- paste0(query, " WHERE ", condition_str)
    }
    
    if (trace) {
      start_time <- Sys.time()
      cat("Starting query at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Executing query:\n", query, "\n")
    }
    
    res <- dbGetQuery(conn, query)
    
    if (trace) {
      end_time <- Sys.time()
      duration <- end_time - start_time
      cat("Query completed in:", round(duration, 3), "seconds\n\n")
    }
    
    return(res[[feature]])
  }
  
  # Function to get min and max of a continuous feature
  get_feature_min_max <- function(feature, conditions) {
    condition_str <- paste(conditions, collapse = " AND ")
    query <- paste0("SELECT MIN(", feature, ") as min_value, MAX(", feature, ") as max_value FROM ", table_name)
    if (condition_str != "") {
      query <- paste0(query, " WHERE ", condition_str)
    }
    
    if (trace) {
      start_time <- Sys.time()
      cat("Starting query at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Executing query:\n", query, "\n")
    }
    
    res <- dbGetQuery(conn, query)
    
    if (trace) {
      end_time <- Sys.time()
      duration <- end_time - start_time
      cat("Query completed in:", round(duration, 3), "seconds\n\n")
    }
    
    return(c(min = res$min_value, max = res$max_value))
  }
  
  # Function to get split points for continuous features
  get_split_points <- function(min_value, max_value, num_splits) {
    splits <- seq(min_value, max_value, length.out = num_splits + 2)[-c(1, num_splits + 2)]
    return(splits)
  }
  
  # Function to extract variable names from conditions
  extract_variables <- function(conditions) {
    variables <- sapply(conditions, function(cond) {
      # Remove 'NOT (' and ')' if present
      cond_clean <- gsub("^NOT \\((.*)\\)$", "\\1", cond)
      # Extract the variable name before any operators (=, <=, >=, <, >)
      var_name <- sub("\\s*[=<>!]+.*$", "", cond_clean)
      var_name <- trimws(var_name)
      return(var_name)
    })
    unique(variables)
  }
  
  # Function to get total number of rows
  get_total_rows <- function() {
    if (is.null(weights)) {
      query <- paste0("SELECT COUNT(*) as total_rows FROM ", table_name)
    } else {
      query <- paste0("SELECT SUM(", weights, ") as total_rows FROM ", table_name)
    }
    res <- dbGetQuery(conn, query)
    return(res$total_rows)
  }
  
  # Get total number of rows in the dataset
  total_rows <- get_total_rows()
  
  # Initialize cumulative count of rows covered by leaves
  cumulative_leaf_rows <- 0
  
  # Record the start time of the algorithm
  algorithm_start_time <- Sys.time()
  
  # Recursive function to build the decision tree and collect leaves
  build_tree <- function(conditions, depth, path) {
    # Compute label counts at current node
    parent_counts <- compute_label_counts(conditions)
    
    # Base case: check for stopping conditions
    if (nrow(parent_counts) == 1 || depth >= max_depth) {
      # Compute number of rows at this leaf
      leaf_row_count <- sum(parent_counts$count)
      
      # Update cumulative count of rows covered by leaves
      cumulative_leaf_rows <<- cumulative_leaf_rows + leaf_row_count
      
      # Compute progress ratio
      progress_ratio <- cumulative_leaf_rows / total_rows
      
      # Compute time elapsed
      current_time <- Sys.time()
      time_elapsed <- as.numeric(difftime(current_time, algorithm_start_time, units = "secs"))
      
      # Estimate total time and time remaining
      if (progress_ratio > 0) {
        estimated_total_time <- time_elapsed / progress_ratio
        estimated_time_remaining <- estimated_total_time - time_elapsed
      } else {
        estimated_time_remaining <- NA
      }
      
      # Print progress if requested
      if (progress) {
        cat(sprintf("Progress: %.2f%%\n", progress_ratio * 100))
        cat(sprintf("Time elapsed: %.2f seconds\n", time_elapsed))
        if (!is.na(estimated_time_remaining)) {
          cat(sprintf("Estimated time remaining: %.2f seconds\n\n", estimated_time_remaining))
        } else {
          cat("Estimated time remaining: Calculating...\n\n")
        }
      }
      
      # Extract variables from the path
      variables <- extract_variables(path)
      
      # Return a leaf node with path, prediction, and variables
      prediction <- parent_counts[[label_column]][which.max(parent_counts$count)]
      leaf <- list(path = path, prediction = prediction, variables = variables)
      return(list(leaf))
    }
    
    best_impurity_reduction <- -Inf
    best_split <- NULL
    
    # Loop over features to find the best split
    for (feature in features) {
      if (feature %in% categorical_features) {
        # Categorical feature
        values <- get_feature_values(feature, conditions)
        for (value in values) {
          value_sanitized <- sanitize_value(as.character(value))
          split_condition <- paste0(feature, " = '", value_sanitized, "'")
          split_counts <- compute_split_counts(conditions, split_condition)
          
          # Skip if splits are empty
          if (nrow(split_counts$left) == 0 || nrow(split_counts$right) == 0) {
            next
          }
          
          impurity_reduction <- compute_impurity_reduction(parent_counts, split_counts$left, split_counts$right)
          if (impurity_reduction > best_impurity_reduction) {
            best_impurity_reduction <- impurity_reduction
            best_split <- list(
              feature = feature,
              split_condition = split_condition,
              split_type = 'categorical',
              value = value,
              left_counts = split_counts$left,
              right_counts = split_counts$right
            )
          }
        }
      } else if (feature %in% continuous_features) {
        # Continuous feature
        min_max <- get_feature_min_max(feature, conditions)
        if (is.na(min_max["min"]) || is.na(min_max["max"]) || min_max["min"] == min_max["max"]) {
          next
        }
        splits <- get_split_points(min_max["min"], min_max["max"], num_splits)
        for (split_point in splits) {
          split_condition <- paste0(feature, " <= ", split_point)
          split_counts <- compute_split_counts(conditions, split_condition)
          
          # Skip if splits are empty
          if (nrow(split_counts$left) == 0 || nrow(split_counts$right) == 0) {
            next
          }
          
          impurity_reduction <- compute_impurity_reduction(parent_counts, split_counts$left, split_counts$right)
          if (impurity_reduction > best_impurity_reduction) {
            best_impurity_reduction <- impurity_reduction
            best_split <- list(
              feature = feature,
              split_condition = split_condition,
              split_type = 'continuous',
              value = split_point,
              left_counts = split_counts$left,
              right_counts = split_counts$right
            )
          }
        }
      }
    }
    
    if (is.null(best_split)) {
      # No valid split found; return a leaf node
      # Compute number of rows at this leaf
      leaf_row_count <- sum(parent_counts$count)
      
      # Update cumulative count of rows covered by leaves
      cumulative_leaf_rows <<- cumulative_leaf_rows + leaf_row_count
      
      # Compute progress ratio
      progress_ratio <- cumulative_leaf_rows / total_rows
      
      # Compute time elapsed
      current_time <- Sys.time()
      time_elapsed <- as.numeric(difftime(current_time, algorithm_start_time, units = "secs"))
      
      # Estimate total time and time remaining
      if (progress_ratio > 0) {
        estimated_total_time <- time_elapsed / progress_ratio
        estimated_time_remaining <- estimated_total_time - time_elapsed
      } else {
        estimated_time_remaining <- NA
      }
      
      # Print progress if requested
      if (progress) {
        cat(sprintf("Progress: %.2f%%\n", progress_ratio * 100))
        cat(sprintf("Time elapsed: %.2f seconds\n", time_elapsed))
        if (!is.na(estimated_time_remaining)) {
          cat(sprintf("Estimated time remaining: %.2f seconds\n\n", estimated_time_remaining))
        } else {
          cat("Estimated time remaining: Calculating...\n\n")
        }
      }
      
      variables <- extract_variables(path)
      prediction <- parent_counts[[label_column]][which.max(parent_counts$count)]
      leaf <- list(path = path, prediction = prediction, variables = variables)
      return(list(leaf))
    }
    
    # Recursively build left and right subtrees, updating the path
    left_conditions <- c(conditions, best_split$split_condition)
    right_conditions <- c(conditions, paste0("NOT (", best_split$split_condition, ")"))
    
    left_path <- c(path, best_split$split_condition)
    right_path <- c(path, paste0("NOT (", best_split$split_condition, ")"))
    
    left_leaves <- build_tree(left_conditions, depth + 1, left_path)
    right_leaves <- build_tree(right_conditions, depth + 1, right_path)
    
    # Combine the leaves from both subtrees
    return(c(left_leaves, right_leaves))
  }
  
  # Build the decision tree starting from the root
  leaves <- build_tree(character(0), 0, character(0))
  
  return(leaves)
}


setwd("~/Desktop/working8/Thesis")
source("./src/utils/helper_functions.R")

connectdB()

# table_name <- 'aggregated'
# categorical_features <- c('affect','functions')

table_name <- "aggregated"
categorical_features <- models[['global']]
label_column <- "permission_denied"

# Build the decision tree
tree <- build_decision_tree(
  conn = conn,
  table_name = table_name,
  categorical_features = categorical_features,
  continuous_features = NULL,
  label_column = label_column,
  weights = 'N',
  max_depth = 10,
  num_splits = 10,
  trace = FALSE,
  progress= TRUE
)

print('model finished!')
saveRDS(tree,file=file.path("global_aggregated_dt.model"))
print('model saved!')

disconnectdB()