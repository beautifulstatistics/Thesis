bnetwork <- function(predictors, table, batch_size = 500, significance = 0.05, min_count = 5) {
  # Helper to get all possible value combinations for the variables
  get_all_combinations <- function(vars) {
    var_values <- lapply(vars, function(var) {
      query <- sprintf("SELECT DISTINCT %s AS val FROM %s WHERE %s IS NOT NULL", var, table, var)
      dbGetQuery(conn, query)$val
    })
    names(var_values) <- vars
    expand.grid(var_values, stringsAsFactors = FALSE)
  }
  
  # Helper to get observed counts
  get_observed_counts <- function(vars) {
    vars_str <- paste(vars, collapse = ", ")
    sprintf("
    SELECT %s, SUM(TOTAL) AS count
    FROM %s
    WHERE %s
    GROUP BY %s",
            vars_str, table,
            paste(sprintf("%s IS NOT NULL", vars), collapse = " AND "),
            vars_str)
  }
  
  # Calculate mutual information
  mi_results <- data.frame()
  start_time <- Sys.time()
  
  # For each pair of predictors
  predictor_pairs <- combn(predictors, 2, simplify = FALSE)
  for (pair in predictor_pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    
    # Get all possible combinations of values
    combinations <- get_all_combinations(c(var1, var2))
    
    # Get observed counts
    observed_query <- get_observed_counts(c(var1, var2))
    observed_counts <- dbGetQuery(conn, observed_query)
    observed_counts <- merge(combinations, observed_counts, by = c(var1, var2), all.x = TRUE)
    observed_counts$count[is.na(observed_counts$count)] <- 0
    
    # Calculate marginal counts
    total_count <- sum(observed_counts$count)
    if (total_count == 0) next
    
    # Marginal counts for var1
    marginal_counts_var1 <- aggregate(count ~ ., data = observed_counts[, c(var1, "count")], sum)
    names(marginal_counts_var1) <- c(var1, "count1")
    
    # Marginal counts for var2
    marginal_counts_var2 <- aggregate(count ~ ., data = observed_counts[, c(var2, "count")], sum)
    names(marginal_counts_var2) <- c(var2, "count2")
    
    # Merge counts
    probs <- merge(observed_counts, marginal_counts_var1, by = var1)
    probs <- merge(probs, marginal_counts_var2, by = var2)
    
    # Calculate probabilities
    probs$p_xy <- probs$count / total_count
    probs$p_x <- probs$count1 / total_count
    probs$p_y <- probs$count2 / total_count
    
    # Calculate mutual information
    # Avoid zero probabilities
    probs <- probs[probs$p_xy > 0 & probs$p_x > 0 & probs$p_y > 0, ]
    mi <- sum(probs$p_xy * log2(probs$p_xy / (probs$p_x * probs$p_y)))
    
    mi_results <- rbind(mi_results, data.frame(
      var1 = var1,
      var2 = var2,
      mutual_information = mi
    ))
  }
  
  # Only proceed if we have valid MI results
  if (nrow(mi_results) == 0) {
    warning("No valid mutual information results found")
    return(NULL)
  }
  
  # Conditional independence testing
  cond_results <- data.frame()
  for (conditioning_var in predictors) {
    vars_to_test <- setdiff(predictors, conditioning_var)
    combo_list <- combn(vars_to_test, 2, simplify = FALSE)
    for (pair in combo_list) {
      var1 <- pair[1]
      var2 <- pair[2]
      
      # Get all possible combinations
      combinations <- get_all_combinations(c(var1, var2, conditioning_var))
      
      # Get observed counts
      observed_query <- get_observed_counts(c(var1, var2, conditioning_var))
      observed_counts <- dbGetQuery(conn, observed_query)
      observed_counts <- merge(combinations, observed_counts, by = c(var1, var2, conditioning_var), all.x = TRUE)
      observed_counts$count[is.na(observed_counts$count)] <- 0
      
      # Calculate expected counts
      total_counts <- aggregate(count ~ get(conditioning_var), data = observed_counts, sum)
      names(total_counts) <- c(conditioning_var, "total_cond")
      
      marginal_counts_var1 <- aggregate(count ~ get(var1) + get(conditioning_var), data = observed_counts, sum)
      names(marginal_counts_var1) <- c(var1, conditioning_var, "count1_cond")
      
      marginal_counts_var2 <- aggregate(count ~ get(var2) + get(conditioning_var), data = observed_counts, sum)
      names(marginal_counts_var2) <- c(var2, conditioning_var, "count2_cond")
      
      expected_counts <- merge(observed_counts, total_counts, by = conditioning_var)
      expected_counts <- merge(expected_counts, marginal_counts_var1, by = c(var1, conditioning_var))
      expected_counts <- merge(expected_counts, marginal_counts_var2, by = c(var2, conditioning_var))
      
      expected_counts$expected_count <- (expected_counts$count1_cond * expected_counts$count2_cond) / expected_counts$total_cond
      
      # Calculate G-statistic
      valid_entries <- expected_counts$expected_count > 0 & expected_counts$count > 0
      expected_counts <- expected_counts[valid_entries, ]
      
      if (nrow(expected_counts) == 0) next
      
      expected_counts$g_stat_component <- 2 * expected_counts$count * log(expected_counts$count / expected_counts$expected_count)
      
      g_stat <- sum(expected_counts$g_stat_component)
      df <- nrow(expected_counts) - 1
      
      # Compute p-value
      p_value <- 1 - pchisq(g_stat, df)
      
      cond_results <- rbind(cond_results, data.frame(
        var1 = var1,
        var2 = var2,
        condition_var = conditioning_var,
        g_stat = g_stat,
        df = df,
        p_value = p_value
      ))
    }
  }
  
  
  final_edges <- data.frame()
  for (i in 1:nrow(mi_results)) {
    edge <- mi_results[i, ]
    is_independent <- FALSE
    edge_p_values <- numeric()
    
    for (var in predictors) {
      if (edge$var1 != var && edge$var2 != var) {
        relevant_test <- cond_results[
          cond_results$var1 == edge$var1 &
            cond_results$var2 == edge$var2 &
            cond_results$condition_var == var, ]
        
        if (nrow(relevant_test) > 0) {
          critical_value <- qchisq(1 - significance, relevant_test$df[1])
          edge_p_values <- c(edge_p_values, relevant_test$p_value)
          if (relevant_test$g_stat < critical_value) {
            is_independent <- TRUE
            break
          }
        }
      }
    }
    
    if (!is_independent) {
      final_edges <- rbind(final_edges, data.frame(
        var1 = edge$var1,
        var2 = edge$var2,
        mutual_information = edge$mutual_information,
        min_p_value = ifelse(length(edge_p_values) > 0, min(edge_p_values), NA)
      ))
    }
  }
  
  # Return results
  execution_time <- difftime(Sys.time(), start_time, units = "secs")
  
  return(list(
    mutual_information = mi_results,
    conditional_tests = cond_results,
    final_edges = final_edges,
    execution_time = execution_time
  ))
}

setwd("/home/kenneywl/Desktop/Thesis")
source("./src/utils/helper_functions.R")

connectdB()
on.exit(disconnectdB())

table <- "aggregated_binomial_test"
predictors <- models[['global']]
response <- c("TOTAL","POSITIVE")

t1 <- Sys.time()
df <- bnetwork(predictors, table)
print(Sys.time() - t1)

print(df)
