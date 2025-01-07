combn_query <- function(predictors, table, combinations, batch_size = 500) {
  get_predictor_combos <- function(preds, n) {
    combo_list <- combn(preds, n, simplify = FALSE)
    lapply(combo_list, function(combo) {
      pred_str <- paste(combo, collapse = ", ")
      group_str <- paste(combo, collapse = ", ")
      cast_str <- paste(sprintf("CAST(%s as TEXT)", combo), collapse = " || '_' || ")
      
      sprintf("
      SELECT
        '%s' as pred,
        %s as pred_val,
        SUM(TOTAL) as total_count,
        SUM(POSITIVE) as positive_count
      FROM %s
      GROUP BY %s",
              pred_str, cast_str, table, group_str)
    })
  }
  
  predictor_queries <- get_predictor_combos(predictors, combinations)
  total_queries <- length(predictor_queries)
  batches <- split(predictor_queries, ceiling(seq_along(predictor_queries)/batch_size))
  total_batches <- length(batches)
  
  results <- data.frame()
  start_time <- Sys.time()
  
  for(i in seq_along(batches)) {
    batch <- batches[[i]]
    batch_start <- Sys.time()
    
    query <- sprintf("
    WITH predictor_counts AS (
      %s
    ),
    observed_vs_expected AS (
      SELECT
        pred,
        pred_val,
        total_count,
        positive_count,
        (total_count - positive_count) as negative_count,
        CAST(SUM(positive_count) OVER (PARTITION BY pred) AS FLOAT) /
          SUM(total_count) OVER (PARTITION BY pred) * total_count as expected_positive,
        CAST(SUM(total_count - positive_count) OVER (PARTITION BY pred) AS FLOAT) /
          SUM(total_count) OVER (PARTITION BY pred) * total_count as expected_negative
      FROM predictor_counts
      WHERE total_count > 0
    ),
    deviance_calc AS (
      SELECT
        pred,
        pred_val,
        (positive_count * LOG(NULLIF(positive_count / expected_positive, 0))) +
        (negative_count * LOG(NULLIF(negative_count / expected_negative, 0))) as deviance_component
      FROM observed_vs_expected
    )
    SELECT
      pred,
      SUM(deviance_component) * 2 as deviance,
      ROW_NUMBER() OVER (ORDER BY SUM(deviance_component) DESC) as rank
    FROM deviance_calc
    GROUP BY pred
    ORDER BY deviance DESC;",
                     paste(batch, collapse = "\nUNION ALL\n"))
    
    batch_results <- dbGetQuery(conn, query)
    results <- rbind(results, batch_results)
    
    # Progress reporting
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    avg_time_per_batch <- elapsed / i
    est_remaining <- avg_time_per_batch * (total_batches - i)
    
    cat(sprintf("\nBatch %d/%d completed", i, total_batches))
    cat(sprintf("\nElapsed time: %.2f minutes", as.numeric(elapsed)))
    cat(sprintf("\nEstimated time remaining: %.2f minutes\n", as.numeric(est_remaining)))
  }
  
  results <- results[order(-results$deviance), ]
  row.names(results) <- 1:nrow(results)
  results$rank <- NULL
  return(results)
}


setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")

connectdB()
on.exit(disconnectdB())

table <- "aggregated_binomial_test"
predictors <- models[['global']][1:4]
n <- 2

print(choose(length(predictors),n))

t1 <- Sys.time()
df <- combn_query(predictors, table, combinations=n)
print(Sys.time() - t1)

print(df)

############

combn_query <- function(predictors, table, combinations, batch_size = 500) {
  get_predictor_combos <- function(preds, n) {
    combo_list <- combn(preds, n, simplify = FALSE)
    lapply(combo_list, function(combo) {
      pred_str <- paste(combo, collapse = ", ")
      group_str <- paste(combo, collapse = ", ")
      cast_str <- paste(sprintf("CAST(%s as TEXT)", combo), collapse = " || '_' || ")
      
      sprintf("
      SELECT
        '%s' as pred,
        %s as pred_val,
        SUM(TOTAL) as total_count,
        SUM(POSITIVE) as positive_count
      FROM %s
      GROUP BY %s",
              pred_str, cast_str, table, group_str)
    })
  }
  
  predictor_queries <- get_predictor_combos(predictors, combinations)
  total_queries <- length(predictor_queries)
  batches <- split(predictor_queries, ceiling(seq_along(predictor_queries)/batch_size))
  total_batches <- length(batches)
  
  results <- data.frame()
  start_time <- Sys.time()
  
  for(i in seq_along(batches)) {
    batch <- batches[[i]]
    batch_start <- Sys.time()
    
    query <- sprintf("
    WITH predictor_counts AS (
      %s
    ),
    probabilities AS (
      SELECT
        pred,
        pred_val,
        CAST(total_count AS FLOAT) / SUM(total_count) OVER (PARTITION BY pred) as p_x,
        CAST(positive_count AS FLOAT) / total_count as p_y_given_x,
        CAST(SUM(positive_count) OVER (PARTITION BY pred) AS FLOAT) / 
          SUM(total_count) OVER (PARTITION BY pred) as p_y
      FROM predictor_counts
      WHERE total_count > 0
    ),
    mi_components AS (
      SELECT
        pred,
        pred_val,
        p_x * (
          p_y_given_x * LOG(NULLIF(p_y_given_x / p_y, 0)) +
          (1 - p_y_given_x) * LOG(NULLIF((1 - p_y_given_x) / (1 - p_y), 0))
        ) as mi_component
      FROM probabilities
    )
    SELECT
      pred,
      SUM(mi_component) as mutual_information,
      ROW_NUMBER() OVER (ORDER BY SUM(mi_component) DESC) as rank
    FROM mi_components
    GROUP BY pred
    ORDER BY mutual_information DESC;",
                     paste(batch, collapse = "\nUNION ALL\n"))
    
    batch_results <- dbGetQuery(conn, query)
    results <- rbind(results, batch_results)
    
    # Progress reporting
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    avg_time_per_batch <- elapsed / i
    est_remaining <- avg_time_per_batch * (total_batches - i)
    
    cat(sprintf("\nBatch %d/%d completed", i, total_batches))
    cat(sprintf("\nElapsed time: %.2f minutes", as.numeric(elapsed)))
    cat(sprintf("\nEstimated time remaining: %.2f minutes\n", as.numeric(est_remaining)))
  }
  
  results <- results[order(-results$mutual_information), ]
  row.names(results) <- 1:nrow(results)
  results$rank <- NULL
  return(results)
}


setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")

connectdB()
on.exit(disconnectdB())

table <- "aggregated_binomial_test"
predictors <- models[['global']][1:4]
n <- 2

print(choose(length(predictors),n))

t1 <- Sys.time()
df <- combn_query(predictors, table, combinations=3)
print(Sys.time() - t1)

print(df)

