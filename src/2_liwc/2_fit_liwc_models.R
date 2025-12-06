# 2_fit_liwc_models.R
# Develop and compare nesting models for censorship prediction
# This script builds models to test the predictive power of top vs bottom nested features

# Load necessary libraries and helper functions
source("./src/utils/helper_functions.R")
source("./src/utils/models.R")
library(speedglm)
library(ggplot2)
library(pROC)

# Connect to the database
connectdB()

# Create artifacts directory if it doesn't exist
if (!dir.exists("artifacts/2_liwc")) {
  dir.create("artifacts/2_liwc", recursive = TRUE)
}

# Function to build and evaluate a model
build_and_evaluate_model <- function(response, predictors, table = "aggregated_binomial_positive", 
                                    model_name = "model", family = binomial_corrected) {
  cat("\nBuilding model:", model_name, "\n")
  cat("Response:", response, "\n")
  cat("Predictors:", paste(predictors, collapse=", "), "\n")
  cat("Table:", table, "\n")
  
  # Check if predictors exist in the table
  fields_query <- paste0("PRAGMA table_info(", table, ")")
  fields <- dbGetQuery(conn, fields_query)
  valid_predictors <- intersect(predictors, fields$name)
  
  if (length(valid_predictors) == 0) {
    cat("Error: No valid predictors found in table", table, "\n")
    return(NULL)
  }
  
  if (length(valid_predictors) < length(predictors)) {
    cat("Warning: Some predictors not found in table. Using only:", 
        paste(valid_predictors, collapse=", "), "\n")
  }
  
  # Create the data function for chunked processing
  start_time <- Sys.time()
  data_fun <- make.data(response, valid_predictors, table = table)
  
  # Build the formula
  formula <- make.formula(response, valid_predictors)
  
  # Fit the model using speedglm for large dataset handling
  model <- NULL
  tryCatch({
    # Use shlm (streaming/chunked GLM) from helper_functions.R
    model <- shlm(formula, data_fun, family = family)
    
    end_time <- Sys.time()
    cat("Model fitting completed in", format(end_time - start_time), "\n")
    
    # Calculate basic model metrics
    cat("Model summary:\n")
    model_summary <- summary(model)
    
    # Calculate AIC and BIC
    model_aic <- AIC(model)
    model_bic <- bic(model)
    cat("AIC:", model_aic, "\n")
    cat("BIC:", model_bic, "\n")
    
    # Extract significant predictors
    sig_preds <- get_sigpreds(model)
    
    cat("\nSignificant predictors making censorship MORE likely:\n")
    if (nrow(sig_preds$more_likely) > 0) {
      print(head(sig_preds$more_likely, 10))
    } else {
      cat("None found\n")
    }
    
    cat("\nSignificant predictors making censorship LESS likely:\n")
    if (nrow(sig_preds$less_likely) > 0) {
      print(head(sig_preds$less_likely, 10))
    } else {
      cat("None found\n")
    }
    
    # Save significant predictors
    write.csv(sig_preds$more_likely, 
              file = paste0("artifacts/2_liwc/", model_name, "_positive_predictors.csv"),
              row.names = FALSE)
    write.csv(sig_preds$less_likely, 
              file = paste0("artifacts/2_liwc/", model_name, "_negative_predictors.csv"),
              row.names = FALSE)
    
    # Save full model summary
    sink(paste0("artifacts/2_liwc/", model_name, "_summary.txt"))
    print(model_summary)
    cat("\nAIC:", model_aic, "\n")
    cat("BIC:", model_bic, "\n")
    sink()
    
    # Save the model object
    saveRDS(model, file = paste0("artifacts/2_liwc/", model_name, ".rds"))
    
    cat("Model and summary saved to artifacts/2_liwc/", model_name, ".*\n")
    
    return(list(
      model = model,
      aic = model_aic,
      bic = model_bic,
      significant_positive = sig_preds$more_likely,
      significant_negative = sig_preds$less_likely
    ))
    
  }, error = function(e) {
    cat("Error fitting model:", e$message, "\n")
    return(NULL)
  })
}

# Function to make predictions and evaluate model performance
evaluate_model_performance <- function(model, response, predictors, table = "aggregated_binomial_positive",
                                      model_name = "model", sample_size = 50000) {
  cat("\nEvaluating model performance for:", model_name, "\n")
  
  # Sample data for evaluation
  fields_query <- paste0("PRAGMA table_info(", table, ")")
  fields <- dbGetQuery(conn, fields_query)
  valid_predictors <- intersect(predictors, fields$name)
  
  if (length(valid_predictors) == 0) {
    cat("Error: No valid predictors found for evaluation\n")
    return(NULL)
  }
  
  # Determine how to sample and evaluate based on response variable
  if (length(response) == 1 && response == "permission_denied") {
    # Binary outcome case
    query <- paste0("SELECT ", response, ", ", paste(valid_predictors, collapse=", "), 
                   " FROM ", table, 
                   " WHERE ROWID IN (SELECT ROWID FROM ", table, 
                   " ORDER BY RANDOM() LIMIT ", sample_size, ")")
    
    test_data <- dbGetQuery(conn, query)
    
    # Make predictions
    predictions <- predict(model, newdata = test_data, type = "response")
    
    # Calculate performance metrics
    # Create ROC curve
    roc_obj <- roc(test_data[[response]], predictions)
    auc_value <- auc(roc_obj)
    
    # Calculate accuracy, precision, recall at 0.5 threshold
    threshold <- 0.5
    predicted_class <- ifelse(predictions >= threshold, 1, 0)
    conf_matrix <- table(Actual = test_data[[response]], Predicted = predicted_class)
    
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    precision <- if (sum(predicted_class) > 0) {
      conf_matrix[2,2] / sum(predicted_class)
    } else {
      NA
    }
    
    recall <- if (conf_matrix[2,1] + conf_matrix[2,2] > 0) {
      conf_matrix[2,2] / (conf_matrix[2,1] + conf_matrix[2,2])
    } else {
      NA
    }
    
    f1_score <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
      2 * (precision * recall) / (precision + recall)
    } else {
      NA
    }
    
    # Print performance metrics
    cat("AUC:", auc_value, "\n")
    cat("Accuracy:", accuracy, "\n")
    cat("Precision:", precision, "\n")
    cat("Recall:", recall, "\n")
    cat("F1 Score:", f1_score, "\n")
    
    # Create ROC curve plot
    pdf(paste0("artifacts/2_liwc/", model_name, "_roc.pdf"))
    plot(roc_obj, main = paste("ROC Curve for", model_name), 
         col = "blue", lwd = 2)
    abline(a = 0, b = 1, lty = 2, col = "gray")
    dev.off()
    
    # Save performance metrics
    perf_df <- data.frame(
      metric = c("AUC", "Accuracy", "Precision", "Recall", "F1_Score"),
      value = c(auc_value, accuracy, precision, recall, f1_score)
    )
    
    write.csv(perf_df, 
              file = paste0("artifacts/2_liwc/", model_name, "_performance.csv"),
              row.names = FALSE)
    
    return(list(
      auc = auc_value,
      accuracy = accuracy,
      precision = precision,
      recall = recall,
      f1_score = f1_score,
      confusion_matrix = conf_matrix
    ))
    
  } else if (length(response) == 2 && all(response %in% c("POSITIVE", "TOTAL"))) {
    # Binomial response case (aggregated data)
    query <- paste0("SELECT ", paste(response, collapse=", "), ", ", 
                   paste(valid_predictors, collapse=", "), 
                   " FROM ", table, 
                   " WHERE ROWID IN (SELECT ROWID FROM ", table, 
                   " ORDER BY RANDOM() LIMIT ", sample_size, ")")
    
    test_data <- dbGetQuery(conn, query)
    
    # Calculate actual proportions
    test_data$actual_prop <- test_data[[response[1]]] / test_data[[response[2]]]
    
    # Make predictions
    predictions <- predict(model, newdata = test_data, type = "response")
    
    # Calculate performance metrics
    mse <- mean((test_data$actual_prop - predictions)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(test_data$actual_prop - predictions))
    
    # Calculate correlation between predicted and actual
    cor_value <- cor(test_data$actual_prop, predictions)
    
    # Print performance metrics
    cat("Mean Squared Error:", mse, "\n")
    cat("Root Mean Squared Error:", rmse, "\n")
    cat("Mean Absolute Error:", mae, "\n")
    cat("Correlation:", cor_value, "\n")
    
    # Create scatter plot of predicted vs actual
    pdf(paste0("artifacts/2_liwc/", model_name, "_prediction.pdf"))
    plot(test_data$actual_prop, predictions,
         main = paste("Actual vs Predicted for", model_name),
         xlab = "Actual Proportion",
         ylab = "Predicted Proportion",
         pch = 16,
         col = adjustcolor("blue", alpha.f = 0.5))
    abline(a = 0, b = 1, col = "red", lty = 2)
    dev.off()
    
    # Create binned residual plot
    residuals <- predictions - test_data$actual_prop
    binned_residuals <- bin.residuals(predictions, test_data$actual_prop)
    
    pdf(paste0("artifacts/2_liwc/", model_name, "_binned_residuals.pdf"))
    plot(binned_residuals$predicted, binned_residuals$residuals,
         main = paste("Binned Residual Plot for", model_name),
         xlab = "Predicted Values",
         ylab = "Average Residual",
         pch = 16,
         ylim = c(-max(abs(binned_residuals$residuals) + 0.05), 
                 max(abs(binned_residuals$residuals) + 0.05)))
    abline(h = 0, col = "red", lty = 2)
    
    # Add error bars
    segments(binned_residuals$predicted, 
            binned_residuals$residuals - binned_residuals$ellipse,
            binned_residuals$predicted, 
            binned_residuals$residuals + binned_residuals$ellipse)
    dev.off()
    
    # Save performance metrics
    perf_df <- data.frame(
      metric = c("MSE", "RMSE", "MAE", "Correlation"),
      value = c(mse, rmse, mae, cor_value)
    )
    
    write.csv(perf_df, 
              file = paste0("artifacts/2_liwc/", model_name, "_performance.csv"),
              row.names = FALSE)
    
    # Save predictions and actuals for further analysis
    pred_df <- data.frame(
      actual = test_data$actual_prop,
      predicted = predictions,
      residual = residuals
    )
    
    write.csv(pred_df, 
              file = paste0("artifacts/2_liwc/", model_name, "_predictions.csv"),
              row.names = FALSE)
    
    return(list(
      mse = mse,
      rmse = rmse,
      mae = mae,
      correlation = cor_value
    ))
  } else {
    cat("Error: Unsupported response variable format\n")
    return(NULL)
  }
}

# Function to compare nested models
compare_nested_models <- function(top_model_result, bottom_model_result, 
                                 comparison_name = "comparison") {
  cat("\nComparing models:", comparison_name, "\n")
  
  if (is.null(top_model_result) || is.null(bottom_model_result)) {
    cat("Error: One or both models are NULL, cannot compare\n")
    return(NULL)
  }
  
  top_model <- top_model_result$model
  bottom_model <- bottom_model_result$model
  
  # Create comparison dataframe
  comparison <- data.frame(
    metric = character(),
    top_model = numeric(),
    bottom_model = numeric(),
    difference = numeric(),
    better_model = character(),
    stringsAsFactors = FALSE
  )
  
  # Add AIC and BIC
  comparison <- rbind(comparison, data.frame(
    metric = "AIC",
    top_model = top_model_result$aic,
    bottom_model = bottom_model_result$aic,
    difference = bottom_model_result$aic - top_model_result$aic,
    better_model = ifelse(bottom_model_result$aic < top_model_result$aic, "bottom", "top"),
    stringsAsFactors = FALSE
  ))
  
  comparison <- rbind(comparison, data.frame(
    metric = "BIC",
    top_model = top_model_result$bic,
    bottom_model = bottom_model_result$bic,
    difference = bottom_model_result$bic - top_model_result$bic,
    better_model = ifelse(bottom_model_result$bic < top_model_result$bic, "bottom", "top"),
    stringsAsFactors = FALSE
  ))
  
  # Add performance metrics if available
  if (!is.null(top_model_result$performance) && !is.null(bottom_model_result$performance)) {
    # For binary classification models
    if (!is.null(top_model_result$performance$auc)) {
      metrics <- c("auc", "accuracy", "precision", "recall", "f1_score")
      better_direction <- c(1, 1, 1, 1, 1)  # 1 means higher is better
      
      for (i in seq_along(metrics)) {
        metric <- metrics[i]
        top_value <- top_model_result$performance[[metric]]
        bottom_value <- bottom_model_result$performance[[metric]]
        diff <- bottom_value - top_value
        better <- ifelse((diff * better_direction[i]) > 0, "bottom", 
                        ifelse((diff * better_direction[i]) < 0, "top", "tie"))
        
        comparison <- rbind(comparison, data.frame(
          metric = toupper(metric),
          top_model = top_value,
          bottom_model = bottom_value,
          difference = diff,
          better_model = better,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      # For regression models
      metrics <- c("mse", "rmse", "mae", "correlation")
      better_direction <- c(-1, -1, -1, 1)  # -1 means lower is better
      
      for (i in seq_along(metrics)) {
        metric <- metrics[i]
        top_value <- top_model_result$performance[[metric]]
        bottom_value <- bottom_model_result$performance[[metric]]
        diff <- bottom_value - top_value
        better <- ifelse((diff * better_direction[i]) > 0, "bottom", 
                        ifelse((diff * better_direction[i]) < 0, "top", "tie"))
        
        comparison <- rbind(comparison, data.frame(
          metric = toupper(metric),
          top_model = top_value,
          bottom_model = bottom_value,
          difference = diff,
          better_model = better,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Print and save comparison
  cat("\nModel comparison results:\n")
  print(comparison)
  
  write.csv(comparison, 
            file = paste0("artifacts/2_liwc/", comparison_name, "_comparison.csv"),
            row.names = FALSE)
  
  # Count which model is better more often
  better_counts <- table(comparison$better_model)
  top_count <- if ("top" %in% names(better_counts)) better_counts["top"] else 0
  bottom_count <- if ("bottom" %in% names(better_counts)) better_counts["bottom"] else 0
  tie_count <- if ("tie" %in% names(better_counts)) better_counts["tie"] else 0
  
  cat("\nOverall comparison:\n")
  cat("Top model better on", top_count, "metrics\n")
  cat("Bottom model better on", bottom_count, "metrics\n")
  cat("Tied on", tie_count, "metrics\n")
  
  # Determine overall winner
  winner <- if (bottom_count > top_count) {
    "bottom"
  } else if (top_count > bottom_count) {
    "top"
  } else {
    # If tied, use AIC as tiebreaker
    ifelse(bottom_model_result$aic < top_model_result$aic, "bottom", "top")
  }
  
  cat("Overall winner:", winner, "model\n")
  
  # Return comparison results
  return(list(
    comparison = comparison,
    winner = winner,
    top_better_count = top_count,
    bottom_better_count = bottom_count,
    tie_count = tie_count
  ))
}

# Main function to run all nesting model comparisons
run_nesting_comparisons <- function() {
  # Set up results tables
  overall_results <- data.frame(
    comparison = character(),
    top_aic = numeric(),
    bottom_aic = numeric(),
    aic_diff = numeric(),
    top_bic = numeric(),
    bottom_bic = numeric(),
    bic_diff = numeric(),
    winner = character(),
    stringsAsFactors = FALSE
  )
  
  # First, test the overall nesting structure (top vs bottom)
  cat("\n============================================================\n")
  cat("TESTING OVERALL NESTING STRUCTURE (TOP vs BOTTOM)\n")
  cat("============================================================\n")
  
  # For aggregated_binomial_positive table (has at least one censored post)
  top_predictors <- nesting_overall$top
  bottom_predictors <- nesting_overall$bottom
  
  # Build top model
  top_model_result <- build_and_evaluate_model(
    response = c("POSITIVE", "TOTAL"),
    predictors = top_predictors,
    table = "aggregated_binomial_positive",
    model_name = "top_overall_model",
    family = binomial_corrected
  )
  
  # Evaluate top model performance
  if (!is.null(top_model_result)) {
    top_model_result$performance <- evaluate_model_performance(
      model = top_model_result$model,
      response = c("POSITIVE", "TOTAL"),
      predictors = top_predictors,
      table = "aggregated_binomial_positive",
      model_name = "top_overall_model"
    )
  }
  
  # Build bottom model
  bottom_model_result <- build_and_evaluate_model(
    response = c("POSITIVE", "TOTAL"),
    predictors = bottom_predictors,
    table = "aggregated_binomial_positive",
    model_name = "bottom_overall_model",
    family = binomial_corrected
  )
  
  # Evaluate bottom model performance
  if (!is.null(bottom_model_result)) {
    bottom_model_result$performance <- evaluate_model_performance(
      model = bottom_model_result$model,
      response = c("POSITIVE", "TOTAL"),
      predictors = bottom_predictors,
      table = "aggregated_binomial_positive",
      model_name = "bottom_overall_model"
    )
  }
  
  # Compare models
  if (!is.null(top_model_result) && !is.null(bottom_model_result)) {
    comparison_result <- compare_nested_models(
      top_model_result = top_model_result,
      bottom_model_result = bottom_model_result,
      comparison_name = "overall_nesting"
    )
    
    # Add to overall results
    overall_results <- rbind(overall_results, data.frame(
      comparison = "Overall Nesting",
      top_aic = top_model_result$aic,
      bottom_aic = bottom_model_result$aic,
      aic_diff = bottom_model_result$aic - top_model_result$aic,
      top_bic = top_model_result$bic,
      bottom_bic = bottom_model_result$bic,
      bic_diff = bottom_model_result$bic - top_model_result$bic,
      winner = comparison_result$winner,
      stringsAsFactors = FALSE
    ))
  }
  
  # Now test each individual nesting structure (within categories)
  cat("\n============================================================\n")
  cat("TESTING INDIVIDUAL NESTING STRUCTURES\n")
  cat("============================================================\n")
  
  # For each nesting group in nesting_bottom
  for (group_name in names(nesting_bottom)) {
    cat("\n------------------------------------------------------------\n")
    cat("Testing nesting for:", group_name, "\n")
    cat("------------------------------------------------------------\n")
    
    # Get top and bottom predictors
    top_predictor <- nesting_bottom[[group_name]]$top
    bottom_predictors <- nesting_bottom[[group_name]]$bottom
    
    # Build top model (the single top-level predictor)
    top_model_result <- build_and_evaluate_model(
      response = c("POSITIVE", "TOTAL"),
      predictors = top_predictor,
      table = "aggregated_binomial_positive",
      model_name = paste0(group_name, "_top_model"),
      family = binomial_corrected
    )
    
    # Evaluate top model
    if (!is.null(top_model_result)) {
      top_model_result$performance <- evaluate_model_performance(
        model = top_model_result$model,
        response = c("POSITIVE", "TOTAL"),
        predictors = top_predictor,
        table = "aggregated_binomial_positive",
        model_name = paste0(group_name, "_top_model")
      )
    }
    
    # Build bottom model (all nested bottom-level predictors)
    bottom_model_result <- build_and_evaluate_model(
      response = c("POSITIVE", "TOTAL"),
      predictors = bottom_predictors,
      table = "aggregated_binomial_positive",
      model_name = paste0(group_name, "_bottom_model"),
      family = binomial_corrected
    )
    
    # Evaluate bottom model
    if (!is.null(bottom_model_result)) {
      bottom_model_result$performance <- evaluate_model_performance(
        model = bottom_model_result$model,
        response = c("POSITIVE", "TOTAL"),
        predictors = bottom_predictors,
        table = "aggregated_binomial_positive",
        model_name = paste0(group_name, "_bottom_model")
      )
    }
    
    # Compare models
    if (!is.null(top_model_result) && !is.null(bottom_model_result)) {
      comparison_result <- compare_nested_models(
        top_model_result = top_model_result,
        bottom_model_result = bottom_model_result,
        comparison_name = paste0(group_name, "_nesting")
      )
      
      # Add to overall results
      overall_results <- rbind(overall_results, data.frame(
        comparison = group_name,
        top_aic = top_model_result$aic,
        bottom_aic = bottom_model_result$aic,
        aic_diff = bottom_model_result$aic - top_model_result$aic,
        top_bic = top_model_result$bic,
        bottom_bic = bottom_model_result$bic,
        bic_diff = bottom_model_result$bic - top_model_result$bic,
        winner = comparison_result$winner,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Save overall results
  write.csv(overall_results, 
            file = "artifacts/2_liwc/all_nesting_comparisons.csv",
            row.names = FALSE)
  
  # Create summary of all comparisons
  cat("\n============================================================\n")
  cat("SUMMARY OF ALL NESTING COMPARISONS\n")
  cat("============================================================\n")
  
  winner_counts <- table(overall_results$winner)
  cat("Top model better in", 
      if ("top" %in% names(winner_counts)) winner_counts["top"] else 0, 
      "comparisons\n")
  cat("Bottom model better in", 
      if ("bottom" %in% names(winner_counts)) winner_counts["bottom"] else 0, 
      "comparisons\n")
  
  # Print details of each comparison
  overall_results$aic_better <- ifelse(overall_results$aic_diff < 0, "bottom", "top")
  overall_results$bic_better <- ifelse(overall_results$bic_diff < 0, "bottom", "top")
  
  cat("\nDetailed comparison results:\n")
  print(overall_results[, c("comparison", "aic_diff", "bic_diff", "winner")])
  
  # Return overall results
  return(overall_results)
}

# Run all nesting comparisons
nesting_results <- run_nesting_comparisons()

# Disconnect from database
disconnectdB()

cat("\nNesting model analysis complete. Results saved to artifacts/2_liwc directory.\n")
