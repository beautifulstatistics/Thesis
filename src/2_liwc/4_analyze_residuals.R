# 4_analyze_residuals.R
# Residual analysis for LIWC-based censorship models
# Computes and visualizes binned residuals to assess model fit

source("src/utils/helper_functions.R")
source("src/utils/models.R")
library(RSQLite)
library(ggplot2)

cat("============================================================\n")
cat("RESIDUAL ANALYSIS FOR LIWC MODELS\n")
cat("============================================================\n")

# Create output directory
if (!dir.exists("artifacts/2_liwc")) {
  dir.create("artifacts/2_liwc", recursive = TRUE)
}

# Connect to database
connectdB()

# Load the VIF-optimized model
cat("\nLoading VIF-optimized model...\n")
model_result <- readRDS("artifacts/2_liwc/vif_optimal_model.rds")
model <- model_result$model
predictors <- model_result$predictors

cat("Model type:", model_result$model_type, "\n")
cat("Predictors:", length(predictors), "\n")

# Load data for residual analysis
cat("\nLoading data for residual analysis...\n")
data <- dbGetQuery(conn, "SELECT * FROM aggregated_binomial_positive")
cat("Observations:", nrow(data), "\n")

# Compute predictions
cat("\nComputing predictions...\n")
fitted_probs <- predict(model, newdata = data, type = "response")

# Compute actual proportions
actual_props <- data$POSITIVE / data$TOTAL

# Compute raw residuals
raw_residuals <- actual_props - fitted_probs

# Compute Pearson residuals
pearson_residuals <- (actual_props - fitted_probs) /
  sqrt(fitted_probs * (1 - fitted_probs) / data$TOTAL)

# Compute deviance residuals
deviance_residuals <- sign(actual_props - fitted_probs) * sqrt(
  2 * data$TOTAL * (
    ifelse(actual_props > 0, actual_props * log(actual_props / fitted_probs), 0) +
    ifelse(actual_props < 1, (1 - actual_props) * log((1 - actual_props) / (1 - fitted_probs)), 0)
  )
)
deviance_residuals[is.nan(deviance_residuals)] <- 0

cat("\n--- Raw Residual Summary ---\n")
cat("Mean:", round(mean(raw_residuals, na.rm = TRUE), 6), "\n")
cat("SD:", round(sd(raw_residuals, na.rm = TRUE), 4), "\n")
cat("Min:", round(min(raw_residuals, na.rm = TRUE), 4), "\n")
cat("Max:", round(max(raw_residuals, na.rm = TRUE), 4), "\n")

cat("\n--- Pearson Residual Summary ---\n")
cat("Mean:", round(mean(pearson_residuals, na.rm = TRUE), 4), "\n")
cat("SD:", round(sd(pearson_residuals, na.rm = TRUE), 4), "\n")

# Binned residual analysis
cat("\n============================================================\n")
cat("BINNED RESIDUAL ANALYSIS\n")
cat("============================================================\n")

# Compute binned residuals using helper function
binned_res <- bin.residuals(predicted = fitted_probs,
                            actual = actual_props,
                            nbins = 100)

cat("\nBinned residual summary (100 bins):\n")
cat("Mean binned residual:", round(mean(binned_res$residuals), 6), "\n")
cat("SD binned residual:", round(sd(binned_res$residuals), 4), "\n")
cat("Bins outside 2SE:", sum(abs(binned_res$residuals) > 2 * binned_res$ellipse), "\n")

# Save binned residuals
write.csv(binned_res, "artifacts/2_liwc/binned_residuals.csv", row.names = FALSE)
cat("Saved: artifacts/2_liwc/binned_residuals.csv\n")

# Plot 1: Binned Residuals vs Predicted
cat("\nGenerating plots...\n")

p1 <- ggplot(binned_res, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = -2 * ellipse, ymax = 2 * ellipse),
              alpha = 0.2, fill = "blue") +
  labs(title = "Binned Residuals vs Predicted Values",
       subtitle = "LIWC Model (VIF-optimized)",
       x = "Predicted Probability",
       y = "Average Residual") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("artifacts/2_liwc/binned_residuals_plot.png", plot = p1,
       width = 10, height = 6, dpi = 150)
cat("Saved: artifacts/2_liwc/binned_residuals_plot.png\n")

# Plot 2: Actual vs Predicted scatter
sample_idx <- sample(1:nrow(data), min(10000, nrow(data)))
plot_data <- data.frame(
  actual = actual_props[sample_idx],
  predicted = fitted_probs[sample_idx]
)

p2 <- ggplot(plot_data, aes(x = predicted, y = actual)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Proportions",
       subtitle = paste0("Sample of ", nrow(plot_data), " observations"),
       x = "Predicted Proportion",
       y = "Actual Proportion") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("artifacts/2_liwc/actual_vs_predicted.png", plot = p2,
       width = 8, height = 8, dpi = 150)
cat("Saved: artifacts/2_liwc/actual_vs_predicted.png\n")

# Plot 3: Pearson residuals histogram
resid_data <- data.frame(pearson = pearson_residuals[!is.na(pearson_residuals) &
                                                       is.finite(pearson_residuals)])

p3 <- ggplot(resid_data, aes(x = pearson)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Pearson Residuals",
       x = "Pearson Residual",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("artifacts/2_liwc/pearson_residuals_hist.png", plot = p3,
       width = 10, height = 6, dpi = 150)
cat("Saved: artifacts/2_liwc/pearson_residuals_hist.png\n")

# Plot 4: Residuals vs each predictor (top predictors only)
cat("\n--- Residuals by Top Predictors ---\n")

# Get significant predictors
sig_preds <- get_sigpreds(model)
top_preds <- head(c(sig_preds$more_likely$predictor, sig_preds$less_likely$predictor), 6)
top_preds <- intersect(top_preds, colnames(data))

if (length(top_preds) > 0) {
  for (pred in top_preds) {
    pred_data <- data.frame(
      predictor = data[[pred]][sample_idx],
      residual = raw_residuals[sample_idx]
    )

    p <- ggplot(pred_data, aes(x = predictor, y = residual)) +
      geom_point(alpha = 0.3, size = 0.5) +
      geom_smooth(method = "loess", color = "red", se = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste("Residuals vs", pred),
           x = pred,
           y = "Residual") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave(paste0("artifacts/2_liwc/residuals_vs_", pred, ".png"), plot = p,
           width = 8, height = 6, dpi = 150)
  }
  cat("Saved residual plots for:", paste(top_preds, collapse = ", "), "\n")
}

# Compute overall fit statistics
cat("\n============================================================\n")
cat("MODEL FIT STATISTICS\n")
cat("============================================================\n")

# Correlation
cor_val <- cor(actual_props, fitted_probs, use = "complete.obs")
cat("Correlation (actual vs predicted):", round(cor_val, 4), "\n")

# R-squared (pseudo)
ss_res <- sum((actual_props - fitted_probs)^2, na.rm = TRUE)
ss_tot <- sum((actual_props - mean(actual_props))^2, na.rm = TRUE)
r_squared <- 1 - ss_res / ss_tot
cat("R-squared:", round(r_squared, 4), "\n")

# MSE, RMSE, MAE
mse <- mean((actual_props - fitted_probs)^2, na.rm = TRUE)
rmse <- sqrt(mse)
mae <- mean(abs(actual_props - fitted_probs), na.rm = TRUE)
cat("MSE:", round(mse, 6), "\n")
cat("RMSE:", round(rmse, 4), "\n")
cat("MAE:", round(mae, 4), "\n")

# Dispersion (for overdispersion check)
dispersion <- sum(pearson_residuals^2, na.rm = TRUE) / (nrow(data) - length(coef(model)))
cat("Dispersion parameter:", round(dispersion, 4), "\n")
if (dispersion > 1.5) {
  cat("  -> Evidence of overdispersion (consider quasibinomial)\n")
} else {
  cat("  -> No strong overdispersion\n")
}

# Save fit statistics
fit_stats <- data.frame(
  metric = c("Correlation", "R_squared", "MSE", "RMSE", "MAE", "Dispersion",
             "Mean_raw_residual", "SD_raw_residual",
             "Bins_outside_2SE"),
  value = c(cor_val, r_squared, mse, rmse, mae, dispersion,
            mean(raw_residuals, na.rm = TRUE), sd(raw_residuals, na.rm = TRUE),
            sum(abs(binned_res$residuals) > 2 * binned_res$ellipse))
)

write.csv(fit_stats, "artifacts/2_liwc/model_fit_statistics.csv", row.names = FALSE)
cat("\nSaved: artifacts/2_liwc/model_fit_statistics.csv\n")

# Save full residuals for further analysis
residual_df <- data.frame(
  actual = actual_props,
  predicted = fitted_probs,
  raw_residual = raw_residuals,
  pearson_residual = pearson_residuals,
  deviance_residual = deviance_residuals,
  weight = data$TOTAL
)

write.csv(residual_df, "artifacts/2_liwc/all_residuals.csv", row.names = FALSE)
cat("Saved: artifacts/2_liwc/all_residuals.csv\n")

# Disconnect
disconnectdB()

cat("\n============================================================\n")
cat("RESIDUAL ANALYSIS COMPLETE\n")
cat("============================================================\n")
