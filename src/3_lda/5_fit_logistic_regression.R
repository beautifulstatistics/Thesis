# 5_fit_logistic_regression.R
# Fit logistic regression on LDA topic distributions to predict censorship
# Uses speedglm for streaming/chunked processing of 226M posts

source("src/utils/helper_functions.R")
library(speedglm)
library(ggplot2)
library(pROC)

cat("============================================================\n")
cat("LDA LOGISTIC REGRESSION MODEL\n")
cat("============================================================\n")

# Create artifacts directory
if (!dir.exists("artifacts/3_lda")) {
  dir.create("artifacts/3_lda", recursive = TRUE)
}

# Connect to database
connectdB()

# -----------------------------------------------------------------------------
# 1. Check LDA distributions table exists and get topic count
# -----------------------------------------------------------------------------

cat("\nChecking LDA distributions table...\n")

table_check <- dbGetQuery(conn,
  "SELECT name FROM sqlite_master WHERE type='table' AND name='lda_distributions'")

if (nrow(table_check) == 0) {
  stop("lda_distributions table not found. Please run 4_generate_distributions.py first.")
}

# Get topic columns
lda_cols <- dbListFields(conn, "lda_distributions")
topic_cols <- lda_cols[grepl("^topic_", lda_cols)]
n_topics <- length(topic_cols)

cat("Found", n_topics, "topic columns\n")

# Get row counts
lda_count <- dbGetQuery(conn, "SELECT COUNT(*) as n FROM lda_distributions")$n
all_count <- dbGetQuery(conn, "SELECT COUNT(*) as n FROM all_data WHERE permission_denied IS NOT NULL")$n

cat("LDA distributions:", format(lda_count, big.mark=","), "rows\n")
cat("Posts with censorship label:", format(all_count, big.mark=","), "rows\n")

# -----------------------------------------------------------------------------
# 2. Create joined view for model fitting
# -----------------------------------------------------------------------------

cat("\nCreating joined view...\n")

dbExecute(conn, "DROP VIEW IF EXISTS lda_censorship")

view_sql <- paste0("
  CREATE VIEW lda_censorship AS
  SELECT
    a.permission_denied,
    ", paste(paste0("l.", topic_cols), collapse = ", "), "
  FROM all_data a
  INNER JOIN lda_distributions l ON a.ROWID = l.rowid
  WHERE a.permission_denied IS NOT NULL
")

dbExecute(conn, view_sql)

# Verify view
view_count <- dbGetQuery(conn, "SELECT COUNT(*) as n FROM lda_censorship")$n
cat("Joined view has", format(view_count, big.mark=","), "rows\n")

# -----------------------------------------------------------------------------
# 3. Fit logistic regression using shlm (chunked speedglm)
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("FITTING LOGISTIC REGRESSION MODEL\n")
cat("============================================================\n")

# Build formula
predictors <- topic_cols
response <- "permission_denied"
formula <- make.formula(response, predictors)

cat("Formula:", deparse(formula), "\n")

# Create data function for chunked processing
data_fun <- make.data(
  response = response,
  predictors = predictors,
  table = "lda_censorship",
  chunksize = 1000000  # 1M rows per chunk
)

# Fit model
cat("\nFitting model (this may take a while)...\n")
start_time <- Sys.time()

model <- shlm(formula, data_fun, family = binomial())

end_time <- Sys.time()
cat("Model fitting completed in", format(end_time - start_time), "\n")

# -----------------------------------------------------------------------------
# 4. Model summary and coefficients
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("MODEL SUMMARY\n")
cat("============================================================\n")

model_summary <- summary(model)
print(model_summary)

# Calculate AIC and BIC
model_aic <- AIC(model)
model_bic <- bic(model)

cat("\nAIC:", model_aic, "\n")
cat("BIC:", model_bic, "\n")
cat("Observations:", model$n, "\n")
cat("Deviance:", model$deviance, "\n")
cat("Null deviance:", model$nulldev, "\n")

# Extract coefficients
coefs <- summary(model)$coefficients
coef_df <- data.frame(
  predictor = rownames(coefs),
  estimate = coefs[, 1],
  std_error = coefs[, 2],
  z_value = coefs[, 3],
  p_value = coefs[, 4],
  odds_ratio = exp(coefs[, 1]),
  stringsAsFactors = FALSE
)
rownames(coef_df) <- NULL

# Sort by absolute effect size
coef_df <- coef_df[order(abs(coef_df$estimate), decreasing = TRUE), ]

cat("\nTop 10 predictors by effect size:\n")
print(head(coef_df, 10))

# Significant predictors
sig_preds <- get_sigpreds(model)

cat("\nSignificant predictors making censorship MORE likely:\n")
if (nrow(sig_preds$more_likely) > 0) {
  print(sig_preds$more_likely)
} else {
  cat("None found\n")
}

cat("\nSignificant predictors making censorship LESS likely:\n")
if (nrow(sig_preds$less_likely) > 0) {
  print(sig_preds$less_likely)
} else {
  cat("None found\n")
}

# Save coefficients
write.csv(coef_df, "artifacts/3_lda/lda_coefficients.csv", row.names = FALSE)
write.csv(sig_preds$more_likely, "artifacts/3_lda/lda_positive_predictors.csv", row.names = FALSE)
write.csv(sig_preds$less_likely, "artifacts/3_lda/lda_negative_predictors.csv", row.names = FALSE)

# Save model summary
sink("artifacts/3_lda/lda_model_summary.txt")
print(model_summary)
cat("\nAIC:", model_aic, "\n")
cat("BIC:", model_bic, "\n")
cat("\nObservations:", model$n, "\n")
sink()

# Save model object
saveRDS(list(
  model = model,
  predictors = predictors,
  n_topics = n_topics,
  aic = model_aic,
  bic = model_bic
), file = "artifacts/3_lda/lda_model.rds")

cat("\nModel saved to artifacts/3_lda/lda_model.rds\n")

# -----------------------------------------------------------------------------
# 5. Residual analysis (sampled for memory efficiency)
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("RESIDUAL ANALYSIS\n")
cat("============================================================\n")

# Sample for residual analysis (full data too large for memory)
sample_size <- 500000
cat("Sampling", format(sample_size, big.mark=","), "observations for residual analysis...\n")

sample_query <- paste0("
  SELECT permission_denied, ", paste(topic_cols, collapse = ", "), "
  FROM lda_censorship
  ORDER BY RANDOM()
  LIMIT ", sample_size
)

sample_data <- dbGetQuery(conn, sample_query)
cat("Loaded", nrow(sample_data), "observations\n")

# Compute predictions
cat("Computing predictions...\n")
fitted_probs <- predict(model, newdata = sample_data, type = "response")

# Actual values
actual <- sample_data$permission_denied

# Raw residuals
raw_residuals <- actual - fitted_probs

# Pearson residuals
pearson_residuals <- (actual - fitted_probs) / sqrt(fitted_probs * (1 - fitted_probs))

# Deviance residuals
deviance_residuals <- sign(actual - fitted_probs) * sqrt(
  -2 * (actual * log(ifelse(actual == 0, 1, fitted_probs)) +
        (1 - actual) * log(ifelse(actual == 1, 1, 1 - fitted_probs)))
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
cat("\n--- Binned Residual Analysis ---\n")

binned_res <- bin.residuals(
  predicted = fitted_probs,
  actual = actual,
  nbins = 100
)

cat("Mean binned residual:", round(mean(binned_res$residuals), 6), "\n")
cat("SD binned residual:", round(sd(binned_res$residuals), 4), "\n")
cat("Bins outside 2SE:", sum(abs(binned_res$residuals) > 2 * binned_res$ellipse), "\n")

# Save binned residuals
write.csv(binned_res, "artifacts/3_lda/binned_residuals.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 6. Generate plots
# -----------------------------------------------------------------------------

cat("\nGenerating plots...\n")

# Plot 1: Binned Residuals vs Predicted
p1 <- ggplot(binned_res, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = -2 * ellipse, ymax = 2 * ellipse),
              alpha = 0.2, fill = "blue") +
  labs(title = "Binned Residuals vs Predicted Values",
       subtitle = "LDA Topic Model",
       x = "Predicted Probability",
       y = "Average Residual") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("artifacts/3_lda/binned_residuals_plot.png", plot = p1,
       width = 10, height = 6, dpi = 150)
cat("Saved: artifacts/3_lda/binned_residuals_plot.png\n")

# Plot 2: ROC Curve
roc_obj <- roc(actual, fitted_probs)
auc_value <- auc(roc_obj)

cat("\nAUC:", round(auc_value, 4), "\n")

png("artifacts/3_lda/roc_curve.png", width = 800, height = 600, res = 150)
plot(roc_obj, main = paste("ROC Curve - LDA Model (AUC =", round(auc_value, 3), ")"),
     col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()
cat("Saved: artifacts/3_lda/roc_curve.png\n")

# Plot 3: Coefficient plot
coef_plot_df <- coef_df[coef_df$predictor != "(Intercept)", ]
coef_plot_df$predictor <- factor(coef_plot_df$predictor,
                                  levels = coef_plot_df$predictor[order(coef_plot_df$estimate)])

p3 <- ggplot(coef_plot_df, aes(x = estimate, y = predictor)) +
  geom_point(aes(color = ifelse(p_value < 0.05, "Significant", "Not significant"))) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std_error,
                     xmax = estimate + 1.96 * std_error),
                 height = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Significant" = "blue", "Not significant" = "gray")) +
  labs(title = "LDA Topic Coefficients",
       x = "Log-Odds (Coefficient)",
       y = "Topic",
       color = "Significance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

ggsave("artifacts/3_lda/coefficient_plot.png", plot = p3,
       width = 10, height = max(6, n_topics * 0.3), dpi = 150)
cat("Saved: artifacts/3_lda/coefficient_plot.png\n")

# Plot 4: Pearson residuals histogram
resid_data <- data.frame(pearson = pearson_residuals[!is.na(pearson_residuals) &
                                                       is.finite(pearson_residuals)])

p4 <- ggplot(resid_data, aes(x = pearson)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Pearson Residuals",
       subtitle = "LDA Model",
       x = "Pearson Residual",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("artifacts/3_lda/pearson_residuals_hist.png", plot = p4,
       width = 10, height = 6, dpi = 150)
cat("Saved: artifacts/3_lda/pearson_residuals_hist.png\n")

# -----------------------------------------------------------------------------
# 7. Model fit statistics
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("MODEL FIT STATISTICS\n")
cat("============================================================\n")

# Confusion matrix at 0.5 threshold
threshold <- 0.5
predicted_class <- ifelse(fitted_probs >= threshold, 1, 0)
conf_matrix <- table(Actual = actual, Predicted = predicted_class)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2,2] / sum(conf_matrix[,2])
recall <- conf_matrix[2,2] / sum(conf_matrix[2,])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Confusion Matrix:\n")
print(conf_matrix)

cat("\nAccuracy:", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall:", round(recall, 4), "\n")
cat("F1 Score:", round(f1_score, 4), "\n")
cat("AUC:", round(auc_value, 4), "\n")

# Dispersion
dispersion <- sum(pearson_residuals^2, na.rm = TRUE) / (length(pearson_residuals) - length(coef(model)))
cat("Dispersion parameter:", round(dispersion, 4), "\n")

if (dispersion > 1.5) {
  cat("  -> Evidence of overdispersion\n")
} else {
  cat("  -> No strong overdispersion\n")
}

# Save all fit statistics
fit_stats <- data.frame(
  metric = c("AIC", "BIC", "AUC", "Accuracy", "Precision", "Recall", "F1_Score",
             "Dispersion", "N_observations", "N_topics",
             "Mean_raw_residual", "SD_raw_residual", "Bins_outside_2SE"),
  value = c(model_aic, model_bic, auc_value, accuracy, precision, recall, f1_score,
            dispersion, model$n, n_topics,
            mean(raw_residuals, na.rm = TRUE), sd(raw_residuals, na.rm = TRUE),
            sum(abs(binned_res$residuals) > 2 * binned_res$ellipse))
)

write.csv(fit_stats, "artifacts/3_lda/model_fit_statistics.csv", row.names = FALSE)
cat("\nSaved: artifacts/3_lda/model_fit_statistics.csv\n")

# Save residuals sample
residual_df <- data.frame(
  actual = actual,
  predicted = fitted_probs,
  raw_residual = raw_residuals,
  pearson_residual = pearson_residuals,
  deviance_residual = deviance_residuals
)
write.csv(residual_df, "artifacts/3_lda/residuals_sample.csv", row.names = FALSE)
cat("Saved: artifacts/3_lda/residuals_sample.csv\n")

# -----------------------------------------------------------------------------
# 8. Cleanup
# -----------------------------------------------------------------------------

# Drop the temporary view
dbExecute(conn, "DROP VIEW IF EXISTS lda_censorship")

disconnectdB()

cat("\n============================================================\n")
cat("LDA LOGISTIC REGRESSION COMPLETE\n")
cat("============================================================\n")
cat("Results saved to artifacts/3_lda/\n")
