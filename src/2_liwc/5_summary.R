# 5_summary.R
# Summary of LIWC-based censorship prediction analysis
# Aggregates results from all previous scripts into a final report

library(ggplot2)
library(gridExtra)
source("src/utils/helper_functions.R")
source("src/utils/models.R")

cat("============================================================\n")
cat("LIWC CENSORSHIP PREDICTION - SUMMARY REPORT\n")
cat("============================================================\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Create output directory
dir.create("artifacts/2_liwc", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 1. LOAD ALL RESULTS
# ============================================================

cat("Loading results from previous analyses...\n\n")

# Model comparison results (from script 2)
nesting_comparisons <- tryCatch(
  read.csv("artifacts/2_liwc/all_nesting_comparisons.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

# Distribution analysis (from script 3)
dist_analysis <- tryCatch(
  read.csv("artifacts/2_liwc/count_distribution_analysis.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

binning_recs <- tryCatch(
  read.csv("artifacts/2_liwc/binning_recommendations.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

# VIF results (from script 3)
vif_recs <- tryCatch(
  read.csv("artifacts/2_liwc/vif_nesting_recommendation.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

vif_final <- tryCatch(
  read.csv("artifacts/2_liwc/vif_final.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

final_predictors <- tryCatch(
  read.csv("artifacts/2_liwc/vif_final_predictors.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

# Significant predictors (from script 3)
pos_predictors <- tryCatch(
  read.csv("artifacts/2_liwc/vif_positive_predictors.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

neg_predictors <- tryCatch(
  read.csv("artifacts/2_liwc/vif_negative_predictors.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

# Model fit statistics (from script 4)
fit_stats <- tryCatch(
  read.csv("artifacts/2_liwc/model_fit_statistics.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

# Load the final model
model_result <- tryCatch(
  readRDS("artifacts/2_liwc/vif_optimal_model.rds"),
  error = function(e) NULL
)

# ============================================================
# 2. NESTING COMPARISON SUMMARY
# ============================================================

cat("============================================================\n")
cat("1. NESTING COMPARISON (TOP vs BOTTOM)\n")
cat("============================================================\n\n")

if (!is.null(nesting_comparisons)) {
  cat("Question: Do fine-grained (bottom) categories predict better than\n")
  cat("          aggregate (top) categories?\n\n")

  winner_counts <- table(nesting_comparisons$winner)
  cat("Results across", nrow(nesting_comparisons), "nesting comparisons:\n")
  cat("  Top model wins:", ifelse("top" %in% names(winner_counts), winner_counts["top"], 0), "\n")
  cat("  Bottom model wins:", ifelse("bottom" %in% names(winner_counts), winner_counts["bottom"], 0), "\n\n")

  # Overall nesting result
  overall <- nesting_comparisons[nesting_comparisons$comparison == "Overall Nesting", ]
  if (nrow(overall) > 0) {
    cat("Overall nesting (all categories):\n")
    cat("  Winner:", overall$winner, "\n")
    cat("  AIC difference:", round(overall$aic_diff, 2),
        ifelse(overall$aic_diff < 0, "(bottom better)", "(top better)"), "\n")
    cat("  BIC difference:", round(overall$bic_diff, 2),
        ifelse(overall$bic_diff < 0, "(bottom better)", "(top better)"), "\n")
  }
  cat("\n")
} else {
  cat("Nesting comparison results not found.\n\n")
}

# ============================================================
# 3. DISTRIBUTION ANALYSIS SUMMARY
# ============================================================

cat("============================================================\n")
cat("2. PREDICTOR DISTRIBUTION ANALYSIS\n")
cat("============================================================\n\n")

if (!is.null(dist_analysis)) {
  cat("Question: Should predictors use binary (0 vs 1+) or count bins?\n\n")

  n_binary <- sum(dist_analysis$recommendation == "binary")
  n_bins <- sum(dist_analysis$recommendation == "count_bins")

  cat("Recommendations:\n")
  cat("  Binary (uniform censorship rate):", n_binary, "predictors\n")
  cat("  Count bins (varying censorship rate):", n_bins, "predictors\n\n")

  cat("Pattern distribution:\n")
  print(table(dist_analysis$pattern))
  cat("\n")

  # Most prevalent predictors
  top_prevalent <- dist_analysis[order(-dist_analysis$pct_nonzero), ][1:10, ]
  cat("Most prevalent predictors (% posts with count > 0):\n")
  for (i in 1:nrow(top_prevalent)) {
    cat(sprintf("  %-20s %5.1f%%\n", top_prevalent$predictor[i], top_prevalent$pct_nonzero[i]))
  }
  cat("\n")
} else {
  cat("Distribution analysis results not found.\n\n")
}

# ============================================================
# 4. VIF-BASED NESTING SUMMARY
# ============================================================

cat("============================================================\n")
cat("3. VIF-BASED NESTING DECISIONS\n")
cat("============================================================\n\n")

if (!is.null(vif_recs)) {
  cat("Question: Which nested groups should collapse to parent due to\n")
  cat("          multicollinearity (VIF > 10)?\n\n")

  n_top <- sum(vif_recs$recommended == "top")
  n_bottom <- sum(vif_recs$recommended == "bottom")

  cat("Decisions:\n")
  cat("  Keep children (bottom):", n_bottom, "groups\n")
  cat("  Collapse to parent (top):", n_top, "groups\n\n")

  if (n_top > 0) {
    cat("Groups collapsed to parent:\n")
    collapsed <- vif_recs$nesting[vif_recs$recommended == "top"]
    for (g in collapsed) {
      cat("  -", g, "\n")
    }
    cat("\n")
  }
}

if (!is.null(vif_final)) {
  cat("Final model VIF summary:\n")
  cat("  Total predictors:", nrow(vif_final), "\n")
  cat("  VIF > 10 (high):", sum(vif_final$vif > 10), "\n")
  cat("  VIF > 5 (moderate):", sum(vif_final$vif > 5), "\n")
  cat("  Max VIF:", round(max(vif_final$vif), 2), "(", vif_final$predictor[which.max(vif_final$vif)], ")\n")
  cat("  Mean VIF:", round(mean(vif_final$vif), 2), "\n\n")
}

if (!is.null(final_predictors)) {
  cat("Final predictor set (", nrow(final_predictors), " predictors):\n")
  cat(paste(final_predictors$predictor, collapse = ", "), "\n\n")
}

# ============================================================
# 5. SIGNIFICANT PREDICTORS
# ============================================================

cat("============================================================\n")
cat("4. SIGNIFICANT PREDICTORS OF CENSORSHIP\n")
cat("============================================================\n\n")

if (!is.null(pos_predictors) && nrow(pos_predictors) > 0) {
  cat("Predictors INCREASING censorship likelihood:\n")
  n_show <- min(15, nrow(pos_predictors))
  for (i in 1:n_show) {
    cat(sprintf("  %-20s coef=%7.4f  OR=%5.2f  p=%s\n",
                pos_predictors$predictor[i],
                pos_predictors$estimate[i],
                exp(pos_predictors$estimate[i]),
                format(pos_predictors$p_value[i], digits = 3, scientific = TRUE)))
  }
  if (nrow(pos_predictors) > n_show) {
    cat("  ... and", nrow(pos_predictors) - n_show, "more\n")
  }
  cat("\n")
} else {
  cat("No significant positive predictors found.\n\n")
}

if (!is.null(neg_predictors) && nrow(neg_predictors) > 0) {
  cat("Predictors DECREASING censorship likelihood:\n")
  n_show <- min(15, nrow(neg_predictors))
  for (i in 1:n_show) {
    cat(sprintf("  %-20s coef=%7.4f  OR=%5.2f  p=%s\n",
                neg_predictors$predictor[i],
                neg_predictors$estimate[i],
                exp(neg_predictors$estimate[i]),
                format(neg_predictors$p_value[i], digits = 3, scientific = TRUE)))
  }
  if (nrow(neg_predictors) > n_show) {
    cat("  ... and", nrow(neg_predictors) - n_show, "more\n")
  }
  cat("\n")
} else {
  cat("No significant negative predictors found.\n\n")
}

# ============================================================
# 6. MODEL FIT SUMMARY
# ============================================================

cat("============================================================\n")
cat("5. MODEL FIT STATISTICS\n")
cat("============================================================\n\n")

if (!is.null(model_result)) {
  cat("Model type:", model_result$model_type, "\n")
  cat("Number of predictors:", length(model_result$predictors), "\n")
  cat("Dispersion parameter:", round(model_result$dispersion, 4), "\n\n")
}

if (!is.null(fit_stats)) {
  cat("Performance metrics:\n")
  for (i in 1:nrow(fit_stats)) {
    val <- fit_stats$value[i]
    fmt <- if (abs(val) < 0.01) format(val, scientific = TRUE, digits = 3) else round(val, 4)
    cat(sprintf("  %-20s %s\n", fit_stats$metric[i], fmt))
  }
  cat("\n")

  # Interpretation
  r2 <- fit_stats$value[fit_stats$metric == "R_squared"]
  disp <- fit_stats$value[fit_stats$metric == "Dispersion"]
  bins_out <- fit_stats$value[fit_stats$metric == "Bins_outside_2SE"]

  if (length(r2) > 0) {
    cat("Interpretation:\n")
    if (r2 < 0.1) {
      cat("  - R-squared is low; LIWC features explain limited variance\n")
    } else if (r2 < 0.3) {
      cat("  - R-squared is moderate; LIWC features have some predictive power\n")
    } else {
      cat("  - R-squared is good; LIWC features explain substantial variance\n")
    }
  }

  if (length(disp) > 0 && disp > 1.5) {
    cat("  - Overdispersion present; quasibinomial is appropriate\n")
  }

  if (length(bins_out) > 0) {
    cat("  - Bins outside 2SE:", bins_out, "/ 100 (expect ~5 by chance)\n")
    if (bins_out > 10) {
      cat("    -> Some systematic misfit in predictions\n")
    } else {
      cat("    -> Model fit is acceptable\n")
    }
  }
  cat("\n")
}

# ============================================================
# 7. KEY FINDINGS
# ============================================================

cat("============================================================\n")
cat("6. KEY FINDINGS\n")
cat("============================================================\n\n")

findings <- character()

# Finding 1: Nesting
if (!is.null(nesting_comparisons)) {
  overall <- nesting_comparisons[nesting_comparisons$comparison == "Overall Nesting", ]
  if (nrow(overall) > 0) {
    if (overall$winner == "bottom") {
      findings <- c(findings, "Fine-grained LIWC categories outperform aggregate categories (AIC/BIC)")
    } else {
      findings <- c(findings, "Aggregate LIWC categories perform as well as fine-grained (prefer parsimony)")
    }
  }
}

# Finding 2: VIF
if (!is.null(vif_recs)) {
  n_collapsed <- sum(vif_recs$recommended == "top")
  if (n_collapsed > 0) {
    findings <- c(findings, paste0(n_collapsed, " category groups collapsed due to multicollinearity"))
  } else {
    findings <- c(findings, "No multicollinearity issues; all bottom-level predictors retained")
  }
}

# Finding 3: Top predictors
if (!is.null(pos_predictors) && nrow(pos_predictors) > 0) {
  top3 <- head(pos_predictors$predictor, 3)
  findings <- c(findings, paste0("Top censorship predictors: ", paste(top3, collapse = ", ")))
}

if (!is.null(neg_predictors) && nrow(neg_predictors) > 0) {
  top3 <- head(neg_predictors$predictor, 3)
  findings <- c(findings, paste0("Top protective factors: ", paste(top3, collapse = ", ")))
}

# Finding 4: Model fit
if (!is.null(fit_stats)) {
  r2 <- fit_stats$value[fit_stats$metric == "R_squared"]
  if (length(r2) > 0) {
    findings <- c(findings, paste0("Model explains ", round(r2 * 100, 1), "% of variance in censorship rates"))
  }
}

cat("Summary:\n")
for (i in seq_along(findings)) {
  cat(i, ". ", findings[i], "\n", sep = "")
}
cat("\n")

# ============================================================
# 8. SAVE SUMMARY REPORT
# ============================================================

cat("============================================================\n")
cat("7. OUTPUT FILES\n")
cat("============================================================\n\n")

# Create summary data frame
summary_df <- data.frame(
  category = character(),
  item = character(),
  value = character(),
  stringsAsFactors = FALSE
)

# Add nesting comparison
if (!is.null(nesting_comparisons)) {
  overall <- nesting_comparisons[nesting_comparisons$comparison == "Overall Nesting", ]
  if (nrow(overall) > 0) {
    summary_df <- rbind(summary_df, data.frame(
      category = "Nesting", item = "Winner", value = overall$winner, stringsAsFactors = FALSE))
    summary_df <- rbind(summary_df, data.frame(
      category = "Nesting", item = "AIC_diff", value = as.character(round(overall$aic_diff, 2)), stringsAsFactors = FALSE))
  }
}

# Add VIF summary
if (!is.null(vif_final)) {
  summary_df <- rbind(summary_df, data.frame(
    category = "VIF", item = "n_predictors", value = as.character(nrow(vif_final)), stringsAsFactors = FALSE))
  summary_df <- rbind(summary_df, data.frame(
    category = "VIF", item = "max_vif", value = as.character(round(max(vif_final$vif), 2)), stringsAsFactors = FALSE))
  summary_df <- rbind(summary_df, data.frame(
    category = "VIF", item = "mean_vif", value = as.character(round(mean(vif_final$vif), 2)), stringsAsFactors = FALSE))
}

# Add fit stats
if (!is.null(fit_stats)) {
  for (i in 1:nrow(fit_stats)) {
    summary_df <- rbind(summary_df, data.frame(
      category = "Fit", item = fit_stats$metric[i], value = as.character(round(fit_stats$value[i], 6)), stringsAsFactors = FALSE))
  }
}

# Add top predictors
if (!is.null(pos_predictors) && nrow(pos_predictors) > 0) {
  top5 <- head(pos_predictors, 5)
  for (i in 1:nrow(top5)) {
    summary_df <- rbind(summary_df, data.frame(
      category = "Positive_Predictors", item = top5$predictor[i],
      value = as.character(round(top5$estimate[i], 4)), stringsAsFactors = FALSE))
  }
}

if (!is.null(neg_predictors) && nrow(neg_predictors) > 0) {
  top5 <- head(neg_predictors, 5)
  for (i in 1:nrow(top5)) {
    summary_df <- rbind(summary_df, data.frame(
      category = "Negative_Predictors", item = top5$predictor[i],
      value = as.character(round(top5$estimate[i], 4)), stringsAsFactors = FALSE))
  }
}

write.csv(summary_df, "artifacts/2_liwc/summary_table.csv", row.names = FALSE)
cat("Saved: artifacts/2_liwc/summary_table.csv\n")

# Save findings as text
findings_file <- "artifacts/2_liwc/key_findings.txt"
writeLines(c(
  "LIWC CENSORSHIP PREDICTION - KEY FINDINGS",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  findings
), findings_file)
cat("Saved: artifacts/2_liwc/key_findings.txt\n")

# List all output files
cat("\nAll artifacts in artifacts/2_liwc/:\n")
files <- list.files("artifacts/2_liwc", full.names = FALSE)
for (f in files) {
  info <- file.info(file.path("artifacts/2_liwc", f))
  cat(sprintf("  %-40s %s\n", f, format(info$size, big.mark = ",")))
}

cat("\n============================================================\n")
cat("SUMMARY COMPLETE\n")
cat("============================================================\n")
