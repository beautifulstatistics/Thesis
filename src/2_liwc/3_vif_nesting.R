# 3_vif_nesting.R
# Distribution analysis and VIF-based nesting decisions for word count models
#
# Part 1: Chi-square homogeneity test to determine binary vs count bins
# Part 2: VIF-based nesting to handle multicollinearity
#
# Uses quasibinomial to handle overdispersion in aggregated data.

library(RSQLite)
library(speedglm)
library(car)
source("src/utils/helper_functions.R")
source("src/utils/models.R")

# Create output directory
dir.create("artifacts/2_liwc", showWarnings = FALSE, recursive = TRUE)

# Connect to database
connectdB()

# ============================================================
# PART 1: CHI-SQUARE HOMOGENEITY TEST FOR BINNING DECISIONS
# ============================================================

cat("============================================================\n")
cat("PART 1: CHI-SQUARE HOMOGENEITY TEST FOR BINNING DECISIONS\n")
cat("============================================================\n\n")

# Test: Among posts with count >= 1, is censorship rate uniform across count values?
# H0: P(censored | count=1) = P(censored | count=2) = ...
# H1: Censorship rates differ by count value
#
# Decision:
# - p > 0.05: Cannot reject uniformity -> use binary (0 vs 1+)
# - p < 0.05: Rates differ significantly -> use count bins

count_predictors <- nesting_overall$bottom

cat("Analyzing", length(count_predictors), "count predictors\n")
cat("Using chi-square test of homogeneity\n")
cat("H0: Censorship rate is uniform across count values (given count >= 1)\n\n")

dist_results <- data.frame(
  predictor = character(),
  n_total = integer(),
  n_nonzero = integer(),
  pct_nonzero = numeric(),
  max_count = integer(),
  unique_counts = integer(),
  chi_sq = numeric(),
  df = integer(),
  p_value = numeric(),
  recommendation = character(),
  pattern = character(),
  stringsAsFactors = FALSE
)

for (pred in count_predictors) {
  cat("---", pred, "---\n")

  # Get distribution of count values with censorship counts
  query <- paste0("
    SELECT ", pred, " as count_val,
           COUNT(*) as n,
           SUM(permission_denied) as n_censored
    FROM numeric
    WHERE ", pred, " >= 0
    GROUP BY ", pred, "
    ORDER BY ", pred)

  dist <- dbGetQuery(conn, query)

  n_total <- sum(dist$n)
  n_nonzero <- sum(dist$n[dist$count_val > 0])
  pct_nonzero <- 100 * n_nonzero / n_total
  max_count <- max(dist$count_val)

  # Filter to count >= 1 for homogeneity test
  nonzero_dist <- dist[dist$count_val > 0, ]
  unique_counts <- nrow(nonzero_dist)

  # Chi-square test of homogeneity
  if (unique_counts >= 2 && sum(nonzero_dist$n_censored) > 0) {
    n_not_censored <- nonzero_dist$n - nonzero_dist$n_censored
    valid_idx <- nonzero_dist$n >= 5

    if (sum(valid_idx) >= 2) {
      cont_table <- cbind(
        censored = nonzero_dist$n_censored[valid_idx],
        not_censored = n_not_censored[valid_idx]
      )

      test_result <- tryCatch({
        chisq.test(cont_table)
      }, error = function(e) {
        chisq.test(cont_table, simulate.p.value = TRUE, B = 2000)
      })

      chi_sq <- test_result$statistic
      df <- test_result$parameter
      p_value <- test_result$p.value

      # Determine pattern
      rates <- nonzero_dist$n_censored / nonzero_dist$n
      if (length(rates) >= 3) {
        mid_idx <- ceiling(length(rates) / 2)
        rate_start <- mean(rates[1:min(3, length(rates))])
        rate_mid <- mean(rates[max(1, mid_idx-1):min(length(rates), mid_idx+1)])
        rate_end <- mean(rates[max(1, length(rates)-2):length(rates)])

        if (rate_end > rate_start * 1.2) {
          pattern <- "increasing"
        } else if (rate_end < rate_start * 0.8) {
          pattern <- "decreasing"
        } else if (rate_mid > max(rate_start, rate_end) * 1.1) {
          pattern <- "inverted-U"
        } else {
          pattern <- "flat/noisy"
        }
      } else {
        pattern <- "sparse"
      }
    } else {
      chi_sq <- NA
      df <- NA
      p_value <- 1
      pattern <- "insufficient_data"
    }
  } else {
    chi_sq <- NA
    df <- NA
    p_value <- 1
    pattern <- "single_value"
  }

  recommendation <- if (is.na(p_value) || p_value > 0.05) "binary" else "count_bins"

  cat("  N non-zero:", format(n_nonzero, big.mark = ","),
      "(", round(pct_nonzero, 1), "%)\n")
  cat("  Unique counts:", unique_counts, "| Max:", max_count, "\n")
  if (!is.na(chi_sq)) {
    cat("  Chi-sq:", round(chi_sq, 2), "| df:", df, "| p:", format(p_value, digits = 4), "\n")
  }
  cat("  Pattern:", pattern, "| Decision:", recommendation, "\n")

  dist_results <- rbind(dist_results, data.frame(
    predictor = pred,
    n_total = n_total,
    n_nonzero = n_nonzero,
    pct_nonzero = round(pct_nonzero, 2),
    max_count = max_count,
    unique_counts = unique_counts,
    chi_sq = ifelse(is.na(chi_sq), NA, round(chi_sq, 2)),
    df = ifelse(is.na(df), NA, df),
    p_value = ifelse(is.na(p_value), NA, p_value),
    recommendation = recommendation,
    pattern = pattern,
    stringsAsFactors = FALSE
  ))
}

cat("\n--- DISTRIBUTION ANALYSIS SUMMARY ---\n")
n_binary <- sum(dist_results$recommendation == "binary")
n_bins <- sum(dist_results$recommendation == "count_bins")
cat("Binary (0 vs 1+):", n_binary, "predictors\n")
cat("Count bins:", n_bins, "predictors\n")

write.csv(dist_results, "artifacts/2_liwc/count_distribution_analysis.csv", row.names = FALSE)
cat("Saved: artifacts/2_liwc/count_distribution_analysis.csv\n")

mapping <- dist_results[, c("predictor", "recommendation", "p_value", "pattern")]
write.csv(mapping, "artifacts/2_liwc/binning_recommendations.csv", row.names = FALSE)
cat("Saved: artifacts/2_liwc/binning_recommendations.csv\n")

# ============================================================
# PART 2: VIF-BASED NESTING ANALYSIS
# ============================================================

cat("\n============================================================\n")
cat("PART 2: VIF-BASED NESTING ANALYSIS\n")
cat("============================================================\n")

VIF_THRESHOLD <- 10  # Target: all VIF below this
USE_QUASI <- TRUE    # Use quasibinomial for overdispersion

cat("VIF threshold:", VIF_THRESHOLD, "\n")
cat("Using quasibinomial:", USE_QUASI, "\n")

model_family <- if (USE_QUASI) quasibinomial_corrected else binomial_corrected

# Load aggregated data
cat("\nLoading aggregated binomial data...\n")
data <- dbGetQuery(conn, "SELECT * FROM aggregated_binomial_positive")
cat("Data loaded:", nrow(data), "observations\n")

fields <- dbGetQuery(conn, "PRAGMA table_info(aggregated_binomial_positive)")
available_cols <- fields$name

# Define nesting structure
nesting_groups <- list(
  functions_pronoun_ppron = list(
    parent = "ppron",
    children = c("i", "we", "you", "shehe", "they", "youpl")
  ),
  functions_pronoun = list(
    parent = "pronoun",
    children = c("ppron", "ipron")
  ),
  functions_tensem = list(
    parent = "tensem",
    children = c("focuspast", "focuspresent", "focusfuture", "progm")
  ),
  functions_particle = list(
    parent = "particle",
    children = c("modal_pa", "general_pa")
  ),
  percept = list(
    parent = "percept",
    children = c("see", "hear", "feel")
  ),
  relativ = list(
    parent = "relativ",
    children = c("motion", "space", "time")
  ),
  social = list(
    parent = "social",
    children = c("family", "friend", "female", "male")
  ),
  drives = list(
    parent = "drives",
    children = c("affiliation", "achieve", "power", "reward", "risk")
  ),
  affect_negemo = list(
    parent = "negemo",
    children = c("anx", "anger", "sad")
  ),
  affect = list(
    parent = "affect",
    children = c("posemo", "negemo")
  ),
  cogproc = list(
    parent = "cogproc",
    children = c("insight", "cause", "discrep", "tentat", "certain", "differ")
  ),
  bio = list(
    parent = "bio",
    children = c("body", "health", "sexual", "ingest")
  ),
  informal = list(
    parent = "informal",
    children = c("swear", "netspeak", "assent", "nonflu", "filler")
  ),
  persconc = list(
    parent = "persconc",
    children = c("work", "leisure", "home", "money", "relig", "death")
  ),
  othergram = list(
    parent = "othergram",
    children = c("compare", "interrog", "number", "quant")
  )
)

current_preds <- intersect(nesting_overall$bottom, available_cols)
cat("Starting with", length(current_preds), "bottom-level predictors\n")

# VIF functions
get_vif_for_predictors <- function(predictors, data) {
  all_predictors <- c(common_control, predictors)
  all_predictors <- intersect(all_predictors, colnames(data))

  formula_str <- paste("cbind(POSITIVE, TOTAL-POSITIVE) ~", paste(all_predictors, collapse = " + "))
  form <- as.formula(formula_str)

  model <- speedglm(form, data = data, family = model_family)

  X <- model.matrix(form, data = data)[, -1]
  vif_values <- numeric(ncol(X))
  names(vif_values) <- colnames(X)

  for (j in 1:ncol(X)) {
    rsq <- summary(lm(X[, j] ~ X[, -j]))$r.squared
    vif_values[j] <- 1 / (1 - rsq)
  }

  return(vif_values)
}

avg_vif_for_group <- function(vif_values, predictors) {
  present <- intersect(names(vif_values), predictors)
  if (length(present) == 0) return(NA)
  return(mean(vif_values[present]))
}

# Iterative VIF-based collapsing
cat("\n--- ITERATIVE VIF-BASED NESTING ---\n")

decisions <- data.frame(
  nesting = character(),
  decision = character(),
  children_avg_vif = numeric(),
  stringsAsFactors = FALSE
)

iteration <- 0
repeat {
  iteration <- iteration + 1
  cat("\n--- Iteration", iteration, "---\n")
  cat("Current predictors:", length(current_preds), "\n")

  vif_values <- tryCatch({
    get_vif_for_predictors(current_preds, data)
  }, error = function(e) {
    cat("Error computing VIF:", e$message, "\n")
    return(NULL)
  })

  if (is.null(vif_values)) break

  max_vif <- max(vif_values, na.rm = TRUE)
  mean_vif <- mean(vif_values, na.rm = TRUE)
  high_vif_count <- sum(vif_values > VIF_THRESHOLD, na.rm = TRUE)

  cat("Max VIF:", round(max_vif, 2), "| Mean VIF:", round(mean_vif, 2), "\n")
  cat("Predictors with VIF >", VIF_THRESHOLD, ":", high_vif_count, "\n")

  if (max_vif <= VIF_THRESHOLD) {
    cat("\nAll VIF below threshold! Done.\n")
    break
  }

  if (iteration > 20) {
    cat("\nMax iterations reached.\n")
    break
  }

  # Find groups to collapse
  collapse_candidates <- list()

  for (name in names(nesting_groups)) {
    group <- nesting_groups[[name]]
    children <- group$children
    parent <- group$parent

    children_in_model <- intersect(children, current_preds)
    if (length(children_in_model) == 0) next
    if (!parent %in% available_cols) next

    avg_vif <- avg_vif_for_group(vif_values, children_in_model)

    if (!is.na(avg_vif) && avg_vif > VIF_THRESHOLD) {
      collapse_candidates[[name]] <- list(
        parent = parent,
        children = children_in_model,
        avg_vif = avg_vif
      )
    }
  }

  if (length(collapse_candidates) == 0) {
    cat("\nNo more groups to collapse, but VIF still high.\n")
    break
  }

  avg_vifs <- sapply(collapse_candidates, function(x) x$avg_vif)
  sorted_names <- names(sort(avg_vifs, decreasing = TRUE))

  to_collapse <- sorted_names[1]
  group <- collapse_candidates[[to_collapse]]

  cat("\nCollapsing:", to_collapse, "\n")
  cat("  Children:", paste(group$children, collapse = ", "), "\n")
  cat("  Avg VIF:", round(group$avg_vif, 2), "\n")
  cat("  Replace with:", group$parent, "\n")

  current_preds <- setdiff(current_preds, group$children)
  current_preds <- c(current_preds, group$parent)

  decisions <- rbind(decisions, data.frame(
    nesting = to_collapse,
    decision = "top",
    children_avg_vif = group$avg_vif,
    stringsAsFactors = FALSE
  ))
}

# Final VIF analysis
cat("\n============================================================\n")
cat("FINAL MODEL VIF\n")
cat("============================================================\n")

final_vif <- get_vif_for_predictors(current_preds, data)
vif_sorted <- sort(final_vif, decreasing = TRUE)

cat("\n--- VIF Values (top 20) ---\n")
for (i in 1:min(20, length(vif_sorted))) {
  flag <- ""
  if (vif_sorted[i] > 10) flag <- " ** HIGH"
  else if (vif_sorted[i] > 5) flag <- " * moderate"
  cat(sprintf("%-20s %10.2f%s\n", names(vif_sorted)[i], vif_sorted[i], flag))
}

cat("\n--- VIF Summary ---\n")
cat("Total predictors:", length(final_vif), "\n")
cat("VIF > 10:", sum(final_vif > 10), "\n")
cat("VIF > 5:", sum(final_vif > 5), "\n")
cat("Max VIF:", round(max(final_vif), 2), "(", names(which.max(final_vif)), ")\n")
cat("Mean VIF:", round(mean(final_vif), 2), "\n")

# Save nesting recommendations
all_nesting_names <- names(nesting_groups)
recommendations <- data.frame(
  nesting = all_nesting_names,
  recommended = "bottom",
  stringsAsFactors = FALSE
)

for (i in 1:nrow(decisions)) {
  idx <- which(recommendations$nesting == decisions$nesting[i])
  if (length(idx) > 0) {
    recommendations$recommended[idx] <- decisions$decision[i]
  }
}

cat("\n--- NESTING RECOMMENDATIONS ---\n")
print(recommendations)

write.csv(recommendations, "artifacts/2_liwc/vif_nesting_recommendation.csv", row.names = FALSE)
cat("\nSaved: artifacts/2_liwc/vif_nesting_recommendation.csv\n")

# Fit final model
cat("\n============================================================\n")
cat("FITTING FINAL MODEL\n")
cat("============================================================\n")

all_predictors <- c(common_control, current_preds)
all_predictors <- intersect(all_predictors, colnames(data))

formula_str <- paste("cbind(POSITIVE, TOTAL-POSITIVE) ~", paste(all_predictors, collapse = " + "))
form <- as.formula(formula_str)

model <- speedglm(form, data = data, family = model_family)

cat("Model fitted with", length(all_predictors), "predictors\n")
cat("Family:", if (USE_QUASI) "quasibinomial" else "binomial", "\n")
cat("Explained deviance:", round(1 - model$deviance/model$nulldev, 4), "\n")

if (USE_QUASI) {
  y <- data$POSITIVE / data$TOTAL
  n <- data$TOTAL
  fitted_probs <- predict(model, newdata = data, type = "response")
  pearson_resid <- (y - fitted_probs) / sqrt(fitted_probs * (1 - fitted_probs) / n)
  dispersion <- sum(pearson_resid^2, na.rm = TRUE) / (nrow(data) - length(coef(model)))
  cat("Dispersion parameter:", round(dispersion, 4), "\n")
} else {
  dispersion <- 1
  cat("AIC:", AIC(model), "\n")
  cat("BIC:", bic(model), "\n")
}

# Save model
result <- list(
  model = model,
  predictors = current_preds,
  vif = final_vif,
  model_type = if (USE_QUASI) "quasibinomial" else "binomial",
  dispersion = dispersion
)

saveRDS(result, "artifacts/2_liwc/vif_optimal_model.rds")
cat("Saved: artifacts/2_liwc/vif_optimal_model.rds\n")

vif_df <- data.frame(predictor = names(vif_sorted), vif = vif_sorted)
write.csv(vif_df, "artifacts/2_liwc/vif_final.csv", row.names = FALSE)
cat("Saved: artifacts/2_liwc/vif_final.csv\n")

# Significant predictors
cat("\n============================================================\n")
cat("SIGNIFICANT PREDICTORS\n")
cat("============================================================\n")

sig_preds <- get_sigpreds(model)

cat("\n--- MORE LIKELY CENSORED (positive) ---\n")
if (nrow(sig_preds$more_likely) > 0) {
  print(head(sig_preds$more_likely, 15))
} else {
  cat("None\n")
}

cat("\n--- LESS LIKELY CENSORED (negative) ---\n")
if (nrow(sig_preds$less_likely) > 0) {
  print(head(sig_preds$less_likely, 15))
} else {
  cat("None\n")
}

write.csv(sig_preds$more_likely, "artifacts/2_liwc/vif_positive_predictors.csv", row.names = FALSE)
write.csv(sig_preds$less_likely, "artifacts/2_liwc/vif_negative_predictors.csv", row.names = FALSE)
write.csv(data.frame(predictor = current_preds), "artifacts/2_liwc/vif_final_predictors.csv", row.names = FALSE)

# Disconnect
disconnectdB()

cat("\n============================================================\n")
cat("DONE\n")
cat("============================================================\n")
