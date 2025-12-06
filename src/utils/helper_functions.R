library(RSQLite)
library(speedglm)
library(rpart)

validdB <- function(conn){
  tryCatch({
    dbIsValid(conn)
  }, error = function(e) {
    FALSE
  })
}

connectdB <- function(cache_size_gb=64, memory_limit_gb = 70,
                      temp_dir = "tmp",
                      dbname="data/counts.db"){
  # Optimized for system with 94GB RAM (~81GB available)
  # cache_size_gb: SQLite page cache in GB (each page is 4096 bytes)
  # memory_limit_gb: SQLite memory limit in GB
  result <<- NULL

  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  Sys.setenv(TMPDIR = temp_dir)
  Sys.setenv(SQLITE_TMPDIR = temp_dir)

  if(!validdB(conn)){
    conn <<- dbConnect(SQLite(), dbname=dbname)

    # Convert GB to number of 4096-byte pages (negative value = KB in SQLite)
    # Using negative value in KB for more precise control
    cache_size_kb <- cache_size_gb * 1024 * 1024  # GB to KB
    query <- paste0("PRAGMA cache_size = -", cache_size_kb)
    dbExecute(conn, query)

    # Memory limit in bytes
    memory_limit_bytes <- memory_limit_gb * 1024^3
    query <- paste0("PRAGMA soft_heap_limit = ", memory_limit_bytes)
    dbExecute(conn, query)

    # Performance optimizations
    dbExecute(conn, "PRAGMA synchronous = OFF")
    dbExecute(conn, "PRAGMA temp_store = MEMORY")
    dbExecute(conn, "PRAGMA journal_mode = WAL")
    dbExecute(conn, "PRAGMA mmap_size = 68719476736")  # 64GB memory-mapped I/O
    dbExecute(conn, "PRAGMA page_size = 4096")
    dbExecute(conn, "PRAGMA threads = 4")  # SQLite threading
  }
}

disconnectdB <- function(){
  if(!is.expired(result)){
    dbClearResult(result)
  }
  if(validdB(conn)){
    dbDisconnect(conn)
  }
}

is.expired <- function(result) {
  
  if(is.null(result)){
    return(TRUE)
  } 
  
  tryCatch({
    dbHasCompleted(result)
    return(FALSE)
  }, error = function(e) {
    return(TRUE)
  })
  
}

make.data <- function(response, 
                      predictors = NULL, 
                      table = 'all_data',
                      where = NULL,
                      limit = NULL, 
                      chunksize = 10**6){
  
  function(reset=FALSE){
    if(!validdB(conn)){
      print('Connection is closed.')
      return(NULL)
    }
    
    if(reset){
      tryCatch({
        dbClearResult(result)
      }, error = function(e) {},
      warning = function(w) {})
      
      select = c(response, predictors)
      select = paste0(select, collapse = ',')
      query = c("SELECT ", select," FROM ", table)
      if (!is.null(where)){
        query <- c(query," WHERE ", where)
      }
      
      if(!is.null(limit)){
        query = c(query, " LIMIT ", limit)
      }
      
      query = paste(query, collapse = '')
      result <<- dbSendQuery(conn, query)
    } else {
      rval = dbFetch(result, n = chunksize)
      if (nrow(rval) == 0){
        rval = NULL
      }
      return(rval)
    }
  }
}

make.data.all <- function(...){
  datafun <- make.data(...)
  datafun(TRUE)
  df <- data.frame()
  
  while(TRUE){
    row <- datafun()
    if(is.null(row)) break
    df <- rbind(df, row)
  }
  
  return(df)
}

make.formula <- function(response, predictors){
  predictors <- paste0(predictors, collapse = "+")
  
  if(length(response) == 2){
    response <- paste0('cbind(',response[1],',',response[2],')')
  }
  
  form_response <- paste0(response, ' ~ ')
  form_response <- paste0(form_response, predictors)
  return(formula(form_response))
}

vif_core <- function(response, predictors, table, limit){
  data <- make.data(response, predictors, table = table, limit = limit)
  
  form <<- make.formula(response, predictors)
  lm1 <- shlm(form, datafun = data)
  
  vifv <- 1/(1-summary(lm1)$r.squared)
  names(vifv) <- response
  vifv
}

format_time <- function(seconds) {
  if (seconds < 60) {
    return(sprintf("%.1f seconds", seconds))
  } else if (seconds < 3600) {
    minutes <- floor(seconds / 60)
    remaining_seconds <- seconds %% 60
    return(sprintf("%d min %d sec", minutes, round(remaining_seconds)))
  } else {
    hours <- floor(seconds / 3600)
    remaining_minutes <- floor((seconds %% 3600) / 60)
    return(sprintf("%d hr %d min", hours, remaining_minutes))
  }
}

vif <- function(predictors, table, limit = NULL){
  
  dir.create('artifacts', showWarnings = FALSE)
  
  checkpoint_file = file.path("artifacts","vif_checkpoint.rds")
  
  if(file.exists(checkpoint_file)){
    checkpoint <- readRDS(checkpoint_file)
    vifs <- checkpoint$vifs
    completed_predictors <- checkpoint$completed_predictors
    start_time <- checkpoint$start_time
    print(completed_predictors)
  } else {
    vifs <- vector("numeric", length(predictors))
    names(vifs) <- predictors
    completed_predictors <- character(0)
    start_time <- Sys.time()
  }
  
  remaining_predictors <- setdiff(predictors, completed_predictors)
  
  for(name in remaining_predictors){
    predictors_clean <- predictors[predictors != name]
    vifs[name] <- vif_core(name, predictors_clean, table = table, limit = limit)
    
    completed_predictors <- c(completed_predictors, name)
    
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    avg_time_per_iteration <- elapsed / length(completed_predictors)
    remaining_iterations <- length(remaining_predictors) - which(remaining_predictors == name)
    estimated_remaining_time <- avg_time_per_iteration * remaining_iterations
    
    cat(sprintf("\rCompleted %d/%d (%.1f%%). Elapsed: %s. Estimated remaining: %s", 
                length(completed_predictors), length(predictors), 
                length(completed_predictors)/length(predictors)*100, 
                format_time(elapsed), format_time(estimated_remaining_time)))
    
    cat(sprintf("\nJust checked: %s, VIF: %.2f", name, vifs[name]))

    saveRDS(list(vifs = vifs, 
                 completed_predictors = completed_predictors, 
                 start_time = start_time), 
            file = checkpoint_file)
  }
  cat("\n")
  
  sort(vifs, decreasing = TRUE)
}

shlm <- function(form, datafun, family = gaussian()){
  datafun(TRUE)
  da2 <- datafun()

  # Store in global env so update() can find them
  .shlm_formula <<- form
  .shlm_data <<- da2
  lm1 <- speedglm(.shlm_formula, data = .shlm_data, family = family)

  # Fetch next chunk
  da2 <- datafun()

  # Process remaining chunks if any
  while(!is.null(da2)){
    .shlm_data <<- da2
    lm1 <- update(lm1, data = .shlm_data, add = TRUE)
    da2 <- datafun()
  }

  return(lm1)
}

ranrows <- function(predictors, response=NULL, table='presence', limit=1000){
  preds <- c(predictors, response)
  preds <- paste0(preds,collapse = ', ')
  
  dbGetQuery(conn, paste0("SELECT ", preds," FROM ",table," ORDER BY RANDOM() LIMIT ", limit))
}

predict.shglm.all <- function(model, data, type='response'){
  all_preds_list <- list()
  index <- 1
  
  data(TRUE)
  da2 <- data(FALSE)
  while(!is.null(da2)) {
    preds <- predict(model, newdata = da2, type=type)
    all_preds_list[[index]] <- preds
    index <- index + 1
    da2 <- data(FALSE)
  }

  all_preds <- do.call(c, all_preds_list)
  attr(all_preds,'names') <- NULL
  
  return(all_preds)
}

delete.columns <- function(columns_to_delete, table){
  all_columns <- dbListFields(conn, table)
  columns_to_keep <- setdiff(all_columns, columns_to_delete)
  
  if (len(setdiff(all_columns,columns_to_keep)) == 0 ){
    print("Columns not in table!")
    return()
  }

  create_new_table_sql <- paste0("CREATE TABLE new_table AS SELECT ",
  paste(columns_to_keep, collapse = ", "),
  " FROM ", table)

  dbExecute(conn, create_new_table_sql)
  dbExecute(conn, paste0("DROP TABLE ", table))
  dbExecute(con, paste0("ALTER TABLE new_table RENAME TO ", table))
}


bin.residuals <- function(predicted, actual, nbins = NULL, type = 'quantile', return_bin_indices = FALSE) {
  if (is.null(nbins)) {
    nbins <- floor(sqrt(length(actual)))
  }
  
  residual <- predicted - actual
  
  if (type == 'quantile') {
    probs <- seq(0, 1, length.out = nbins + 1)
    breaks <- unique(quantile(predicted, probs, names = FALSE))
    if (length(breaks) <= 2) {
      warning("Not enough unique quantile breaks; switching to equal-width bins.")
      breaks <- seq(min(predicted), max(predicted), length.out = nbins + 1)
    }
  } else {
    breaks <- seq(min(predicted), max(predicted), length.out = nbins + 1)
  }
  
  bin_indices <- cut(predicted, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  data <- data.frame(
    residual = residual,
    predicted = predicted,
    bin = bin_indices
  )
  
  if (return_bin_indices) {
    return(data.frame(bin = bin_indices))
  } else {
    res_mean <- aggregate(residual ~ bin, data, mean)$residual
    pred_mean <- aggregate(predicted ~ bin, data, mean)$predicted
    length_bin <- aggregate(residual ~ bin, data, length)$residual
    res_sd <- aggregate(residual ~ bin, data, sd)$residual
    
    res_sd[is.na(res_sd)] <- 0
    
    res_se <- res_sd / sqrt(length_bin)
    ellipse <- 1.96 * res_se
    
    df <- data.frame(
      residuals = res_mean,
      predicted = pred_mean,
      count = length_bin,
      ellipse = ellipse
    )
    
    return(df)
  }
}

aggregate.predictors.binomial <- function(response, predictors, name, table = 'presence'){
  dbExecute(conn, paste0("DROP TABLE IF EXISTS ", name))
  predictors <- paste0(predictors, collapse = ", ")
  query <- paste0("
    CREATE TABLE ", name, " AS 
    SELECT ", predictors, ", 
           COUNT(*) as TOTAL,
           SUM(",response,") as POSITIVE
    FROM ",table,"
    GROUP BY ", predictors)
  
  dbExecute(conn, query)
  
  cat('Table', name, 'made.\n')
}

aggregate.predictors.duplicates <- function(predictors, name, table = 'presence'){
  dbExecute(conn, paste0("DROP TABLE IF EXISTS ", name))
  predictors <- paste0(predictors, collapse = ", ")
  query <- paste0("
    CREATE TABLE ", name, " AS 
    SELECT ", predictors, ", 
           COUNT(*) as TOTAL
    FROM ",table,"
    GROUP BY ", predictors)
  
  dbExecute(conn, query)
  
  cat('Table', name, 'made.\n')
}

bic <- function(model){
  if (is.null(model$coefficients)) {
    return(Inf)
  }
  
  k <- length(model$coefficients)
  n <- model$n
  tll <- 2 * as.numeric(logLik(model))
  bic <- log(n) * k - tll
  bic
}

aic <- function(model) {
  if (is.null(model$coefficients)) {
    return(Inf)
  }
  
  k <- length(model$coefficients)
  tll <- 2 * as.numeric(logLik(model))
  aic <- 2 * k - tll
  aic
}

get_sigpreds <- function(model) {
  coefs <- summary(model)$coefficients
  terms <- rownames(coefs)

  sig_terms_idx <- which(coefs[, 4] <= 0.05)
  
  # If no significant terms, return empty data frames
  if (length(sig_terms_idx) == 0) {
    empty_df <- data.frame(term = character(0), estimate = numeric(0), p_value = numeric(0))
    return(list(more_likely = empty_df, less_likely = empty_df))
  }
  
  sig_terms <- terms[sig_terms_idx]
  sig_coefs <- coefs[sig_terms_idx, , drop = FALSE]

  split_terms <- strsplit(sig_terms, ":")
  term_sets <- lapply(split_terms, sort)

  df_terms <- data.frame(
    term = sig_terms,
    estimate = sig_coefs[, 1],
    p_value = sig_coefs[, 4],
    varlist = I(term_sets),
    order = lengths(term_sets),
    stringsAsFactors = FALSE
  )

  df_terms <- df_terms[order(-df_terms$order), ]

  # Handle filtering of redundant terms only if we have more than one term
  keep <- rep(TRUE, nrow(df_terms))
  if (nrow(df_terms) > 1) {
    for (i in seq_len(nrow(df_terms))) {
      if (!keep[i]) next
      current_vars <- df_terms$varlist[[i]]
      # Only execute the inner loop if there are rows after i
      if (i < nrow(df_terms)) {
        for (j in (i+1):nrow(df_terms)) {
          if (keep[j]) {
            if (all(df_terms$varlist[[j]] %in% current_vars)) {
              keep[j] <- FALSE
            }
          }
        }
      }
    }
  }

  terms <- c("term", "estimate", "p_value")
  final_df <- df_terms[keep, terms]
  
  # Create positive and negative dataframes
  positive <- final_df[final_df$estimate > 0, ]
  if (nrow(positive) > 0) {
    positive <- positive[order(positive$estimate, decreasing = TRUE), ]
  }
  
  negative <- final_df[final_df$estimate < 0, ]
  if (nrow(negative) > 0) {
    negative <- negative[order(negative$estimate, decreasing = FALSE), ]
  }
  
  list(more_likely = positive, less_likely = negative)
}


## THIS CORRECTS THE LOGLIKLIHOOD TO ACCOUNT FOR AGGREGATING

loglike_corrected <- function (y, n, mu, wt, dev) 
{
  n <- wt
  m <- if (any(n > 1)) n else wt
  correction <- ifelse(round(m) == 1, 0, lchoose(round(m), round(m * y)))
  -2 * sum(ifelse(m > 0, (wt/m), 0) * dbinom(round(m * y), 
                                             round(m), mu, log = TRUE) - correction)
}

binomial_corrected = binomial()
binomial_corrected$aic = loglike_corrected

# Quasibinomial with corrected AIC (for overdispersed data)
quasibinomial_corrected = quasibinomial()
quasibinomial_corrected$aic = loglike_corrected

dev.residual_corrected = function (y, mu, wt){
  wt <- wt[1:2]
  binomial()$dev.resids(y, mu, wt)
}

QAIC <- function(model){
  ll <- logLik(model)
  df <- attr(ll,'df')
  disp <- sum(resid(model,'pearson')**2)/model$df.residual
  -2 * c(ll) / disp + 2 * df
}

AIC <- function(model){
  ll <- logLik(model)
  df <- attr(ll,'df')
  -2 * c(ll) + 2 * df
}

disp <- function(model, residual){
  sum(residual**2)/model$df.residual
}

pearson_resid <- function(actualCens, predictedProp, N){
  (actualCens - N*predictedProp)/sqrt(N*predictedProp*(1-predictedProp))
}

deviance_resid <- function(actualProp,predictedProp,N){
  r1 = sqrt(binomial()$dev.resids(actualProp,predictedProp,N))
  ifelse(actualProp > predictedProp, r1, -r1)
}

p2logit <- function(top,bottom){
  log(top) - log(bottom) - log(bottom - top) + log(bottom)
}

logLik.mock_glm <- function(object, ...) {
  structure(-Inf, 
            df = length(object$coefficients), 
            class = "logLik")
}

create_mock_model <- function(predictors, form, link) {
  mock_model <- list(
    coefficients = structure(rep(NA_real_, length(predictors) + 1), 
                             names = c("(Intercept)", predictors)),
    df.residual = Inf,
    df.null = Inf,
    null.deviance = Inf,
    deviance = Inf,
    aic = Inf,
    family = binomial(link = link),
    formula = form,
    call = call("glm", formula = form)
  )
  class(mock_model) <- c("mock_glm","glm", "lm")
  
  mock_loglik <- structure(-Inf, 
                           df = length(mock_model$coefficients), 
                           nobs = Inf,
                           class = "logLik")
  
  mock_model$loglik <- mock_loglik
  
  return(mock_model)
}

##################################

time <- function(){
  print(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

#################################

### REWRITE summary.speedglm to allow arbitrary dispersion

summary.speedglm <- function (object, correlation = FALSE, dispersion = NULL, ...) 
{
  if (!inherits(object, "speedglm")) 
    stop("object is not of class speedglm")
  z <- object
  var_res <- as.numeric(z$RSS/z$df)
  if(is.null(dispersion)){
    dispersion <- if (z$family$family %in% c("poisson", "binomial")) 
      1 
    else var_res
  }
  if (z$method == "qr") {
    z$XTX <- z$XTX[z$ok, z$ok]
  }
  inv <- solve(z$XTX, tol = z$tol.solve)
  covmat <- diag(inv)
  se_coef <- rep(NA, length(z$coefficients))
  se_coef[z$ok] <- sqrt(dispersion * covmat)
  if (z$family$family %in% c("binomial", "poisson")) {
    z1 <- z$coefficients/se_coef
    p <- 2 * pnorm(abs(z1), lower.tail = FALSE)
  }
  else {
    t1 <- z$coefficients/se_coef
    p <- 2 * pt(abs(t1), df = z$df, lower.tail = FALSE)
  }
  dn <- c("Estimate", "Std. Error")
  if (z$family$family %in% c("binomial", "poisson")) {
    param <- data.frame(z$coefficients, se_coef, z1, p)
    dimnames(param) <- list(names(z$coefficients), c(dn, 
                                                     "z value", "Pr(>|z|)"))
  }
  else {
    param <- data.frame(z$coefficients, se_coef, t1, p)
    dimnames(param) <- list(names(z$coefficients), c(dn, 
                                                     "t value", "Pr(>|t|)"))
  }
  eps <- 10 * .Machine$double.eps
  if (z$family$family == "binomial") {
    if (any(z$mu > 1 - eps) || any(z$mu < eps)) 
      warning("fitted probabilities numerically 0 or 1 occurred")
  }
  if (z$family$family == "poisson") {
    if (any(z$mu < eps)) 
      warning("fitted rates numerically 0 occurred")
  }
  keep <- match(c("call", "terms", "family", "deviance", "aic", 
                  "df", "nulldev", "nulldf", "iter", "tol", "n", "convergence", 
                  "ngoodobs", "logLik", "RSS", "rank"), names(object), 
                0)
  ans <- c(object[keep], list(coefficients = param, dispersion = dispersion, 
                              correlation = correlation, cov.unscaled = inv, cov.scaled = inv * 
                                var_res))
  if (correlation) {
    ans$correl <- (inv * var_res)/outer(na.omit(se_coef), 
                                        na.omit(se_coef))
  }
  class(ans) <- "summary.speedglm"
  return(ans)
}

#######################################################
# rpart
#######################################################

prune.serule <- function(model){
  cp_table <- model$cptable
  min_xerror_idx <- which.min(cp_table[, "xerror"])
  min_xerror <- cp_table[min_xerror_idx, "xerror"]
  se_xerror <- cp_table[min_xerror_idx, "xstd"]
  
  optimal_idx <- max(which(cp_table[, "xerror"] <= min_xerror + se_xerror))
  optimal_cp <- cp_table[optimal_idx, "CP"]
  
  prune(model, cp = optimal_cp)
}

prune.cp <- function(model){
  cp_table <- model$cptable
  min_xerror_idx <- which.min(cp_table[, "xerror"])
  optimal_cp <- cp_table[min_xerror_idx, "CP"]
  
  prune(model, cp = optimal_cp)
}

rpart.getimp <- function(tree) {
  v <- tree$frame$var
  v = unique(v[v != '<leaf>'])
  vi = tree$variable.importance
  vi = vi[attr(vi,'names') %in% v]
  cumsum(vi/sum(vi))
}

rpart.get_leaf_variables <- function(tree, interaction="*", avges=FALSE) {
  leaves <- as.numeric(row.names(tree$frame[tree$frame$var == "<leaf>", ]))
  paths <- path.rpart(tree, leaves, print.it = FALSE)

  result_df <- data.frame(
    leaf_id = leaves,
    path_vars = character(length(leaves)),
    count = numeric(length(leaves)),
    prob = numeric(length(leaves)),
    prediction = numeric(length(leaves)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(paths)) {
    path <- paths[[i]]
    vars <- c()
    
    for (label in path[-1]) {
      var <- strsplit(strsplit(label, "<")[[1]][1], ">")[[1]][1]
      vars <- c(vars, var)
    }

    result_df$path_vars[i] <- paste0(unique(vars), collapse = interaction)
    leaf_index <- which(as.numeric(row.names(tree$frame)) == leaves[i])
    result_df$count[i] <- tree$frame$n[leaf_index]
    
    if (!is.null(tree$frame$yval2)) {
      pred_class <- which.max(tree$frame$yval2[leaf_index, -1])
      result_df$prob[i] <- tree$frame$yval2[leaf_index, pred_class + 1]
      result_df$prediction[i] <- tree$frame$yval[leaf_index]
    } else {
      result_df$prediction[i] <- tree$frame$yval[leaf_index]
      result_df$prob[i] <- NA
    }
  }

  path_ids <- unique(result_df$path_vars)

  if (avges) {
    final_result <- data.frame(
      path_vars = character(length(path_ids)),
      total_count = numeric(length(path_ids)),
      avg_prob = numeric(length(path_ids)),
      weighted_prediction = numeric(length(path_ids)),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(path_ids)) {
      path_rows <- result_df$path_vars == path_ids[i]
      final_result$path_vars[i] <- path_ids[i]
      final_result$total_count[i] <- sum(result_df$count[path_rows])
      
      if (any(!is.na(result_df$prob[path_rows]))) {
        final_result$avg_prob[i] <- weighted.mean(result_df$prob[path_rows], 
                                                  result_df$count[path_rows])
      } else {
        final_result$avg_prob[i] <- NA
      }
      
      final_result$weighted_prediction[i] <- weighted.mean(result_df$prediction[path_rows], 
                                                           result_df$count[path_rows])
    }
  } else {
    final_result <- data.frame(
      path_vars = character(length(path_ids)),
      total_count = numeric(length(path_ids)),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(path_ids)) {
      path_rows <- result_df$path_vars == path_ids[i]
      final_result$path_vars[i] <- path_ids[i]
      final_result$total_count[i] <- sum(result_df$count[path_rows])
    }
  }
  
  terms_vars <- strsplit(final_result$path_vars, "\\*")
  keep <- rep(TRUE, length(final_result$path_vars))
  
  for (i in seq_along(terms_vars)) {
    vars_i <- terms_vars[[i]]
    for (j in seq_along(terms_vars)) {
      if (i != j) {
        vars_j <- terms_vars[[j]]
        if (all(vars_i %in% vars_j)) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  
  final_result <- final_result[keep, ]
  return(final_result)
}

rf.get_leaf_variables <- function(tree, varnames, interaction = "*", avges = FALSE) {
  tree <- as.data.frame(tree)
  max_node <- nrow(tree)
  
  # Collect all paths to terminal nodes
  paths <- list()
  predictions <- c()
  counts <- c()
  
  traverse <- function(node_id, path_vars) {
    if (node_id > max_node) return(NULL)
    node <- tree[node_id, , drop = FALSE]
    
    if (node[["split var"]] == 0) {
      paths[[length(paths) + 1]] <<- path_vars
      predictions <<- c(predictions, node[["prediction"]])
      counts <<- c(counts, 1)
      return()
    }
    
    split_var_name <- varnames[node[["split var"]]]
    left <- node[["left daughter"]]
    right <- node[["right daughter"]]
    
    traverse(left, c(path_vars, split_var_name))
    traverse(right, c(path_vars, split_var_name))
  }
  
  traverse(1, character(0))
  
  df <- data.frame(
    path_vars = sapply(paths, function(p) paste(unique(p), collapse = interaction)),
    prediction = predictions,
    count = counts,
    stringsAsFactors = FALSE
  )
  
  # Aggregate results
  if (avges) {
    agg <- aggregate(cbind(count, prediction) ~ path_vars, data = df, FUN = function(x) {
      c(sum = sum(x), wmean = weighted.mean(x, df$count[df$path_vars == df$path_vars[match(x, df$prediction)]]))
    })
    
    out <- data.frame(
      path_vars = agg$path_vars,
      total_count = sapply(agg$prediction, function(x) x[1]),
      weighted_prediction = sapply(agg$prediction, function(x) x[2])
    )
  } else {
    out <- aggregate(count ~ path_vars, data = df, sum)
    colnames(out) <- c("path_vars", "total_count")
  }
  
  # Prune redundant paths (subset removal)
  # Split and sort by path length
  split_paths <- strsplit(out$path_vars, paste0("\\", interaction))
  path_lengths <- sapply(split_paths, length)
  order_idx <- order(path_lengths)  # shortest first
  
  n_paths <- length(split_paths)
  keep <- rep(TRUE, n_paths)
  
  for (ii in seq_len(n_paths)) {
    i_idx <- order_idx[ii]
    if (!isTRUE(keep[i_idx])) next
    
    vars_i <- split_paths[[i_idx]]
    for (jj in (ii + 1):n_paths) {
      j_idx <- order_idx[jj]
      if (!isTRUE(keep[j_idx])) next
      
      vars_j <- split_paths[[j_idx]]
      
      # prune i if it's a strict subset of j
      if (length(vars_i) < length(vars_j) && all(vars_i %in% vars_j)) {
        keep[i_idx] <- FALSE
        break
      }
    }
  }
  
  out <- out[keep, , drop = FALSE]
  
  return(out)
}


########################

# library(rpart)

# rpart.sql.extract_vars_depth <- function(models_list, prune=NULL) {
#   pruned_variables_list <- vector("list", length(models_list))
  
#   for (i in seq_along(models_list)) {
#     model <- models_list[[i]]
#     if (is.null(prune)){
#       pruned_model <- model
#     } else if (prune == 'cp'){
#       pruned_model <- prune.cp(model)
#     } else if (prune == 'serule'){
#       pruned_model <- prune.serule(model)
#     }
    
#     frame <- pruned_model$frame
#     vars_in_tree <- frame$var
#     node_numbers <- as.numeric(row.names(frame))
    
#     splitting_nodes <- vars_in_tree != "<leaf>"
#     vars <- vars_in_tree[splitting_nodes]
#     nodes <- node_numbers[splitting_nodes]
    
#     depth <- floor(log2(nodes))
#     vars_and_depths <- data.frame(variable=vars, depth=depth)
#     min_depths <- aggregate(depth ~ variable, data=vars_and_depths, FUN=min)
    
#     pruned_variables_list[[i]] <- min_depths
#   }
  
#   return(pruned_variables_list)
# }

# rpart.sql <- function(formula, datafun, ...) {
#   datafun(TRUE)
#   models_list <- list()
#   i <- 0
  
#   dots <- list(...)
  
#   while(TRUE) {
#     i <- i + 1
#     print(i)
#     row <- datafun()
#     if(is.null(row)) break
#     models_list[[i]] <- do.call(rpart, 
#                                 c(list(formula = formula, 
#                                        data = row, 
#                                        model = FALSE, 
#                                        x = FALSE, 
#                                        y = FALSE),
#                                   dots))
#   }
  
#   return(models_list)
# }

# rpart.sql.predict <- function(models, newdata, type = "vector") {
#   predictions <- sapply(models, function(model) {
#     predict(model, newdata = newdata, type = type)
#   })
  
#   target_var <- all.vars(models[[1]]$terms)[1]
  
#   if (is.factor(newdata[[target_var]]) || is.character(newdata[[target_var]])) {
#     avg_probs <- rowMeans(predictions)
#     predicted_class <- ifelse(avg_probs > 0.5, levels(newdata[[target_var]])[2], levels(newdata[[target_var]])[1])
#     return(factor(predicted_class, levels = levels(newdata[[target_var]])))
#   } else {
#     avg_prediction <- rowMeans(predictions)
#     return(as.vector(avg_prediction))
#   }
# }

# rpart.sql.predict.all <- function(models, datafun, type = "vector"){
#   datafun(TRUE)
#   v <- c()
  
#   i <- 0
#   while(TRUE){
#     i <- i + 1
#     print(i)
#     row <- datafun()
#     if(is.null(row)) break
#     pred <- rpart.sql.predict(models,row,type=type)
#     v <- c(v, pred)
#   }
#   return(v)
# }

# rpart.sql.variable_importance <- function(models, data, target_var, metric = "accuracy") {
#   original_preds <- predict_ensemble(models, data)
  
#   target_var <- all.vars(models[[1]]$terms)[1]
  
#   if (is.factor(data[[target_var]])) {
#     original_perf <- mean(original_preds == data[[target_var]])
#   } else {
#     original_perf <- -mean((original_preds - data[[target_var]])^2)
#   }
  
#   importance <- c()
#   predictors <- setdiff(names(data), target_var)
#   for (var in predictors) {
#     permuted_data <- data
#     permuted_data[[var]] <- sample(permuted_data[[var]])
#     permuted_preds <- predict_ensemble(models, permuted_data)
    
    
#     if (is.factor(data[[target_var]])) {
#       permuted_perf <- mean(permuted_preds == data[[target_var]])
#     } else {
#       permuted_perf <- -mean((permuted_preds - data[[target_var]])^2)
#     }
    
#     importance[var] <- original_perf - permuted_perf
#   }
  
#   importance <- sort(importance, decreasing = TRUE)
#   return(importance)
# }

# build_query <- function(preds, path = character()) {
#   # Build the subset CTE
#   query1 <- paste0(
#     "\nWITH\n",
#     "subset AS (SELECT ", paste0(preds, collapse = ', '), ", POSITIVE, TOTAL FROM ", table)
# 
#   # Add WHERE clause if path conditions exist
#   if (length(path) > 0) {
#     query1 <- paste0(query1, " WHERE ", paste0(path, collapse = ' AND '))
#   }
# 
#   query1 <- paste0(query1, "),\n\n")
# 
#   # Build individual CTEs for each predictor
#   query2 <- paste0(
#     preds, " AS (SELECT '", preds, "' AS name, ",
#     preds, " as child, SUM(POSITIVE) AS count, SUM(TOTAL) as total FROM subset GROUP BY ",
#     preds, ")",
#     collapse = ',\n'
#   )
# 
#   # Combine all parts and add UNION ALL
#   query <- paste0(query1, query2, '\n\n')
#   final_query <- paste0(query, paste0("SELECT * FROM ", preds, collapse = "\nUNION ALL\n"))
# 
#   return(final_query)
# }
# 
# calculate_deviance <- function(counts, totals) {
#   overall_p <- sum(counts) / sum(totals)
# 
#   expected <- totals * overall_p
# 
#   observed_pos <- counts
#   observed_neg <- totals - counts
#   expected_pos <- expected
#   expected_neg <- totals - expected
# 
#   dev_pos <- 2 * observed_pos * log(ifelse(observed_pos == 0, 1, observed_pos/expected_pos))
#   dev_neg <- 2 * observed_neg * log(ifelse(observed_neg == 0, 1, observed_neg/expected_neg))
# 
#   sum(dev_pos + dev_neg)
# }
# 
# calculate_predictor_deviance <- function(df) {
#   result <- do.call(rbind, by(df, df$name, function(pred_df) {
#     dev <- calculate_deviance(counts=pred_df$count, total=pred_df$total)
#     data.frame(
#       pred = pred_df$name[1],
#       deviance = dev,
#       stringsAsFactors = FALSE
#     )
#   }))
# 
#   result$rank <- rank(-result$deviance)
#   result <- result[order(-result$deviance), ]
#   rownames(result) <- NULL
#   return(result)
# }
##############