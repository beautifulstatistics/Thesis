library(RSQLite)
library(speedglm)

validdB <- function(conn){
  tryCatch({
    dbIsValid(conn)
  }, error = function(e) {
    FALSE
  })
}

connectdB <- function(cache_size=64, memory_limit = 64, 
                      temp_dir = "/home/kenneywl/Desktop/Thesis/tmp", 
                      dbname="/home/kenneywl/Desktop/Thesis/data/counts.db"){
  result <<- NULL
  
  Sys.setenv(TMPDIR = temp_dir)
  Sys.setenv(SQLITE_TMPDIR = temp_dir)
  
  if(!validdB(conn)){
    conn <<- dbConnect(SQLite(), dbname=dbname)
    
    cache_size <- cache_size * (1024^3) / 4096
    query <- paste0("PRAGMA cache_size = ", cache_size)
    dbExecute(conn, query)
    
    memory_limit <- memory_limit * 1024^3
    query <- paste0("PRAGMA memory_limit = ", memory_limit)
    dbExecute(conn, query)
    
    dbExecute(conn, "PRAGMA synchronous = OFF")
    dbExecute(conn, "PRAGMA temp_store = MEMORY") 
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
  
  dir.create('evaluation', showWarnings = FALSE)
  
  checkpoint_file = file.path("evaluation","vif_checkpoint.rds")
  
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

shlm <- function(form, datafun){
  datafun(TRUE)
  da2 = datafun()
  
  lm1 <- speedlm(form, data = da2)
  
  while(!is.null(da2)){
    lm1 <- update(lm1, data = da2, add = TRUE)
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

os_walk <- function(path) {
  walk_recursive <- function(dir) {
    files <- list.files(dir, full.names = TRUE, recursive = FALSE)
    dirs <- files[file.info(files)$isdir]
    
    file_paths <- files[!files %in% dirs]
    file_paths
  }
  
  walk_recursive(path)
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

## THIS CORRECTS THE LOGLIKLIHOOD TO ACCOUNT FOR AGGREGATING

loglike_corrected <- function (y, n, mu, wt, dev) 
{
  n <- wt
  m <- if (any(n > 1)) n else wt
  correction <- ifelse(round(m) == 1, 0, lchoose(round(m), round(m * y)))
  -2 * sum(ifelse(m > 0, (wt/m), 0) * dbinom(round(m * y), 
                                             round(m), mu, log = TRUE) - correction)
}

dev.residual_corrected = function (y, mu, wt){
  wt <- wt[1:2]
  binomial()$dev.resids(y, mu, wt)
}

binomial_corrected = binomial()
binomial_corrected$aic = loglike_corrected

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

### MODELS

models = list(
  highest = c('functions', 'othergram', 'social', 'percept', 'persconc',
              'drives', 'affect', 'cogproc', 'bio', 'relativ', 'informal'),
  
  functions = c('pronoun','prep','auxverb','adverb','conj','negate',
                'quanunit','prepend','specart','tensem','particle'),
  functions_pronoun = c('ppron','ipron'),
  functions_pronoun_ppron = c('i','we','you','shehe','they','youpl'),
  functions_tensem = c('focuspast','focusfuture','progm'),
  functions_particle = c('modal_pa','general_pa'),
  
  othergram = c('compare','interrog','number','quant'),
  social = c('family','friend','female','male'),
  percept = c('see','hear','feel'),
  drives = c('affiliation','achieve','power','reward','risk'),
  persconc = c('work','leisure','home','money','relig','death'),
  
  affect = c('posemo','negemo'),
  affect_negemo = c('anx','anger','sad'),
  
  cogproc = c('insight','cause','discrep','tentat','certain','differ'),
  bio = c('body','health','sexual','ingest'),
  relative = c('motion','space','time'),
  informal = c('swear','netspeak','assent','nonflu','filler'),
  
  collective_action = c('ppron','focuspresent','focusfuture','drives','motion','space','time'),
  
  lowest = c('i','we','you','shehe','they','youpl','ipron','prep','auxverb',
    'adverb','conj','negate','quanunit','prepend','specart','focuspast',
    'focuspresent','focusfuture','progm','modal_pa','general_pa','compare',
    'interrog','number','quant','posemo','anx','anger','sad','family',
    'friend','female','male','insight','cause','discrep','tentat','certain',
    'differ','see','hear','feel','body','health','sexual','ingest','affiliation',
    'achieve','power','reward','risk','motion','space','time','work','leisure',
    'home','money','relig','death','swear','netspeak','assent','nonflu','filler')
)

for(name in names(models)){
  models[[name]] <- c('tokencount','image', models[[name]])
}

models[['global']] <- unique(unlist(models))

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

# library(rpart)
# 
# rpart.sql <- function(formula, datafun, ...) {
#   datafun(TRUE)
#   models_list <- list()
#   i <- 0
#   
#   dots <- list(...)
#   
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
#   
#   return(models_list)
# }
# 
# rpart.sql.predict <- function(models, newdata, type = "vector") {
#   predictions <- sapply(models, function(model) {
#     predict(model, newdata = newdata, type = type)
#   })
#   
#   target_var <- all.vars(models[[1]]$terms)[1]
#   
#   if (is.factor(newdata[[target_var]]) || is.character(newdata[[target_var]])) {
#     avg_probs <- rowMeans(predictions)
#     predicted_class <- ifelse(avg_probs > 0.5, levels(newdata[[target_var]])[2], levels(newdata[[target_var]])[1])
#     return(factor(predicted_class, levels = levels(newdata[[target_var]])))
#   } else {
#     avg_prediction <- rowMeans(predictions)
#     return(as.vector(avg_prediction))
#   }
# }
# 
# rpart.sql.predict.all <- function(models, datafun, type = "vector"){
#   datafun(TRUE)
#   v <- c()
#   
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
# 
# rpart.sql.variable_importance <- function(models, data, target_var, metric = "accuracy") {
#   original_preds <- predict_ensemble(models, data)
#   
#   target_var <- all.vars(models[[1]]$terms)[1]
#   
#   if (is.factor(data[[target_var]])) {
#     original_perf <- mean(original_preds == data[[target_var]])
#   } else {
#     original_perf <- -mean((original_preds - data[[target_var]])^2)
#   }
#   
#   importance <- c()
#   predictors <- setdiff(names(data), target_var)
#   for (var in predictors) {
#     permuted_data <- data
#     permuted_data[[var]] <- sample(permuted_data[[var]])
#     permuted_preds <- predict_ensemble(models, permuted_data)
#     
#     
#     if (is.factor(data[[target_var]])) {
#       permuted_perf <- mean(permuted_preds == data[[target_var]])
#     } else {
#       permuted_perf <- -mean((permuted_preds - data[[target_var]])^2)
#     }
#     
#     importance[var] <- original_perf - permuted_perf
#   }
#   
#   importance <- sort(importance, decreasing = TRUE)
#   return(importance)
# }
# 
# prune.serule <- function(model){
#   cp_table <- model$cptable
#   min_xerror_idx <- which.min(cp_table[, "xerror"])
#   min_xerror <- cp_table[min_xerror_idx, "xerror"]
#   se_xerror <- cp_table[min_xerror_idx, "xstd"]
#   
#   optimal_idx <- max(which(cp_table[, "xerror"] <= min_xerror + se_xerror))
#   optimal_cp <- cp_table[optimal_idx, "CP"]
#   
#   prune(model, cp = optimal_cp)
# }
# 
# prune.cp <- function(model){
#   cp_table <- model$cptable
#   min_xerror_idx <- which.min(cp_table[, "xerror"])
#   optimal_cp <- cp_table[min_xerror_idx, "CP"]
#   
#   prune(model, cp = optimal_cp)
# }
# 
# rpart.sql.extract_vars_depth <- function(models_list, prune=NULL) {
#   pruned_variables_list <- vector("list", length(models_list))
#   
#   for (i in seq_along(models_list)) {
#     model <- models_list[[i]]
#     if (is.null(prune)){
#       pruned_model <- model
#     } else if (prune == 'cp'){
#       pruned_model <- prune.cp(model)
#     } else if (prune == 'serule'){
#       pruned_model <- prune.serule(model)
#     }
#     
#     frame <- pruned_model$frame
#     vars_in_tree <- frame$var
#     node_numbers <- as.numeric(row.names(frame))
#     
#     splitting_nodes <- vars_in_tree != "<leaf>"
#     vars <- vars_in_tree[splitting_nodes]
#     nodes <- node_numbers[splitting_nodes]
#     
#     depth <- floor(log2(nodes))
#     vars_and_depths <- data.frame(variable=vars, depth=depth)
#     min_depths <- aggregate(depth ~ variable, data=vars_and_depths, FUN=min)
#     
#     pruned_variables_list[[i]] <- min_depths
#   }
#   
#   return(pruned_variables_list)
# }
# 
# rpart.getimp <- function(tree_model) {
#   v <- tree_model$frame$var
#   v = unique(v[v != '<leaf>'])
#   vi = tree_model$variable.importance
#   vi = vi[attr(vi,'names') %in% v]
#   cumsum(vi/sum(vi))
# }
# 
# rpart.get_leaf_variables <- function(tree) {
#   leaves <- as.numeric(row.names(tree$frame[tree$frame$var == "<leaf>", ]))
#   
#   paths <- path.rpart(tree, leaves, print.it = FALSE)
#   leaf_vars <- c()
#   
#   for (i in seq_along(paths)) {
#     path <- paths[[i]]
#     vars <- c()
#     
#     for (label in path[-1]) {
#       var <- strsplit(strsplit(label, "<")[[1]][1], ">")[[1]][1]
#       vars <- c(vars, var)
#     }
#     
#     leaf_vars <- c(leaf_vars, paste0(unique(vars), collapse = "*"))
#   }
#   
#   leaf_vars <- unique(leaf_vars)
#   
#   terms_vars <- strsplit(leaf_vars, "\\*")
#   keep <- rep(TRUE, length(leaf_vars))
#   
#   for (i in seq_along(terms_vars)) {
#     vars_i <- terms_vars[[i]]
#     for (j in seq_along(terms_vars)) {
#       if (i != j) {
#         vars_j <- terms_vars[[j]]
#         if (all(vars_i %in% vars_j)) {
#           keep[i] <- FALSE
#           break
#         }
#       }
#     }
#   }
#   
#   leaf_vars <- leaf_vars[keep]
#   
#   return(leaf_vars)
# }

########################
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