library(RSQLite)
library(speedglm)

validdB <- function(conn){
  tryCatch({
    dbIsValid(conn)
  }, error = function(e) {
    FALSE
  })
}

connectbB <- function(){
  result <<- NULL
  
  if(!validdB(conn)){
    conn <<- dbConnect(SQLite(), dbname="data/counts.db")
  
    cache_size <- 44 * (1024^3) / 4096
    query <- paste0("PRAGMA cache_size = ", cache_size)
    dbExecute(conn, query)
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

make.data <- function(response, predictors = NULL, table = 'all_data', limit = NULL, chunksize = 10**6){
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
  
  vifv <- 1/summary(lm1)$r.squared
  names(vifv) <- response
  vifv
}

vif <- function(predictors, table, limit = NULL){
  vifs <- NULL
  for(name in predictors){
    predictors_clean = predictors[predictors != name]
    value <- vif_core(name, predictors_clean, table = table, limit = limit)
    vifs <- c(vifs, value)
    cat(name)
    cat(',')
  }
  cat('\n')
  
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

all_linear_predict.shglm <- function(model, data){
  all_preds_list <- list()
  index <- 1
  
  data(TRUE)
  da2 <- data(FALSE)
  while(!is.null(da2)) {
    preds <- linear_predict.shglm(model, da2)
    all_preds_list[[index]] <- preds
    index <- index + 1
    da2 <- data(FALSE)
  }

  all_preds <- do.call(c, all_preds_list)
  return(all_preds)
}

linear_predict.shglm <- function(model, newdata){
  form <- formula(model$tf)
  if(length(form) == 0){
    form <- formula(model)
  }
  rowSums(model.matrix(form, newdata) %*% coef(model))
}

all_predict.shglm <- function(model, data){
  all_preds_list <- list()
  index <- 1
  
  data(TRUE)
  da2 <- data(FALSE)
  while(!is.null(da2)) {
    preds <- predict.shglm(model, da2)
    all_preds_list[[index]] <- preds
    index <- index + 1
    da2 <- data(FALSE)
  }

  all_preds <- do.call(c, all_preds_list)
  attr(all_preds,'names') <- NULL
  
  return(all_preds)
}

get_all_data <- function(data){
  dat = data.frame()
  
  data(TRUE)
  da2 <- data(FALSE)
  while(!is.null(da2)) {
    dat <- rbind(dat,da2)
    da2 <- data(FALSE)
  }
  dat
}

predict.shglm <- function(model, newdata){
  linear_predict <- linear_predict.shglm(model, newdata)
  1 / (1 + exp(-linear_predict))
}

bin.residuals <- function(predicted,actual,nbins=NULL){
  if(is.null(nbins)){
    nbins <- floor(sqrt(length(actual)))
  }
  
  residual <- predicted - actual
  
  support <- seq(0,1,length.out=nbins+1)
  order <- quantile(predicted,support,names=FALSE)
  order <- cut(predicted,breaks=unique(order),labels=FALSE)
  order <- list(order)
  
  res_mean <- aggregate(residual,by=order, FUN=mean)$x
  pred_mean <- aggregate(predicted,by=order, FUN=mean)$x
  length_bin <- aggregate(actual,by=order, FUN=length)$x
  
  pred_mean_order <- order(pred_mean)
  pred_mean_sorted <- pred_mean[pred_mean_order]
  length_bin_sorted <- length_bin[pred_mean_order]
  
  ellipse <- 2 * sqrt(pred_mean_sorted*(1-pred_mean_sorted)/length_bin_sorted)
  
  df <- data.frame(residuals=res_mean, predicted=pred_mean, start=cumsum(length_bin), ellipsex=pred_mean_sorted, ellipsey=ellipse)
  
  return(df)
}

aggregate_predictors <- function(predictors, name, source_table = 'presence'){
  dbExecute(conn, paste0("DROP TABLE IF EXISTS ", name))
  predictors <- paste0(predictors, collapse = ", ")
  query <- paste0("
    CREATE TABLE ", name, " AS 
    SELECT ", predictors, ", 
           COUNT(*) as N,
           SUM(permission_denied) as censored,
           COUNT(*) - SUM(permission_denied) as not_censored,
           SUM(permission_denied)/COUNT(*) as censored_proportion
    FROM ",source_table,"
    GROUP BY ", predictors)
  
  dbExecute(conn, query)
  
  cat('Table', name, 'made.\n')
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

global <- unique(unlist(models))


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

