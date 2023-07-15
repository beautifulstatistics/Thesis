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
      query = query = c("SELECT ", select," FROM ",table)
      if(!is.null(limit)){
        query = c(query, " LIMIT ", limit)
      }
      query = paste(query,collapse = '')
      
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

prep.formula <- function(predictors){
  predictors <- paste0(predictors, collapse = "+")
  
  form_response <- 'cbind(censored,not_censored) ~ '
  form_response <- paste0(form_response, predictors)
  return(formula(form_response))
}

shlm <- function(form, data){
  data(TRUE)
  da2 <- data(FALSE)
  
  form <<- formula(form)
  lm1 <- speedlm(form, data=da2)
  
  da2 <- data(FALSE)
  while(!is.null(da2)) {
    lm1 <- update(lm1, data=da2, add=TRUE)
    da2 <- data(FALSE)
  }
  
  return(lm1)
}


vif_core <- function(response, predictors, table, limit){
  form <- prep.formula(response, predictors)
  data <- make.data(response, predictors, table = table, limit=limit)
  lm1 <- shlm(form, data)
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

linear_predictors.shglm <- function(model, newdata){
  form <- formula(model$tf)
  rowSums(model.matrix(form, newdata) %% coef(model))
}

predict.shglm <- function(model, newdata){
  linear_predictors <- linear_predictors.shglm(model, newdata)
  1 / (1 + exp(-linear_predictors))
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

aggregate_predictors <- function(predictors, name){
  predictors <- paste0(predictors, collapse = ", ")
  dbExecute(conn, paste0("DROP TABLE IF EXISTS ", name))
  query <- paste0("
    CREATE TABLE ", name," AS 
    SELECT ", predictors,", 
           SUM(permission_denied) as censored,
           COUNT(*) - SUM(permission_denied) as not_censored 
    FROM presence
    GROUP BY ", predictors)
  
  dbExecute(conn, query)
  
  cat('Table',name, 'made.\n')
}

aggregate_predictors_test <- function(predictors, nme){
  dbExecute(conn, paste0("DROP TABLE IF EXISTS ", name))
  
  predictors <- paste0(predictors, collapse = ", ")
  query <- paste0("
    CREATE TABLE ", name," AS 
    SELECT ", predictors,", 
           SUM(permission_denied) as censored,
           COUNT(*) - SUM(permission_denied) as not_censored 
    FROM presence
    LIMIT 1")
  
  dbExecute(conn, query)
  
  print(paste0('Table ', name, ' made.'))
}
