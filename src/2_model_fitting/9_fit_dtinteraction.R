build_query <- function(priors,cands,table){
  if (length(cands) <= 0){
    return("")
  }
  allc <- paste0(c(priors,cands), collapse = ', ')
  if (length(priors)>0) {
    priorsp <- paste0(paste0(priors,collapse = ', '),", ")
  } else {
    priorsp <- ""
  }
  
  query1 <- paste0(
    "\n\nWITH\n",
    "aggset AS (SELECT ", allc, ", SUM(positive) AS positive, SUM(N) AS total FROM ",table," GROUP BY ",allc,'),\n\n')
  
  query2 <- paste0(
    cands, " AS (SELECT '",cands,"' AS name, 2 * SUM((CASE WHEN positive > 0 THEN positive * (LOG(total) - LOG(positive)) ELSE 0 END) + (CASE WHEN (total - positive) > 0 THEN (total - positive) * (LOG(total) - LOG(total - positive)) ELSE 0 END)) AS deviance FROM (SELECT SUM(positive) AS positive, SUM(total) AS total FROM aggset GROUP BY ",
    priorsp, cands, "))",
    collapse = ',\n'
  )
  
  query <- paste0(query1, query2, ',\n\njoined AS (\n')
  query <- paste0(query, paste0("SELECT * FROM ", cands, collapse = "\nUNION ALL\n"))
  query <- paste0(query,'\n)\n\n')
  
  final_query <- paste0(query,"SELECT * FROM joined ORDER BY deviance ASC\n\n")
  
  return(final_query)
}

setwd("/home/kenneywl/Desktop/Thesis")
source("./src/utils/helper_functions.R")

connectdB()
on.exit(disconnectdB())

table <- "aggregated_binomial"
predictors <- models[['global']]

priors <-'image:power:negemo:bio:work:posemo:home:tokencount:affiliation:you:interrog:i:leisure:negate:progm:ipron:percept'
priors <- strsplit(priors, ":")[[1]]
preds <- setdiff(predictors,priors)
tree <- list()
i = 0
while(TRUE){
  print("")
  print(paste("Priors:",paste0(priors,collapse=':')))
  
  print(Sys.time())
  print('Starting Query...')
  query <- build_query(priors,preds,table)
  
  t1 <- Sys.time()
  df <- dbGetQuery(conn,query)
  t2 <- Sys.time() - t1
  print("")
  print(paste(t2,attr(t2,'units')))
  
  print("")
  print(df)
  
  i = i +1
  tree[[i]] = list(PRIORS = priors,
                   PREDICTORS = preds,
                   RESPONSE = df,
                   CHOSEN = df[1,1],
                   DEVIANCE = df[1,2],
                   QUERY = query)

  priors <- c(priors,df[1,1])
  preds <- setdiff(preds,df[1,1])
  
  if (length(preds) == 0){
    break
  }
}

print("Chosen Order:")
print(priors)

saveRDS(tree,file="dt_interactions.model")

print('Finished')