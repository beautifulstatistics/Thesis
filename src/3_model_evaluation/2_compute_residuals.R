# setwd("~/Desktop/working8/Thesis")
# source("./src/utils/helper_functions.R")
# connectbB()

name = 'global'
table = 'presence'
link = 'logit'
model_path = file.path('models', paste0(name,'_',table,'_',link,'.model'))
model <- readRDS(model_path)

print('Starting calculations...')
da <- make.data('permission_denied', models[[name]], table=table)
predictedLinear <- all_linear_predict.shglm(model, da)
predictedProp <- 1 / (1 + exp(-predictedLinear))

if (sum(is.na(predictedLinear)) > 0 | sum(is.na(predictedProp)) > 0 | sum(predictedProp == 0) > 0){
  print("error in computing predicted values!")
  return()
}

print("Adding Columns...")
if (!"predictedProp" %in% dbListFields(conn, "presence")) {
  dbExecute(conn, "ALTER TABLE presence ADD COLUMN predictedProp REAL")
}
if (!"predictedLinear" %in% dbListFields(conn, "presence")) {
  dbExecute(conn, "ALTER TABLE presence ADD COLUMN predictedLinear REAL")
}

print("Making Temp Table...")
df <- data.frame(predictedProp, predictedLinear)
df$ID <- seq_len(nrow(df))
dbWriteTable(conn, "temp_presence", df, temporary = TRUE)

print("Updating database...")
query <- "
  UPDATE presence
  SET 
    predictedProp = temp.predictedProp,
    predictedLinear = temp.predictedLinear
  FROM temp_presence AS temp
  WHERE presence.ID = temp.ID
"
dbExecute(conn, query)

disconnectdB()
