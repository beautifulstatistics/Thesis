setwd("/home/kenneywl/Desktop/Thesis")
source("./src/utils/helper_functions.R")

connectdB()

name <- 'global'
table <- 'presence'
link <- 'logit'
prop <- "predictedPropADT"
bin <- 'binADT'

model_path <- file.path('models', paste0(name, '_', table, '_', link, '_adjustedADT.model'))
model <- readRDS(model_path)

predictors <- attr(model$tf, 'variables')
predictors <- paste0(predictors)[2:length(predictors)]

da <- make.data(predictors, table=table)

predictedProp <- predict.shglm.all(model, da, type='response')

print('data collected')
res <- bin.residuals(predicted = predictedProp, 
                     actual = make.data.all('permission_denied', table=table)$permission_denied, 
                     nbins = NULL, 
                     return_bin_indices = TRUE)

print('binning finished')
res$predictedProp = predictedProp
res$ID = seq(length(predictedProp))

rm(predictedProp)

if (!prop %in% dbListFields(conn, table)) {
  dbExecute(conn, paste0("ALTER TABLE ", table, " ADD COLUMN ", prop, " REAL"))
}

if (!bin %in% dbListFields(conn, table)) {
  dbExecute(conn, paste0("ALTER TABLE ",table , " ADD COLUMN ",bin ," INTEGER"))
}

dbWriteTable(conn, "temp_table", res, temporary = TRUE)
print('temp table written')

query <- paste0("
  UPDATE ", table, "
  SET 
    ", prop, " = temp_table.predictedProp,
    ", bin," = temp_table.bin
  FROM temp_table
  WHERE ", table, ".ID = temp_table.ID
  
")

dbExecute(conn, query)
print('data written')

dbExecute(conn, "DROP TABLE IF EXISTS temp_table")

disconnectdB()
print("Process completed successfully.")
