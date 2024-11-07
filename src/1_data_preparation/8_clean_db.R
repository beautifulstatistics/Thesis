setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")

connectdB()

print('Begining ANALYZE')
dbExecute(conn, "ANALYZE")

print('Begining VACUUM')
dbExecute(conn, "VACUUM")

print("Finished")
disconnectdB()