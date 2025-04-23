setwd("/home/kenneywl/Desktop/Thesis")
source("./src/utils/helper_functions.R")

connectdB()

print('Begining ANALYZE')
dbExecute(conn, "ANALYZE")

print("Finished")
disconnectdB()
