setwd("~/Desktop/working2/Thesis")
source("./src/utils/helper_functions.R")

tree <- readRDS(file=file.path("global_aggregated_dt.model"))

for (leaf in tree) {
  cat("Leaf Prediction:", leaf$prediction, "\n")
  cat("Path to Leaf:\n")
  for (condition in leaf$path) {
    cat("  ", condition, "\n")
  }
  cat("\n")
}

connectdB()
dbListTables(conn)

dbListFields(conn,'aggregated_binomial')

dbGetQuery(conn,"SELECT N, positive, negative FROM aggregated_binomial LIMIT 100")
