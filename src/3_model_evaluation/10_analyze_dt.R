


tree <- readRDS(file=file.path("global_presence_dt.model"))

for (leaf in tree) {
  cat("Leaf Prediction:", leaf$prediction, "\n")
  cat("Path to Leaf:\n")
  for (condition in leaf$path) {
    cat("  ", condition, "\n")
  }
  cat("\n")
}