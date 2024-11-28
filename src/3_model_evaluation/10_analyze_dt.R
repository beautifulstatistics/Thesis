setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")

tree <- readRDS(file=file.path("dt.model"))

attr(tree,"terms")
attr(tree,"largest_terms")

attr(tree,'nodes')
