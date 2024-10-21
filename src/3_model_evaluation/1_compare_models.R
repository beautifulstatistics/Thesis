setwd("~/Desktop/working8/Thesis")
source("./src/utils/helper_functions.R")

clean <- function(path){
  name <- strsplit(path,'/')[[1]]
  name <- name[length(name)]
  name <- strsplit(name,'\\.')[[1]][1]
  name
}

ll = list()
lla = list()
llb = list()
for (path in list.files("./models", full.names = TRUE, recursive = FALSE)) {
  name <- path
  # if (grepl("residual|global_numeric_rpart|global_presence_rpart", name)) {
  #   next
  # }
  
  print(name)
  if (!grepl("model$", name)) {
    next
  }
  
  
  model <- readRDS(path)

  name <- clean(name)
  ll[[name]] <- -as.numeric(logLik(model))
  lla[[name]] <- aic(model)
  llb[[name]] <- bic(model)
}

ll <- lla
ll <- unlist(ll)
ll <- sort(ll - min(ll))
ll

top <- length(ll)
paste0(top,' Models')


# model <- readRDS('models/global_presence_logit.model')
