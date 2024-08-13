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
for(path in os_walk("./models")){
  model <- readRDS(path)
  name <- clean(path)
  
  ll[[name]] = -as.numeric(logLik(model))
  lla[[name]] = model$aic
  llb[[name]] = bic(model)
}

ll <- llb
ll <- unlist(ll)
ll <- sort(ll - min(ll))
ll

top <- length(ll)
bottom <- length(names(models))*2*3
paste0(top,'/',bottom,' = ',round(top/bottom,2), ' finished')