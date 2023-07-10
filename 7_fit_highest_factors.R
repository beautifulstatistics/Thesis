setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(speedglm)

connectbB()

predictors <- c("function_", "othergram", "social", "percept", "persconc", 
                'drives', 'affect', 'cogproc', 'bio', 'relativ', 'informal', 'image')
response = c('censored','not_censored')
form_response <- 'cbind(censored,not_censored)'

da = dbGetQuery(conn, "SELECT * FROM aggregated_hf")
form <- prep.formula(form_response, predictors)

glm1 <- glm(form, data=da, family=binomial())

print(summary(glm1))
saveRDS(glm1,file='models/highest_factors_presence.model')

(
  ggplot()

)

disconnectdB()

