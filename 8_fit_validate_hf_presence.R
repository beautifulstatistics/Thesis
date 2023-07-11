setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(speedglm)
library(ggplot2)

connectbB()

predictors <- c("tokencount", "function_", "othergram", "social", "percept", "persconc", 
                'drives', 'affect', 'cogproc', 'bio', 'relativ', 'informal', 'image')

response = c('censored','not_censored')
form_response <- 'cbind(censored,not_censored)'

da = dbGetQuery(conn, "SELECT * FROM aggregated_hf")
form <- prep.formula(form_response, predictors)
glm1 <- glm(form, data=da, family=quasibinomial())
print(summary(glm1))

saveRDS(glm1,file='models/highest_factors_presence.model')

da$N <- da$censored + da$not_censored
da$predicted <- predict(glm1, type= 'link')
da$residual <- resid(glm1, type = 'pearson')

(
  ggplot(da)
  + aes(x=predicted,y=residual)
  + geom_point()
)

disconnectdB()
