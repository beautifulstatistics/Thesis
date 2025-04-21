setwd("/home/kenneywl/Desktop/Thesis")
source("./src/utils/helper_functions.R")

connectdB()

table = 'presence'

timeStart <- Sys.time()

fields <- c('predictedProp','permission_denied',models[['global']])
query <- paste0('SELECT', paste0(' AVG(',fields,') AS ',fields,collapse = ','),' FROM ', table,' GROUP BY bin')
avg_bin <- dbGetQuery(conn,query)

time1 <- Sys.time()

print("avg_bin finsihed:")
print(time1-timeStart)

control <- rpart.control(
  minsplit = 2,      # Minimum number of observations in a node for a split to be attempted
  minbucket = 1,     # Minimum number of observations in any terminal node
  cp = 0.001,        # Complexity parameter - lower values create a larger tree
  maxdepth = 8       # Maximum depth of the tree
)

fit <- rpart(predictedProp - permission_denied ~ ., avg_bin, 
             method = 'anova',
             control = control)
fit <- list(fit)

time2 <- Sys.time()

print("rpart finsihed:")
print(time2-time1)

saveRDS(fit, file='global_presence_logit_rpart_residualSen.model')

print('model fit.')

vars <- rpart.sql.extract_vars_depth(fit,prune='cp')
print(vars)

disconnectdB()


# numeric - default
#   variable depth
# 1   affect     3
# 2 interrog     2
# 3    power     1
# 4     work     0

# presence - default
#      variable depth
# 1 affiliation     3
# 2      negemo     0
# 3      posemo     2
# 4       swear     1
# 5        work     1

# presence - sensitive
# variable depth
# 1  affiliation     3
# 2      cogproc     5
# 3       drives     2
# 4       female     4
# 5        ipron     3
# 6       negemo     0
# 7       posemo     2
# 8        swear     1
# 9           we     2
# 10        work     1
