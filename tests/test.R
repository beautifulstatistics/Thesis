
############################################
set.seed(123)
x1 = rep(c(0,1), each=1000)
x2 = rep(c(rep(c(0), each=500),rep(c(1), each=500)),2)
x3 = rep(rep(c(rep(c(0), each=250),rep(c(1), each=250)),2),2)

linear_predictor <- x1 + x2 + x3 + rnorm(2000,mean=0,sd=.1)
prob <- 1/(1+exp(-linear_predictor))
pos <- rbinom(2000,200,prob)

df <- data.frame(pos = pos,N=200,
                 x1 = x1, x2 = x2, x3 = x3)

m1 <- glm(cbind(pos,N-pos) ~ x1 + x2 + x3,df, family=binomial())
m1



#######################






# normal equations for y ~ x1 + x2 in 2^2 factorial
# sum(df$y)         = N   * b0 + N/2 * b1 + N/2 * b2 + N/4 * b1*b2
# sum(df$y|x1 == 1) = N/2 * b0 + N/2 * b1 + N/4 * b2 + N/4 * b1*b2
# sum(df$y|x2 == 1) = N/2 * b0 + N/4 * b1 + N/2 * b2 + N/4 * b1*b2
# sum(df$y|x2 == 1 &
#          x1 == 1) = N/4 * b0 + N/4 * b1 + N/4 * b2 + N/4 * b1*b2

N <- nrow(df)

A <- matrix(c(
    2,     1,    1,  1/2,
    1,     1,  1/2,  1/2,
    1,   1/2,    1,  1/2,
  1/2,   1/2,  1/2,  1/2
  ), nrow=4, byrow=TRUE)

b <- c(
  2*sum(df$y)/N,
  2*sum(df$y[df$x1 == 1])/N,
  2*sum(df$y[df$x2 == 1])/N,
  2*sum(df$y[df$x1 == 1 & df$x2 == 1])/N
)

solve(A, b)

m1 <- lm(y ~ x1 + x2 + x3,df)
m1

A <- matrix(c(
  2,     1,    1,   1,
  1,     1,  1/2, 1/2,
  1,   1/2,    1, 1/2,
  1,   1/2,  1/2,   1
), nrow=4, byrow=TRUE)

b <- c(
  2*sum(df$y)/N,
  2*sum(df$y[df$x1 == 1])/N,
  2*sum(df$y[df$x2 == 1])/N,
  2*sum(df$y[df$x3 == 1])/N
)

solve(A, b)


######

df <- data.frame(y = sample(c(0,1), 100, replace = TRUE),
                 x1 = sample(c(0,1), 100, replace = TRUE),
                 x2 = sample(c(0,1), 100, replace = TRUE))

model <- glm(y ~ x1, data = df, family = binomial)
model

c0 <- subset(df, x1 == 0)$y
c1 <- subset(df, x1 == 1)$y

N <- length(df$y)
P <- sum(df$y)
n0 <- length(c0)
p0 <- sum(c0)
n1 <- length(c1)
p1 <- sum(c1)

ND <- 2 * P * log(N / P) + 2 * (N - P) * log(N / (N - P))
ND

RD <- (2 * p0 * log(n0 / p0) + 2 * (n0-p0) * log(n0 / (n0-p0))) +
  (2 * p1 * log(n1 / p1) + 2 * (n1-p1) * log(n1 / (n1-p1)))
RD

############

y = sample(c(0,1), 100, replace = TRUE)

df <- data.frame(y = y,
                 x1 = sample(c(0,1), 100, replace = TRUE),
                 x2 = sample(c(0,1), 100, replace = TRUE),
                 x3 = sample(c(0,1), 100, replace = TRUE),
                 x4 = sample(c(0,1), 100, replace = TRUE))

# df[df$x1 == 0 & df$x2 == 0,'y'] = 0

model <- glm(y ~ x1*x2, data = df, family = binomial)
model

c00 <- subset(df, x1 == 0 & x2 == 0)$y
c01 <- subset(df, x1 == 0 & x2 == 1)$y
c10 <- subset(df, x1 == 1 & x2 == 0)$y
c11 <- subset(df, x1 == 1 & x2 == 1)$y

dev <- function(c){
  n <- length(c)
  p <- sum(c)
  2*(ifelse(p>0, p * log(n / p),0) + ifelse(n-p>0,(n-p) * log(n / (n-p)),0))
}

#NULL Deviance
dev(df$y)

#Residual Deviance
dev(c00) + dev(c01) + dev(c10) + dev(c11)

########################
logit <- function(p) {
  if(any(p <= 0 | p >= 1)) stop("Probabilities must be between 0 and 1 (exclusive).")
  log(p / (1 - p))
}

inv_logit <- function(log_odds) {
  exp(log_odds) / (1 + exp(log_odds))
}

x1 = sample(c(0,1), 10000, replace = TRUE)
x2 = sample(c(0,1), 10000, replace = TRUE)

linear_predictor <- 1 + x1 - 7*x2
prob_y <- 1 / (1 + exp(-linear_predictor))

df <- data.frame(y = y <- rbinom(10000, 1, prob_y),
                 x1 = x1, x2 = x2)

m1 <- glm(y ~ x1 + x2,df,family='binomial')
dfs <- aggregate(y ~ x1 + x2, data = df, 
                         FUN = function(x) c(sum = sum(x), 
                                             n = length(x),
                                             p = sum(x)/length(x)))
dfs <- cbind(dfs,dfs$y)
dfs$y <- NULL
dfs$lp = logit(dfs$p)
m2 <- lm(lp~x1+x2,dfs, weights=n)

m1
m2

logit(7130 / 10000)

dfs
empirical_logit <- function(adj) {
  p_adj <- (dfs$sum + adj) / (dfs$n + 2*adj)
  return(log(p_adj / (1 - p_adj)))
}

empirical_logit(0)
empirical_logit(.05)
empirical_logit(3)
empirical_logit(30)
empirical_logit(532.9636)

m1 <- glm(cbind(sum,n-sum) ~ x1 + x2,dfs,family='binomial')
logit(sum(dfs$sum)/sum(dfs$n))

uniroot(function(x) empirical_logit(x) + 0.4058818, interval=c(500,1000))

mean(inv_logit(empirical_logit(532.9636)))

m2 <- lm(empirical_logit(532.9636)~x1+x2,dfs)

predict(m1,newdata = dfs,type='response')
inv_logit(predict(m2,newdata = dfs))

###############################

dev <- function(n,s,p){
  2 * (s * log(s/(n*p)) +
             (n-s) * log((n-s)/(n*(1-p))))
}

p <- dfs$sum/dfs$n
dev(dfs$n,dfs$sum,p)


dfs$deviance <- with(dfs,dev(n,sum,))
glm(cbind(sum,n-sum) ~ x1*x2,dfs,family='binomial')$null.deviance




setwd("~/Desktop/workingfast/Thesis")
source("./src/utils/helper_functions.R")
source("./src/utils/binomial_corrected.R")


connectdB()
disconnectdB()

f <- function(x){
    r = dbGetQuery(conn, "
                   WITH
                   logitp AS (SELECT LOG(((positive + 0.000447388) / (N + 2 * 0.000447388))) - LOG(1 - ((positive + 0.000447388) / (N + 2 * 0.000447388))) as logitp FROM aggregated_binomial)
                   SELECT AVG(logitp) FROM logitp
                   ")
    r$m
}

# change x to find f(x) within .0001 of -7.876303
f(.001) = -7.073517
f(.0001) = -9.372307

# model
# Generalized Linear Model of class 'speedglm':
#   
#   Call:  shglm(formula = permission_denied ~ 1, datafun = da, family = binomial(link = "logit"),      sparse = FALSE, trace = TRUE) 
# 
# Coefficients:
#   (Intercept)  
# -7.135  


dbGetQuery(conn, "SELECT SUM(positive), SUM(N) FROM aggregated_binomial")
logit(86084 / 226841249)
-7.876303



result <- uniroot(function(x) f(x) + 7.876303,
                  interval = c(0.000447389, 0.0004473876),
                  tol = 0.00000001,trace=1)

f(0.000447388) = -7.876303

result <- uniroot(function(x) f(x) + 7.876303,  # f(x) + 7.876303 = 0
                 interval = c(0.0001, 0.001),    # Search interval from your test points
                 tol = 0.0001)  

da <- make.data(response = 'permission_denied',
                predictors = 'N',
                table = "aggregated",
                chunksize = 4 * 10^6,
                limit = 10)

model <- shglm2(permission_denied~1,
                datafun = da,
                family = binomial(link='logit'),
                weights=~N,
                trace = TRUE,
                sparse = FALSE)


dbGetQuery(conn, "WITH
                   logitp AS (SELECT LOG(((positive + 0.000447388) / (N + 2 * 0.000447388))) - LOG(1 - ((positive + 0.000447388) / (N + 2 * 0.000447388))) as logitp FROM aggregated_binomial)
                   SELECT logitp FROM aggregated_binomial, logitp
                   ")


da <- make.data(response = 'permission_denied',
                predictors = 'N',
                table = 'aggregated',
                chunksize = 4 * 10^6)

model <- shglm(permission_denied~1,
                  datafun = da,
                  family = binomial(link='logit'),
                  trace = TRUE,
                  sparse = FALSE)


########################################################################


library(RSQLite)

# Create a connection
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# Create a sample table of employees with managers
dbExecute(con, "
  CREATE TABLE employees (
    id INTEGER PRIMARY KEY,
    name TEXT,
    manager_id INTEGER
  )
")

# Insert sample data
dbExecute(con, "
  INSERT INTO employees (id, name, manager_id) VALUES 
    (1, 'CEO', NULL),
    (2, 'Manager1', 1),
    (3, 'Manager2', 1),
    (4, 'Employee1', 2),
    (5, 'Employee2', 2),
    (6, 'Employee3', 3)
")

# Recursive query to get all subordinates of a manager
query <- "
  WITH RECURSIVE subordinates AS (
    -- Base case: direct reports
    SELECT id, name, manager_id, 1 as level
    FROM employees
    WHERE manager_id = 1  -- Start with CEO's direct reports
    
    UNION ALL
    
    -- Recursive case: reports of reports
    SELECT e.id, e.name, e.manager_id, s.level + 2
    FROM employees e
    INNER JOIN subordinates s ON e.manager_id = s.id
  )
  SELECT * FROM subordinates
  ORDER BY level, id;
"

# Execute the query
result <- dbGetQuery(con, query)
print(result)

# Clean up
dbDisconnect(con)

########################################################

x1 = rep(c(0,1,2,3), each=1000)
x2 = rep(c(rep(c(0), each=500),
           rep(c(1), each=500),
           rep(c(2), each=500),
           rep(c(3), each=500)),
           2)
x3 = rep(rep(c(rep(c(0), each=250),rep(c(1), each=250)),2),2)

linear_predictor <- 2*x1
df <- data.frame(y = linear_predictor,
                 x1 = as.factor(x1))

m1 <- lm(y ~ x1,df)
m1

c00 <- subset(df, x1 == 0 & x2 == 0)$y
c01 <- subset(df, x1 == 0 & x2 == 1)$y
c10 <- subset(df, x1 == 1 & x2 == 0)$y
c11 <- subset(df, x1 == 1 & x2 == 1)$y

a <- aggregate(y~x1,df,FUN=mean)
a

predict(m1,newdata=a)





