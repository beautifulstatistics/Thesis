
df <- data.frame(y = rnorm(10000),x1=sample(c(0,1),10000,replace=TRUE), x2 = sample(c(0,1),10000,replace=TRUE))
df$x3 <- df$x1*df$x2

lm(y ~ x1:x2,df)
lm(y ~ x3,df)

# Then the intercept should be this, but it isn't
mean(subset(df,x1==0 | x2==0)$y)
mean(subset(df,x1==1 & x2==1)$y) - mean(subset(df,x1==0 | x2==0)$y)
#####

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

linear_predictor <- 1 + x1 - x2
prob_y <- 1 / (1 + exp(-linear_predictor))

df <- data.frame(y = y <- rbinom(10000, 1, prob_y),
                 x1 = x1, x2 = x2)

m1 <- glm(y~x1+x2,df,family=binomial)
dfs <- aggregate(y~x1+x2,data=df,FUN=sum)
dfn <- aggregate(y~x1+x2,data=df,FUN=length)
dfs$n <- dfn$y
dfs$p <- dfs$y/dfs$n
dfs$lp = logit((dfs$y+.5)/(dfs$n+1))
m2 <- lm(lp~x1+x2,dfs)

predict(m1,newdata = dfs,type='response')
inv_logit(predict(m2,newdata = dfs))

setwd("~/Desktop/working2/Thesis")
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

model
Generalized Linear Model of class 'speedglm':
  
  Call:  shglm(formula = permission_denied ~ 1, datafun = da, family = binomial(link = "logit"),      sparse = FALSE, trace = TRUE) 

Coefficients:
  (Intercept)  
-7.135  


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
