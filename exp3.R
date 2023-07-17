setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

connectbB()

df = dbGetQuery(conn, "SELECT affect, permission_denied FROM all_data LIMIT 1000")
df$permission_denied = sample(c(0,1), nrow(df), replace=TRUE)

df$affect <- ifelse(ifelse(df$affect < 3,0,df$affect) > 6, 2, df$affect)

loglike_corrected <- function (y, n, mu, wt, dev) 
{
  m <- if (any(n > 1)) n else wt
  correction <- ifelse(round(m) == 1, 0, log(choose(round(m), round(m * y))))
  -2 * sum(ifelse(m > 0, (wt/m), 0) * dbinom(round(m * y), 
                                             round(m), mu, log = TRUE) - correction)
}


binomial_corrected = quasibinomial()
binomial_corrected$aic = loglike_corrected

a = aggregate(df$permission_denied, list(affect = df$affect),FUN=sum)
a$xx = aggregate(df$permission_denied, list(affect = df$affect),FUN=length)$x - a$x

g1 <- glm(permission_denied ~ affect, df, family = binomial_corrected)
summary(g1)

g2 <- glm(cbind(x, xx)~ affect, a, family= binomial_corrected)
summary(g2)

logLik(g1)
logLik(g2)
