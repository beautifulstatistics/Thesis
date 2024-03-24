setwd("~/Desktop/working8/Thesis")
library(ggplot2)
library(rattle)
library(rpart)

checks = read.csv('checks.csv')
che = checks[sample(1:nrow(checks),500000),]

tree_model <- rpart(predictedLinear ~ residualDeviance, 
                    data = checks, 
                    method = "anova")

pfit <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])

plot(pfit, uniform = TRUE, main = "Decision Tree")
text(pfit, use.n = TRUE, all = TRUE, cex = .8)

fancyRpartPlot(pfit)

############################
############################

N <- 6
p <- 16

df <- list(y=sample(c(0,1),size=2^N,replace=TRUE))
for( i in 1:p){
  df[[toString(i)]] = sample(c(0,1),size=2^N,replace=TRUE)
  # df[[toString(i)]] = rnorm(N)
}
df <- data.frame(df)

aggregate_df <- aggregate(y ~ ., 
                          data = df, 
                          FUN = function(x) {
  c(success = sum(x), N=length(x))
})

aggregate_df <- cbind(aggregate_df,aggregate_df$y)
aggregate_df$y <- NULL

g <- glm(cbind(success,N-success)~., 
         data=aggregate_df,
         family=binomial())

aggregate_df$l <- predict(g,type='link')
aggregate_df$p <- predict(g,type='response')
aggregate_df$d <- resid(g,type='deviance')
aggregate_df$r <- resid(g,type='response')

print(nrow(aggregate_df))
print(2^p)
print(nrow(aggregate_df) <= 2^p)

(
  ggplot(aggregate_df)
  + aes(x=l,y=d)
  + geom_point()
  + xlab('Predicted Link Values')
  + ylab('Deviance Residuals')
  + labs(title=paste0("Deviance Residuals Vs Linear Predicted"))
  + theme(plot.title = element_text(hjust = 0.5))
)

tree_model <- rpart(l ~ r, 
                    data = aggregate_df, 
                    method = "anova")

# tree_model <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
fancyRpartPlot(tree_model)

p = predict(tree_model)
length(unique(p))
