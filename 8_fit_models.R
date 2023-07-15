setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(speedglm)

connectbB()

tables = dbListTables(conn)
tables = tables[!(tables %in% c('presence','all_data'))]

dir.create('models',showWarnings = FALSE)
dir.create('models/residuals',showWarnings = FALSE)
dir.create('models/plots',showWarnings = FALSE)

for(name in tables){
    da = dbGetQuery(conn, paste0("SELECT * FROM ",name))
    
    model <- glm(cbind(censored,not_censored)~ ., data=da, family=quasibinomial())
    print(summary(model))
    
    saveRDS(model,file=paste0('models',name,'.model'))
    
    da$N <- da$censored + da$not_censored
    da$predicted <- predict(model, type= 'link')
    da$residual <- resid(model, type = 'deviance')
    
    write.csv(da,paste0('models/residuals/',name,'.csv'))
    
    p = (
      ggplot(da)
      + aes(x=predicted,y=residual)
      + geom_point()
      + xlab('Predicted Values')
      + ylab('Deviance Residuals')
      + labs(title=paste0(name," Model: Residuals Vs Predicted"))
      + theme(plot.title = element_text(hjust = 0.5))
    )
    
    ggsave(paste0('models/plots/',name,'.png'), plot = p)
    
}

disconnectdB()

da = dbGetQuery(conn, 'SELECT permission_denied, posemo, negemo FROM presence LIMIT 4')

model.matrix(permission_denied ~ posemo * negemo, da)

