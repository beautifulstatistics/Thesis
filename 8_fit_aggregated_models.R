setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

connectbB()

tables = dbListTables(conn)
tables = tables[!(tables %in% c('presence','all_data'))]

dir.create('models',showWarnings = FALSE)
dir.create('models/aggregated',showWarnings = FALSE)
dir.create('models/aggregated/residuals',showWarnings = FALSE)
dir.create('models/aggregated/plots',showWarnings = FALSE)

for(name in tables){
    da = dbGetQuery(conn, paste0("SELECT * FROM ",name))
    
    model <- glm(cbind(censored,not_censored)~ ., data=da, family=quasibinomial())
    print(summary(model))
    
    saveRDS(model,file=paste0('models',name,'.model'))
    
    da$N <- da$censored + da$not_censored
    da$predicted <- predict(model, type= 'link')
    da$residual <- resid(model, type = 'deviance')
    
    write.csv(da,paste0('models/aggregated/residuals/',name,'.csv'))
    
    p = (
      ggplot(da)
      + aes(x=predicted,y=residual)
      + geom_point()
      + xlab('Predicted Values')
      + ylab('Deviance Residuals')
      + labs(title=paste0(name," Model: Residuals Vs Predicted"))
      + theme(plot.title = element_text(hjust = 0.5))
    )
    
    ggsave(paste0('models/aggregated/plots/',name,'.png'), plot = p)
    
}

disconnectdB()




















