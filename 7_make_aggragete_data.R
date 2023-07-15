setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(speedglm)
library(ggplot2)

connectbB()

models = list(
  highest = c('tokencount', 'functions', 'othergram', 'social', 'percept', 'persconc',
              'drives', 'affect', 'cogproc', 'bio', 'relativ', 'informal', 'image'),

  functions = c('tokencount', 'pronoun','prep','auxverb','adverb','conj','negate',
                'quanunit','prepend','specart','tensem','particle'),
  functions_pronoun = c('tokencount','ppron','ipron'),
  functions_pronoun_ppron = c('tokencount','i','we','you','shehe','they','youpl'),
  functions_tensem = c('tokencount','focuspast','focusfuture','progm'),
  functions_particle = c('tokencount','modal_pa','general_pa'),

  othergram = c('tokencount','compare','interrog','number','quant'),
  social = c('tokencount','family','friend','female','male'),
  percept = c('tokencount','see','hear','feel'),
  drives = c('tokencount','affiliation','achieve','power','reward','risk'),
  persconc = c('tokencount', 'work','leisure','home','money','relig','death'),

  affect = c('tokencount','posemo','negemo'),
  affect_negemo = c('tokencount','anx','anger','sad'),

  cogproc = c('tokencount','insight','cause','discrep','tentat','certain','differ'),
  bio = c('tokencount','body','health','sexual','ingest'),
  relative = c('tokencount','motion','space','time'),
  informal = c('tokencount','swear','netspeak','assent','nonflu','filler'),

  collective_action = c('tokencount','ppron','focuspresent','focusfuture','drives','motion','space','time')
)


for(name in names(models)){
  t1 <- Sys.time()
  preds <- models[[name]]
  aggregate_predictors(preds,name)
  print(Sys.time()-t1)
}

print(dbListTables(conn))

disconnectdB()
