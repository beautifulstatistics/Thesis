setwd("~/Desktop/working/Thesis")
source("5_functions.R")
library(speedglm)
library(ggplot2)

connectbB()

models = list(
  highest = c('functions', 'othergram', 'social', 'percept', 'persconc',
              'drives', 'affect', 'cogproc', 'bio', 'relativ', 'informal'),

  functions = c('pronoun','prep','auxverb','adverb','conj','negate',
                'quanunit','prepend','specart','tensem','particle'),
  functions_pronoun = c('ppron','ipron'),
  functions_pronoun_ppron = c('i','we','you','shehe','they','youpl'),
  functions_tensem = c('focuspast','focusfuture','progm'),
  functions_particle = c('modal_pa','general_pa'),

  othergram = c('compare','interrog','number','quant'),
  social = c('family','friend','female','male'),
  percept = c('see','hear','feel'),
  drives = c('affiliation','achieve','power','reward','risk'),
  persconc = c('work','leisure','home','money','relig','death'),

  affect = c('posemo','negemo'),
  affect_negemo = c('anx','anger','sad'),

  cogproc = c('insight','cause','discrep','tentat','certain','differ'),
  bio = c('body','health','sexual','ingest'),
  relative = c('motion','space','time'),
  informal = c('swear','netspeak','assent','nonflu','filler'),

  collective_action = c('ppron','focuspresent','focusfuture','drives','motion','space','time')
)


for(name in names(models)){
  t1 <- Sys.time()
  preds <- models[[name]]
  preds <- c(preds, c('tokencount', 'image'))
  aggregate_predictors(preds,name)
  print(Sys.time()-t1)
}

print(dbListTables(conn))

disconnectdB()
