### MODELS

nesting_overall = list(
  top = c('functions', 'othergram', 'social', 'percept', 'persconc',
              'drives', 'affect', 'cogproc', 'bio', 'relativ', 'informal'),
  bottom = c('i','we','you','shehe','they','youpl','ipron','prep','auxverb',
    'adverb','conj','negate','quanunit','prepend','specart','focuspast',
    'focuspresent','focusfuture','progm','modal_pa','general_pa','compare',
    'interrog','number','quant','posemo','anx','anger','sad','family',
    'friend','female','male','insight','cause','discrep','tentat','certain',
    'differ','see','hear','feel','body','health','sexual','ingest','affiliation',
    'achieve','power','reward','risk','motion','space','time','work','leisure',
    'home','money','relig','death','swear','netspeak','assent','nonflu','filler')
)

nesting_bottom = list(
  functions = list(top = 'functions',bottom = c('pronoun','prep','auxverb','adverb','conj','negate',
                'quanunit','prepend','specart','tensem','particle')),
  functions_pronoun = list(top='pronoun', bottom=c('ppron','ipron')),
  functions_pronoun_ppron = list(top='ppron', bottom=c('i','we','you','shehe','they','youpl')),
  functions_tensem = list(top='tensem', bottom=c('focuspast','focusfuture','progm')),
  functions_particle = list(top='particle', bottom=c('modal_pa','general_pa')),
  othergram = list(top='othergram', bottom=c('compare','interrog','number','quant')),
  social = list(top='social', bottom=c('family','friend','female','male')),
  percept = list(top='percept', bottom=c('see','hear','feel')),
  drives = list(top='drives', bottom=c('affiliation','achieve','power','reward','risk')),
  persconc = list(top='persconc', bottom=c('work','leisure','home','money','relig','death')),
  affect = list(top='affect', bottom=c('posemo','negemo')),
  affect_negemo = list(top='negemo', bottom=c('anx','anger','sad')),
  cogproc = list(top='cogproc', bottom=c('insight','cause','discrep','tentat','certain','differ')),
  bio = list(top='bio', bottom=c('body','health','sexual','ingest')),
  relativ = list(top='relativ', bottom=c('motion','space','time')),
  informal = list(top='informal', bottom=c('swear','netspeak','assent','nonflu','filler'))
)

# Then depending on these we may substitute functions bottom predictors with the expanded set.

common_control <- c('tokencount','image')