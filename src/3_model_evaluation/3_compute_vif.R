setwd("~/Desktop/working8/Thesis")
source("5_helper_functions.R")

connectbB()

vs <- vif(global, 'presence', limit = 10**7)

print(vs)

disconnectdB()
