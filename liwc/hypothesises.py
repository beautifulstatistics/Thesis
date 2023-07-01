
# ### Hypothesises

# We do a regression on the AND space of the following.

# affect ||| emotions pos neg etc -->
# social ||| family friends etc
# cogproc ||| cognatively
# percept ||| see hear
# bio ||| eating health sex
# drives ||| power risk reward
# relativ ||| organization in time and space
# persconc ||| personal life stuff
# totallen ||| length
# ppron ||| personal pronouns


# #### Hypothesis 1: Collective Action
# - ppron
# - relativ
# - percept

# #### Hypothesis 2: Cognative
# - cogproc
# - totallen

# #### Hypothesis 3: Emotionals
# - posemo
# - negemo

# #### Hypothesis 5: Broad search of AND space. (Decision Tree)

HYPOTHESISES = [
                ['ppron','relativ','drives'],
                ['cogproc','tokencount'],
                ['posemo','negemo'],
            ]