source(paste(dir_exploration, "/visualize_missings_patterns_Funcs.r", sep=""))

visualize_missings_patterns(data.all, 
                            subfeatures = features.updated,
                            sampleSize = 20000,
                            fileName = paste(dir_exploration_figures, "/MissingDataPattern.png", sep = ""))

# The obtained chart shows that almost all variables contain missing values
# In some variables the frequency of missing values is relatively high
# We cannot assume that the missing data are Missing at Random, not for all variables