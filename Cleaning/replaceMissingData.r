cat("\n *** Replace missing data in data sets by -1 *** \n")
data.all <- replace_missingsBy(data.all, by = -1)
updateDatasets(data.all)