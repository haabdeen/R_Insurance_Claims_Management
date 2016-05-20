cat("\nUniforming the missing data in data sets\n")
data.all <- replace_missingsBy(data.all, by = NA)
updateDatasets(data.all)
