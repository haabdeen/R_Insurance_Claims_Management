# uniforming the missing data in data sets
data.all <- replace_missingsBy(data.all, by = NA)
updateDatasets(data.all)
