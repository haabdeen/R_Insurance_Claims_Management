# uniforming the missing data in data sets
data.all <- replace_missingsBy(data.all, by = NA)
updateDatasets(data.all)

features.nzv <- findNZV(data.all, subfeatures = features.updated)
updateFeatures(subset(features.updated, 
                      !(features.updated %in% 
                          features.nzv)))

