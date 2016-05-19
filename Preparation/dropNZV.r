# find features with near zero variance
findNZV <- function(x, freqCut = 95/5, saveMetrics = T, foreach = T, allowParallel = T, 
                    subfeatures = NULL, sampleSize = NULL, verbos = T){
  require(caret)
  if(is.null(subfeatures)) subfeatures <- names(x)
  if(!is.null(sampleSize)) x <- x[sample(nrow(x), sampleSize),]
  Temp <- nearZeroVar(x[, subfeatures], freqCut = 95/5, saveMetrics = T, foreach = T, allowParallel = T)
  Temp <- Temp[Temp$nzv,]
  if(verbos) print(Temp)
  return(row.names(Temp))
}

# uniforming the missing data in data sets
data.all <- replace_missingsBy(data.all, by = NA)
updateDatasets(data.all)

# find and drop features with near zero variance in data.all
features.nzv <- findNZV(data.all, subfeatures = features.updated)
dropFeatures(features.nzv)

