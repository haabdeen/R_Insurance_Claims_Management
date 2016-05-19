# call to update the content of datasets, data.all, data.train and data.test
updateDatasets <- function(df){
  data.all <<- df
  data.train <<- subset(data.all, data.all$train_flag)
  data.test <<- subset(data.all, !(data.all$train_flag))
}


# call when you whant to update the lists of features
updateFeatures <- function(featuresList){
  features.updated <<- featuresList
  print("Counter of all selected features: ")
  print(length(features.updated))
  
  features.categoric.updated <<-  subset(features.updated, sapply(data.all[, features.updated], 
                                                                  is.character))
  print("Counter of Categoric selected features: ")
  print(length(features.categoric.updated))
  
  features.numeric.updated <<-  subset(features.updated, !(features.updated %in% 
                                                             features.categoric.updated)) 
  print("Counter of Numeric selected features: ")
  print(length(features.numeric.updated))
  
  
 features.numeric.discrete.updated <<- subset(features.numeric.updated, sapply(data.all[, features.numeric.updated], 
                                                                               is.discreteVar)) 
  print("Counter of Numeric Discrete selected features: ")
  print(length(features.numeric.discrete.updated))
  
  features.numeric.continuous.updated <<- subset(features.numeric.updated, !(features.numeric.updated %in% 
                                                                               features.numeric.discrete.updated))
  print("Counter of Numeric Continuous selected features: ")
  print(length(features.numeric.continuous.updated))
}

dropFeatures <- function(fs){
  updateFeatures(subset(features.updated, 
                        !(features.updated %in% 
                            fs)))
}

addFeatures <- function(fs){
  updateFeatures(c(features.updated, fs))
}