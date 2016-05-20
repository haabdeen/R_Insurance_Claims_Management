source(paste(dir_exploration_functions, "/vif_Func.r", sep=""))

if(any(is.na(data.all))){
  source(paste(dir_cleaning, "/replaceMissingData.r", sep=""))
}
if(any(is.na(data.all))){
  stop("Data still contains NAs")
}

# find numeric vars which don't have colinearity issues with other vars
# features.numeric.updated
# [1] "v10"  "v12"  "v14"  "v21"  "v34"  "v40"  "v50"  "v62"  "v72"  "v114" "v129"
Temp <- subset(data.train, data.train$target == 0)
Temp <- data.train[sample(nrow(data.train), 15000), features.numeric.updated]
noncolinearVars0 <- vif_Func(in_frame= Temp, thresh= 10, trace=T)
# [1] "v12"  "v14"  "v21"  "v40"  "v50"  "v62"  "v72"  "v129"
#
Temp <- subset(data.train, data.train$target == 1)
Temp <- data.train[sample(nrow(data.train), 15000),features.numeric.updated ]
noncolinearVars1 <- vif_Func(in_frame= Temp, thresh= 10, trace=T)
# [1] "v12"  "v14"  "v21"  "v50"  "v62"  "v72"  "v114" "v129"

noncolinearVars <- union(noncolinearVars0, noncolinearVars1)
# [1] "v12"  "v14"  "v21"  "v40"  "v50"  "v62"  "v72"  "v129" "v114"

colinearVars <- setdiff(features.numeric.updated, noncolinearVars)
# "v10" "v34"
