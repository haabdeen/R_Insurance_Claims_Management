source(paste(dir_exploration_functions, "/vif_Func.r", sep=""))

# find numeric vars which don't have colinearity issues with other vars
Temp <- subset(data.train, data.train$target == 0)
Temp <- data.train[sample(nrow(data.train), 15000), features.numeric.updated]
noncolinearVars0 <- vif_Func(in_frame= Temp, thresh= 50, trace=T)
#
Temp <- subset(data.train, data.train$target == 1)
Temp <- data.train[sample(nrow(data.train), 15000),features.numeric.updated ]
noncolinearVars1 <- vif_Func(in_frame= Temp, thresh= 50, trace=T)
