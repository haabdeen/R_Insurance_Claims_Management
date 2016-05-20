source(paste(dir_exploration_functions, "/vif_Func.r", sep=""))

Temp <- subset(data.train, data.train$target == 0)
Temp <- data.train[, features.updated]
noncolinearVars0 <- vif_Func(in_frame= Temp, thresh= 50, trace=T)
#
Temp <- subset(data.train, data.train$target == 1)
Temp <- data.train[,features.updated ]
noncolinearVars1 <- vif_Func(in_frame= Temp, thresh= 50, trace=T)
