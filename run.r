# save the path of current working directory in wd
wd <- getwd()
source(paste(wd, "/prepareDir.r", sep = ""))
prepare_directories(wd)

sources <- list()
sources[["loadLibsAndFuncs"]] <- paste(dir_lib, "/loadLibsAndFuncs.r", sep = "")
sources[["readData"]] <- paste(dir_preparation, "/readData.r", sep = "")
sources[["uniformingMissingDataByNA"]] <- paste(dir_cleaning, "/uniformingMissingData.r", sep = "")
sources[["dropFeaturesWithNZV"]] <- paste(dir_cleaning, "/dropNZV.r", sep = "")

sapply(sources, source)

# source(paste(dir_lib, "/loadLibsAndFuncs.r", sep = ""))
# source(paste(dir_preparation, "/readData.r", sep = ""))
# source(paste(dir_cleaning, "/uniformingMissingData.r", sep = ""))
# source(paste(dir_cleaning, "/dropNZV.r", sep = ""))