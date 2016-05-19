# This script will initialize global variables with the names/paths of working directories
# It will then load all required libraries for this project

source("source_dir.r", chdir = T)

wd <- getwd()
dir_preparation <- paste(wd, "/Preparation", sep = "")
dir_data_input <- paste(wd, "/Data/Input", sep = "")
dir_data_output <- paste(wd, "/Data/Output", sep = "")
dir_data_temp <- paste(wd, "/Data/Runtime.temp", sep = "")
dir_lib <- paste(wd, "/Libraries", sep = "")
dir_exploration_figures <- paste(wd, "/Exloration/Figures", sep = "")

source(paste(dir_lib, "/lib.r", sep=""))
source(paste(dir_lib, "/functions.r", sep=""))
source(paste(dir_lib, "/staticDataManipluationFuncs.r", sep=""))
source(paste(dir_lib, "/characterDataFuncs.r", sep=""))
source(paste(dir_lib, "/missingDataManipluationFuncs.r", sep=""))
