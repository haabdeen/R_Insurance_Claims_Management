# Load all required libraries for this project

load_Libs_Funcs <- function(lib_directory) {
  source(paste(lib_directory, "/lib.r", sep = ""))
  source(paste(lib_directory, "/functions.r", sep = ""))
  source(paste(lib_directory, "/staticDataManipluationFuncs.r", sep = ""))
  source(paste(lib_directory, "/characterDataFuncs.r", sep = ""))
  source(paste(lib_directory, "/missingDataManipluationFuncs.r", sep = ""))
}

if (!exists("dir_lib", envir = globalenv())){
  stop("Project directories must be initialized first: see prepareDir.r")
}else{
  load_Libs_Funcs(dir_lib)
}
