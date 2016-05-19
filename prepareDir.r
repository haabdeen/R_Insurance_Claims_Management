# This script will initialize global variables with the names/paths of working directories
prepare_directories <- function(working_dir) {
  #source("source_dir.r", chdir = T)
  dir_preparation <<- paste(working_dir, "/Preparation", sep = "")
  dir_data_input <<- paste(working_dir, "/Data/Input", sep = "")
  dir_data_output <<- paste(working_dir, "/Data/Output", sep = "")
  dir_data_temp <<- paste(working_dir, "/Data/Runtime.temp", sep = "")
  dir_lib <<- paste(working_dir, "/Libraries", sep = "")
  dir_cleaning <<- paste(working_dir, "/Cleaning", sep = "")
  dir_exploration <<- paste(working_dir, "/Exploration", sep = "")
  dir_exploration_figures <<- paste(dir_exploration, "/Figures", sep = "")
  dir_exploration_functions <<- paste(dir_exploration, "/Functions", sep = "")
}