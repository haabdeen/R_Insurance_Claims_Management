#====================================================================
cat("\nRead train and test datasets\n")
#====================================================================

data.train <- read.csv(paste(dir_data_input, "/train.csv", sep = ""), stringsAsFactors= F) 
print("data train dim: ")
print(dim(data.train))

data.test <- read.csv(paste(dir_data_input, "/test.csv", sep = ""), stringsAsFactors= F) 
print("data test dim: ")
print(dim(data.test))


#merge test and train datasets after marking train data rows
data.test["target"] <- -1
data.train["train_flag"] <- T 
data.test["train_flag"] <- F
data.all <- rbind(data.train, data.test)
print("all data (train + test) dim: ")
print(dim(data.all))




#====================================================================
cat("\nOrganise features according to their types/classes\n")
#====================================================================
outcome <- "target"
nonfeatures_names <- c("ID", "target", "train_flag")
features <- subset(names(data.all), !(names(data.all) %in% nonfeatures_names) )
updateFeatures(features)


