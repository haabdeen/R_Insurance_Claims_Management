#====================================================================
# Read train and test datasets
#====================================================================

data.train <- read.csv(paste(dir_data_input, "/train.csv", sep = ""), stringsAsFactors= F) 
print(dim(data.train))

data.test <- read.csv(paste(dir_data_input, "/test.csv", sep = ""), stringsAsFactors= F) 
print(dim(data.test))


#merge test and train datasets after marking train data rows
data.test["target"] <- -1
data.train["train_flag"] <- T 
data.test["train_flag"] <- F
data.all <- rbind(data.train, data.test)
print(dim(data.all))




#====================================================================
# Organise features according to their types/classes
#====================================================================
outcome <- "target"
nonfeatures_names <- c("ID", "target", "train_flag")
features <- subset(names(data.all), !(names(data.all) %in% nonfeatures_names) )
updateFeatures(features)


