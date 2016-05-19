
library(data.table)
library(plyr)
library(reshape2)
library(scales)
library(stringr)
library(tidyr)
library(regexr)
library(Hmisc)
library(ggplot2)
library(GGally)
library(gridExtra)
library(VIM)
###########################################
# Machine learning packages
###########################################
library(rpart)
library(stats)
library(gbm)
library(FSelector)
library(ROCR)
library(pROC)
library(caret)
#library(missForest)


#====================================================================
# Functions and Constants
#====================================================================

missingValueDefault <- -1

#define a new functions for missing values
is.missing <- function(x){
  is.na(x) | is.nan(x) | is.infinite(x) | x==""
}

missing.omit <- function(x){
  subset(x, !is.missing(x))
}

is.missing.data.frame <- function(x){
  do.call(cbind, lapply(x, is.missing))
}



# return the names of categorical variables in df where number of levels is larger than a threshold
bigFactors = function(df, cat_variables, threshold){
  bigFactors <- c()
  levelsSize <- NULL
  for(cat in cat_variables){
    cat_var <- df[, cat]
    levelsSize <- length(unique(cat_var))
    if(levelsSize >= threshold){
      bigFactors <- c(bigFactors, cat) 
    }
  }
  return(bigFactors) 
}

# functions for replacing missing/incomplete values
#===================================================
#replace NA/NaN items by the median or mean value in a variable/vector
missings2Numeric <- function(df, var, fun =c('mean', 'median'), verbos = F){
  colItems <- df[, var]
  try(if (!is.numeric(colItems))
    stop(paste(
      "var must be a numeric variable in df:", 
      names(df[,var, drop = F]), 
      "is not numeric.", 
      sep = " ")
    )
  )
  missingItems <- is.na(colItems) | is.nan(colItems)
  numeric_value <- NULL
  if(fun == 'median'){
    numeric_value <- median(subset(colItems, !is.missing(colItems), na.rm=TRUE))
  }else{
    numeric_value <- mean(subset(colItems, !is.missing(colItems), na.rm=TRUE))
  }
  df[,var][missingItems] <- numeric_value
  if(verbos){
    print(paste(paste(rep("-", each=nchar(var)+2), collapse =""), 
                sum(missingItems), "na items replaced by the", 
                fun, 
                numeric_value, 
                sep = " "))
  }
  return(df)
}

#convert the item with infinite to max value from a vector (the vector must be numeric)
infinite2max <- function(df, var, verbos = F){
  colItems <- df[, var]
  try(if (!is.numeric(colItems))
    stop(paste(
      "var must be a numeric variable in df:", 
      names(df[,var, drop = F]), 
      "is not numeric.", 
      sep = " ")
    )
  )
  infItems <- is.infinite(colItems)
  mx <- max(subset(colItems, !is.missing(colItems)), na.rm=TRUE)
  df[,var][infItems] <- mx
  if(verbos){
    print(paste(paste(rep("-", each=nchar(var)+2), collapse =""), 
                sum(infItems), "infinite items replaced by max", mx, sep = " ")) 
  }
  return(df)
}

replace_missings2Numerics <- function(df, vars, fun =c('mean', 'median'), verbos= FALSE){
  Temp <- df
  for(var in vars){
    print(var)
    Temp <- infinite2max(missings2Numeric(Temp, var, fun, verbos), var, verbos)
  }
  return(Temp)
}




trimspaces <- function(x){ gsub("\\s", "", x) }

strsplitV <- function(v, s=".", missingValue = as.character(missingValueDefault)){
  vc <- trimspaces(as.character(v))
  for (i in 1:length(vc)) {
    if(!(is.na(vc[i])) && ifelse(is.na(missingValue), T, vc[i]!=missingValue) && nchar(vc[i]) > 1){
      vc[i] <- paste(unlist(strsplit(vc[i], split="", fixed = T)), collapse = s) 
    }
  }
  return(vc)
}

separateStringFactor <- function(df, colName, missingValue = as.character(missingValueDefault)){
  dft <- data.frame(x=df[, c(colName)])
  fcharacter <- as.character(dft$x)
  ncol <- max(sapply(fcharacter, nchar))
  into <- paste(colName, c(1:ncol), sep = ".")
  dft$x <- strsplitV(fcharacter, s="::", missingValue = missingValue)
  separate(dft, "x", into=into, extra = "warn", sep = "::", fill = "right")
}

# df <- data.frame(newcol=c("AB CD", "-10", " A ", "BAZ XYG ", " YT", "-10", " B "))
# 
# separateStringFactor(df, "newcol")
# newcol.1 newcol.2 newcol.3 newcol.4 newcol.5 newcol.6
# 1        A        B        C        D     <NA>     <NA>
# 2        A        A        B     <NA>     <NA>     <NA>
# 3        B        A        Z        X        Y        G
# 4        Y        T     <NA>     <NA>     <NA>     <NA>


notmisssings <- function(x){
  subset(x, !is.na(x) & x!= missingValueDefault)
}

statistics <- function(vec){
  r <- vec
  y <- notmisssings(r)
  ###################
  s.min <- NA
  s.max <- NA
  s.mean <- NA
  s.median <- NA
  s.sum <- NA
  s.05 <- NA
  s.10 <- NA
  s.25 <- NA
  s.50 <- NA
  s.75 <- NA
  s.90 <- NA
  s.95 <- NA
  s.missings <- length(r)
  s.negatives <- 0
  
  if(length(y) > 0){
    sts <- describe(y)[["counts"]]
    s.min <- min(y)
    s.max <- max(y)
    s.sum <- sum(y)
    s.mean <- sts["Mean"][[1]]
    s.05 <- sts[".05"][[1]]
    s.10 <- sts[".10"][[1]]
    s.25 = sts[".25"][[1]]
    s.50 = sts[".50"][[1]]
    s.75 = sts[".75"][[1]]
    s.90 = sts[".90"][[1]]
    s.95 = sts[".95"][[1]]
    s.missings <- length(subset(r, is.na(r) || (r == missingValueDefault)))
    s.negatives <- length(subset(y, y < 0))
  }
  list(s.min=s.min, s.max=s.max, s.sum=s.sum, s.mean=s.mean, 
       s.05=s.05, s.10=s.10, s.25=s.25, s.50=s.50, s.75=s.75, s.90=s.90, s.95=s.95, 
       s.missings=s.missings, s.negatives=s.negatives)
}

getStatsOnRows <- function(df, replaceMissings = FALSE, replaceMissingsBy = missingValueDefault){ 
  Temp <- as.data.frame(t(apply(df, 1, function(x){ unlist(statistics(x), use.names=F) })))
  setnames(Temp, old = names(Temp), new = c("s.min", "s.max", "s.sum", "s.mean", 
                                            "s.05", "s.10", "s.25", "s.50", "s.75", "s.90", "s.95", 
                                            "s.missings", "s.negatives") 
  )
  Temp <- apply(Temp, 2, function(x){ as.numeric(as.character(x)) })
  if(replaceMissings==TRUE){
    Temp[is.missing.data.frame(Temp)] <- replaceMissingsBy
  }
  return(Temp)
}

#====================================================================
# End Functions
#====================================================================


# Set a seed for reproducibility
set.seed(2000)






#====================================================================
# Read Data
#====================================================================
# Read train and test datasets
project_dir <- "D:/Kaggle/Projects/BNP_Paribas_Cardif_Claims_Management"

data.train <- read.csv(paste(project_dir, "/input/train.csv", sep = ""), stringsAsFactors= F) 
print(dim(data.train))

data.test <- read.csv(paste(project_dir, "/input/test.csv", sep = ""), stringsAsFactors= F) 
print(dim(data.test))


#merge test and train datasets after marking train data rows
data.test["target"] <- -1
data.train["train_flag"] <- T 
data.test["train_flag"] <- F
data.all <- rbind(data.train, data.test)
print(dim(data.all))


# call to update the content of datasets, data.all, data.train and data.test
updateDatasets <- function(df){
  data.all <<- df
  data.train <<- subset(data.all, data.all$train_flag)
  data.test <<- subset(data.all, !(data.all$train_flag))
}



#====================================================================
# Organise features according to their types/classes
#====================================================================
outcome <- "target"
nonfeatures_names <- c("ID", "target", "train_flag")
features <- subset(names(data.all), !(names(data.all) %in% nonfeatures_names) )
features.categoric <- names(data.all[, sapply(data.all, is.character)])
features.numeric <- names(data.all[, sapply(data.all, is.numeric)])
features.numeric <- subset(features.numeric, !(features.numeric %in% nonfeatures_names))

# organize numeric variables as continuous or discrete
features.numeric.discrete <- c("v62", "v129", "v38", "v72")
features.numeric.continuous <- subset(features.numeric, !(features.numeric %in% features.numeric.discrete))

features.updated <- features
features.numeric.updated <- features.numeric
features.categoric.updated <- features.categoric
features.numeric.discrete.updated <- features.numeric.discrete
features.numeric.continuous.updated <- features.numeric.continuous
features.updated.dummy <- NULL

# call when you whant to update the list of features
updateFeatures <- function(featuresList) {
  features.updated <<- featuresList
  print("Counter of all selected features: ")
  print(length(features.updated))
  
  features.categoric.updated <<-
    subset(features.categoric.updated,
           features.categoric.updated %in% features.updated)
  print("Counter of Categoric selected features: ")
  print(length(features.categoric.updated))
  
  features.numeric.updated <<-
    subset(features.numeric.updated,
           features.numeric.updated %in% features.updated)
  print("Counter of Numeric selected features: ")
  print(length(features.numeric.updated))
  
  features.updated.dummy <<-
    subset(features.updated.dummy,
           features.updated.dummy %in% features.updated)
  print("Counter of Dummy selected features: ")
  print(length(features.updated.dummy))
  
  features.numeric.discrete.updated <<-
    subset(
      features.numeric.discrete.updated,
      features.numeric.discrete.updated %in% features.numeric.updated
    )
  print("Counter of Numeric Discrete selected features: ")
  print(length(features.numeric.discrete.updated))
  
  features.numeric.continuous.updated <<-
    subset(features.numeric.updated,
           !(
             features.numeric.updated %in% union(features.updated.dummy, features.numeric.discrete.updated)
           ))
  print("Counter of Numeric Continuous selected features: ")
  print(length(features.numeric.continuous.updated))
}


features.updated.withMissings <- c("v56", "v31", "v21", "v22", "v112",
                                   "v34", "v40", "v12", "v50", "v10",
                                   "v125", "v114", "v14", "v52", "v91", "v107",
                                   "v30", "v113", "v102", "v23", "v51", "v85", "v119", "v123", "v16", "v69",
                                   "v78", "v115", "v131", "v1", "v2", "v4", "v6", "v7", "v9", "v11", "v13",
                                   "v15", "v17", "v18", "v19", "v20", "v26", "v27", "v28", "v29", "v32", "v33",
                                   "v35", "v37", "v39", "v41", "v42", "v43", "v44", "v45", "v48", "v49", "v53",
                                   "v55", "v57", "v58", "v59", "v60", "v61", "v64", "v65", "v67", "v68", "v73",
                                   "v76", "v77", "v80", "v83", "v84", "v86", "v88", "v90", "v92", "v93", "v94",
                                   "v95", "v96", "v97", "v99", "v100", "v101", "v103", "v104", "v106", "v111",
                                   "v116", "v118", "v120", "v121", "v122", "v126", "v127", "v130", "v87", "v98",
                                   "v105", "v5", "v8", "v25", "v36", "v46", "v54", "v63", "v70", "v81", "v82",
                                   "v89", "v108", "v109", "v117", "v124", "v128")


# let's first replace "" values, or any type of incomplete/missing values such as nan or infinit, by NA values
# data.all[is.missing.data.frame(data.all)] <- NA
data.all[is.missing.data.frame(data.all)] <- NA


updateDatasets(data.all)
#====================================================================
# End Read Data
#====================================================================




#===================================================================================
# Drop near-zero variance variables
#===================================================================================

# Let's trust the caret function ?nearZeroVar to detect variables that have zero- or near zero-varience
# Temp <- data.all[, features]
# Temp <- nearZeroVar(Temp, freqCut = 95/5, saveMetrics = T)
# Temp[Temp$nzv,]
#     freqRatio percentUnique zeroVar  nzv
# v3   31.83537   0.001748909   FALSE TRUE
# v74 162.30571   0.001311682   FALSE TRUE
# v38  51.91442   0.005246727   FALSE TRUE


features.nzv <- c("v3", "v74", "v38")

updateFeatures(subset(features.updated,!(features.updated %in% features.nzv)))




# #===================================================================================
# # Analyzing factors (character variables)
# #===================================================================================
# 
# First remove v91 s it's a duplication of another categorical variable
updateFeatures(subset(features.updated,!(features.updated %in% c("v91"))))


# # some categoric variables their values are vector of characters/letters
# # I'll separate/split each varible in multiple columns: column per character
features.categoric.updated.stringFactors <-
  c("v22", "v56", "v113", "v125")
features.categoric.updated.notStringFactors <-
  subset(
    features.categoric.updated,
    !(
      features.categoric.updated %in%
        features.categoric.updated.stringFactors
    )
  )


###### First of all, let's clean categoric variables by triming all spaces in these variables
for(feature in features.categoric.updated) {
  data.all[, feature] <- trimspaces(data.all[, feature])
}
updateDatasets(data.all)

# Separate string factor to character factors
Temp <- data.all[, features.categoric.updated.stringFactors]
features.separatedStringFactors <- NULL
for (feature in features.categoric.updated.stringFactors) {
  df <- separateStringFactor(Temp, feature, missingValue = NA)
  Temp <- cbind(Temp, df)
  features.separatedStringFactors <<-
    c(features.separatedStringFactors, names(df))
}
Temp <- Temp[, features.separatedStringFactors]

# Temp[is.na(Temp)] <- as.character(missingValueDefault)
# #Remove from these new categoric variables those which have nzv
# Temp.nzv <- nearZeroVar(Temp, freqCut = 95/5, saveMetrics = T)
# Temp.nzv[Temp.nzv$nzv,]
#         freqRatio percentUnique zeroVar  nzv
# v22.4    23.6267  0.0118051365   FALSE TRUE
# features.separatedStringFactors <- subset(names(Temp),
#                                           !(names(Temp) %in% c("v22.4")))
# Temp <- Temp[, features.separatedStringFactors]


data.all <- cbind(data.all, Temp)
features.updated <-
  c(features.updated, features.separatedStringFactors)
features.categoric.updated <-
  c(features.categoric.updated, features.separatedStringFactors)
updateDatasets(data.all)



#apply(data.all[, features.categoric.updated], 2, function(x){length(unique(x))})
# # v22    v24    v30    v31    v47    v52    v56    v66    v71    v75    v79   v107   v110 
# # 23420      5      8      4     10     13    131      3     12      4     18      8      3 
# # v112   v113   v125  v22.1  v22.2  v22.3  v22.4  v56.1  v56.2 v113.1 v113.2 v125.1 v125.2 
# # 23     38     91     27     27     27     27      27     27     27     12     27     27 
# 
# Analyzing the number of levels in factors I categorize them as small and big factors:
features.categoric.updated.smallFactors <-
  c("v24", "v30", "v31", "v52", "v66", "v75", "v107", "v110")
features.categoric.updated.bigFactors  <-
  subset(
    features.categoric.updated,!(
      features.categoric.updated %in% features.categoric.updated.smallFactors
    )
  )

# # for small factors, I'll consider the dummy transformation
# # for big factors, I'll consider only numeric transformation


# # Get the dummies of features.categoric.updated.smallFactors
Temp <- data.all[, features.categoric.updated.smallFactors]
Temp <- as.data.frame(apply(Temp, 2, factor))
features.categoric.updated.dummy <-
  dummyVars("~.", data = Temp, fullRank = T)
data.dummies <-
  as.data.frame(predict(features.categoric.updated.dummy, Temp))
data.dummies[is.na(data.dummies)] <- missingValueDefault
#
# Temp <- nearZeroVar(data.dummies, freqCut = 98 / 2, saveMetrics = T)
# Temp[Temp$nzv, ]
# v75.C  5577.3902  0.0008744546   FALSE TRUE
# v107.G  508.3742  0.0013116818   FALSE TRUE
#
data.dummies <- data.dummies[, !(names(data.dummies) %in%
                                   c("v75.C", "v107.G"))]

Temp <- cbind(data.all, data.dummies)
updateDatasets(Temp)
features.updated.dummy <- names(data.dummies)
features.updated <- c(features.updated, features.updated.dummy)
features.numeric.updated <- c(features.numeric.updated, features.updated.dummy)

length(features.updated)
# 171



#===================================================================================
# Transforming categoric variables to numeric
#===================================================================================

Temp <- data.all[, features.categoric.updated]
for (feature in features.categoric.updated) {
  fcaharacter <- as.character(Temp[, feature])
  levels <- sort(unique(fcaharacter))
  map <- c(1:length(levels)) 
  Temp[,feature] <- as.numeric(mapvalues(fcaharacter, from=levels, to=map))
}

numericTransExt <- "numericTrans"
features.categoric.updated.numericTrans <-
  paste(features.categoric.updated, numericTransExt, sep = ".")
features.categoric.updated.smallFactors.numericTrans <- 
  paste(features.categoric.updated.smallFactors, numericTransExt, sep = ".")
features.categoric.updated.bigFactors.numericTrans <- 
  paste(features.categoric.updated.bigFactors, numericTransExt, sep = ".")

data.all[, features.categoric.updated.numericTrans] <- Temp
features.updated <- c(features.updated, features.categoric.updated.numericTrans)

updateDatasets(data.all)




#===================================================================================
# Replacing incomplete values (NAs) by the missingValueDefault
#===================================================================================

data.all[, features.categoric.updated] <-
  as.data.frame(apply(data.all[, features.categoric.updated], 2, as.character))
data.all[is.na(data.all)] <- missingValueDefault
updateDatasets(data.all)




#=============================================================================
# Identify and remove correlated variables
#=============================================================================

Temp <- data.all[sample(nrow(data.all), 10000), features.numeric.continuous.updated]
features.numeric.continuous.updated.cor <-  cor(Temp, use = "pairwise.complete.obs", method = "pearson")
# # library(corrplot)
# # corrplot(features.numeric.updated.cor)
names(Temp[,findCorrelation(features.numeric.continuous.updated.cor, cutoff=0.99)])

features.numeric.continuous.updated.cor <- c("v20", "v32", "v41", 
                                             "v42", "v49", "v53", 
                                             "v64", "v65", "v67", 
                                             "v73", "v118", "v26", 
                                             "v11", "v46", "v17", 
                                             "v29", "v33", "v92", 
                                             "v77", "v13", "v43", 
                                             "v83")

updateFeatures(subset(
  features.updated,
  !(
    features.updated %in% features.numeric.continuous.updated.cor
  )
))



# # #=============================================================================
# # # Data Exploration
# # #=============================================================================

# 
# TheMostImportantVars <- c("v50", "v31", "v66", "v12", 
#                           "v10", "v47", "v110", "v114",  
#                           "v40", "v34", "v21", "v56", 
#                           "v14", "v79")

data.all[, "varContNew1"] <-
  data.all[, "v50"] * data.all[, "v14"] * data.all[, "v21"]
data.all[, "varContNew2"] <- 
  data.all[, "v10"] * data.all[, "v14"] * data.all[, "v21"]
data.all[, "varContNew3"] <- 
  data.all[, "v50"] + data.all[, "v10"]
features.numeric.continuous.updated.newVars <- c("varContNew1", "varContNew2", "varContNew3")
cont.Preprocessing <-
  preProcess(data.all[, features.numeric.continuous.updated.newVars ],
             method = c("spatialSign"))
cont.Preprocessing
data.all[, features.numeric.continuous.updated.newVars] <-
  predict(cont.Preprocessing,
          data.all[, features.numeric.continuous.updated.newVars])
updateDatasets(data.all)
features.numeric.continuous.updated <-
  union(features.numeric.continuous.updated,
        features.numeric.continuous.updated.newVars)
features.numeric.updated <- union(features.numeric.updated,
                                  features.numeric.continuous.updated.newVars)
features.updated <- union(features.updated,
                          features.numeric.continuous.updated.newVars)
# data.train[, features.numeric.continuous.updated.newVars] <- 
#   subset(data.all[, features.numeric.continuous.updated.newVars], 
#          data.all$train_flag)
# data.test[, features.numeric.continuous.updated.newVars] <- 
#   subset(data.all[, features.numeric.continuous.updated.newVars], 
#          !data.all$train_flag)


# For LMT
# v50                 100.00000
# varContNew1          71.79868
# varContNew3          67.08014
# v47.numericTrans     59.37904
# v110.numericTrans    58.71310
# v66.numericTrans     58.25963
# v10                  50.62664
# v34                  46.76836
# v114                 44.70045
# varContNew2          40.59277
# v56.2.numericTrans   39.35241
# v31.numericTrans     38.95613
# v14                  37.98075
# v12                  34.33391
# v75.numericTrans     34.20847
# v107.numericTrans    33.98829
# v52.numericTrans     32.18015
# v24.numericTrans     26.78215
# v56.1.numericTrans   23.63725
# v21                  22.93650
# v56.numericTrans     19.96394
# v30.numericTrans     18.54743
# v113.2.numericTrans  16.89381
# v79.numericTrans     14.26630
# v113.1.numericTrans   0.00000


# For earth
# v50                 100.0000
# v66.numericTrans     69.1113
# v110.numericTrans    58.5734
# v56.2.numericTrans   40.4637
# v56.1.numericTrans   34.6230
# v34                  29.7921
# v113.1.numericTrans  24.0425
# v10                  17.9578
# v14                   9.9198
# v31.numericTrans      3.0463
# v30.numericTrans      0.7106
# v113.2.numericTrans   0.0000
# v47.numericTrans      0.0000

# TheMostImportantVars.Cont <- c(features.numeric.continuous.updated.newVars, 
#                                "v50", "v10", "v34", "v14", "v114", 
#                                "v21", "v12")
TheMostImportantVars.Cont <- c(features.numeric.continuous.updated.newVars, 
                               "v50", "v10", "v34", "v14")
# TheMostImportantVars.SmallCat <- c("v66.numericTrans", "v110.numericTrans", 
#                                    "v31.numericTrans", "v30.numericTrans", 
#                                    "v75.numericTrans", "v107.numericTrans",
#                                    "v52.numericTrans", "v24.numericTrans")
TheMostImportantVars.SmallCat <- c("v66.numericTrans", "v110.numericTrans", 
                                   "v31.numericTrans", "v30.numericTrans", 
                                   "v75.numericTrans")
# TheMostImportantVars.SmallCat.Dummy <- c("v66.B", "v66.C", 
#                                          "v110.B", "v110.C", 
#                                          "v31.B",  "v31.C", 
#                                          "v30.B", "v30.C", "v30.D", "v30.E", "v30.F", "v30.G",
#                                          "v75.B", "v75.D", 
#                                          "v107.D", "v107.E", "v107.F", 
#                                          "v52.B", "v52.C", "v52.D","v52.E", "v52.F", "v52.G", 
#                                          "v52.H", "v52.I", "v52.J", "v52.K", "v52.L", 
#                                          "v24.B", "v24.C", "v24.D", "v24.E")
TheMostImportantVars.SmallCat.Dummy <- c("v66.B", "v66.C", 
                                         "v110.B", "v110.C", 
                                         "v31.B",  "v31.C", 
                                         "v30.B", "v30.C", "v30.D", "v30.E", "v30.F", "v30.G",
                                         "v75.B", "v75.D")
# TheMostImportantVars.BigCat <- c("v47.numericTrans", 
#                                  "v56.numericTrans", "v56.1.numericTrans", "v56.2.numericTrans", 
#                                  "v79.numericTrans", 
#                                  "v113.1.numericTrans", 
#                                  "v113.2.numericTrans")
TheMostImportantVars.BigCat <- c("v47.numericTrans", 
                                 "v56.numericTrans", "v56.1.numericTrans", "v56.2.numericTrans",
                                 "v79.numericTrans", 
                                 "v113.1.numericTrans")


# Temp <- data.train[sample(nrow(data.train), 10000), 
#                    c(features.numeric.continuous.updated.newVars, "target")]
# Temp$target <- as.factor(Temp$target)
# 
# png(
#   filename = "ContNewVars5-target.png",
#   type = "cairo",
#   units = "cm",
#   width = 100,
#   height = 50,
#   pointsize = 11,
#   res = 200
# )
# theme_set(theme_bw())
# plot.ContVars.ggp <-
#   ggpairs(
#     Temp,
#     mapping = aes(color = target, alpha = .4),
#     upper = list(
#       continuous = "density",
#       combo = "facetdensity",
#       mapping = aes(color = target, alpha = .4)
#     ),
#     lower = list(continuous = "smooth",
#                  mapping = aes(color = target, alpha = .4))
#   ) + theme_bw()
# plot.ContVars.ggp
# dev.off()





# #Small factors
# Temp <- data.train[sample(nrow(data.train), 10000), c(features.categoric.updated.smallFactors, "target")]
# Temp$target <- as.factor(Temp$target)
# 
# png(
#   filename = "SmallFactorsVars-target.png",
#   type = "cairo",
#   units = "cm",
#   width = 100,
#   height = 50,
#   pointsize = 11,
#   res = 200
# )
# theme_set(theme_bw())
# plot.SmallFactors.ggp <-
#   ggpairs(
#     Temp,
#     mapping = aes(color = target, alpha = .4)
#   ) + theme_bw()
# plot.SmallFactors.ggp
# dev.off()
# 





# # # #=============================================================================
# # # # variables pre-processing
# # # #=============================================================================
# 
# 
# # Let's find the principal components 
# # First, get the PCs for continuous vars
features.numeric.continuous.updated.PC.Processing <-
  preProcess(data.all[, features.numeric.continuous.updated],
             method = c("pca"),
             thresh = 0.99)
features.numeric.continuous.updated.PC.Processing
# Pre-processing:
#   - centered (86)
# - ignored (0)
# - principal component signal extraction (86)
# - scaled (86)
# 
# PCA needed 32 components to capture 99 percent of the variance
features.numeric.continuous.updated.PCs <- paste("Cont_PC", c(1:32), sep = "")
data.all[, features.numeric.continuous.updated.PCs] <-
  predict(features.numeric.continuous.updated.PC.Processing,
          data.all[, features.numeric.continuous.updated])


# # Second, get the PCs for discrete (categorical variables)
features.numeric.disAndCat.updated <- union(features.numeric.discrete.updated, 
              features.categoric.updated.numericTrans)
features.numeric.disAndCat.updated.PC.Processing <-
  preProcess(data.all[, features.numeric.disAndCat.updated],
             method = c("pca"),
             thresh = 0.99)
features.numeric.disAndCat.updated.PC.Processing
# Pre-processing:
#   - centered (29)
# - ignored (0)
# - principal component signal extraction (29)
# - scaled (29)
# 
# PCA needed 24 components to capture 99 percent of the variance
features.numeric.disAndCat.updated.PCs <- paste("Disc_PC", c(1:24), sep = "")
data.all[, features.numeric.disAndCat.updated.PCs] <-
  predict(features.numeric.disAndCat.updated.PC.Processing,
          data.all[, features.numeric.disAndCat.updated])



updateDatasets(data.all)


# 
# # Using xgbtrees and fsCaret utilities I found that that the most important variables are:
# TheMostImportantVars <- c("v50", "v31", "v66", "v12", 
#                           "v10", "v47", "v110", "v114",  
#                           "v40", "v34", "v21", "v56", 
#                           "v14", "v79")
# TheMostImportantVars.Cont <-  subset(
#   TheMostImportantVars,
#   TheMostImportantVars %in% features.numeric.continuous.updated
# )
# TheMostImportantVars.Cat <-  subset(
#   TheMostImportantVars,
#   TheMostImportantVars %in% features.categoric.updated
# )
# 
# 
# # Apply expoTrans for continuous vars
# Temp <-
#   data.train[sample(nrow(data.train), 5000), TheMostImportantVars.Cont]
# features.Preprocessing <-
#   preProcess(Temp[, TheMostImportantVars.Cont],
#              method = c("expoTrans"))
# #features.Preprocessing
# TheMostImportantVars.Cont.Transformed <-
#   paste(TheMostImportantVars.Cont, "expoTrans", sep = ".")
# data.all[, TheMostImportantVars.Cont.Transformed] <-
#   predict(features.Preprocessing,
#           data.all[, TheMostImportantVars.Cont])
# # Add new numeric continuous features
# data.all["v50Mv10.expoTrans"] <- data.all[, c("v50.expoTrans")] * data.all[, c("v10.expoTrans")]
# data.all["vContM.expoTrans"] <-
#   data.all[, c("v50.expoTrans")] * data.all[, c("v10.expoTrans")] * data.all[, c("v114.expoTrans")] * data.all[, c("v14.expoTrans")]
# updateDatasets(data.all)
# # # further handly transformations on some added features based on visual analysis of these features
# # data.all["v50.expoTrans"] <-
# #   ifelse(
# #     data.all$v50.expoTrans < 1,
# #     (data.all$v50.expoTrans * 2) * -1,
# #     data.all$v50.expoTrans * 2
# #   )
# # data.all["v50Mv10.expoTrans"] <-
# #   ifelse(data.all$v50Mv10 < 1,
# #          (data.all$v50Mv10 * 2) * -1,
# #          data.all$v50Mv10 * 2)
# # data.all["vContM.expoTrans"] <-
# #   ifelse(data.all$vContM < 300,
# #          (data.all$vContM * 2) * -1,
# #          data.all$vContM * 2)
# # updateDatasets(data.all)
# 
# TheMostImportantVars.Cont.Transformed <- union(TheMostImportantVars.Cont.Transformed, c("v50Mv10.expoTrans", "vContM.expoTrans"))
# #TheMostImportantVars.Cont <- union(TheMostImportantVars.Cont, TheMostImportantVars.Cont.Transformed)
# 
# 
# # Let's have a look on transformed continuous vars
# # Temp <-
# #   data.train[sample(nrow(data.train), 2000), c("v50",
# #                                          "v50.expoTrans",
# #                                          "v10",
# #                                          "v10.expoTrans",
# #                                          "v50Mv10.expoTrans",
# #                                          "vContM.expoTrans",
# #                                          outcome)]
# # Temp[outcome] <- as.factor(Temp[, outcome])
# # 
# # png(filename="ImportantContVarsTransformed.png",
# #     type="cairo",
# #     units="cm",
# #     width=100,
# #     height=40,
# #     pointsize=11,
# #     res=300)
# # theme_set(theme_bw())
# # plot.mostImportantVar.ggp <-
# #   ggpairs(
# #     Temp,
# #     mapping = aes(color = target, alpha = .4),
# #     lower = list(
# #       continuous = "smooth",
# #       #wrap(ggp_bin, bins = 4),
# #       combo = "facetdensity",
# #       mapping = aes(color = target, alpha = .4)
# #     )
# #   ) + theme_bw()
# # plot.mostImportantVar.ggp
# # dev.off()
# 
# 
# # We perform also expoTrans on categorical variables (which were already transformed to discrete vars)
# TempCat <-
#   data.train[sample(nrow(data.train), 5000), TheMostImportantVars.Cat]
# features.Preprocessing <-
#   preProcess(TempCat[, TheMostImportantVars.Cat],
#              method = c("expoTrans"))
# #features.Preprocessing
# TheMostImportantVars.Cat.Transformed <-
#   paste(TheMostImportantVars.Cat, "expoTrans", sep = ".")
# data.all[, TheMostImportantVars.Cat.Transformed] <-
#   predict(features.Preprocessing,
#           data.all[, TheMostImportantVars.Cat])
# updateDatasets(data.all)
# 
# 
# # Let's have a look on transformed categoric vars
# # TempCat <-
# #   data.train[sample(nrow(data.train), 2000), c(
# #     "v31.expoTrans",
# #     "v66.expoTrans",
# #     "v47.expoTrans",
# #     "v110.expoTrans",
# #     "v56.expoTrans",
# #     "v79.expoTrans",
# #     outcome
# #   )]
# # TempCat[outcome] <- as.factor(TempCat[, outcome])
# # png(
# #   filename = "ImportantCatVarsTransformed.png",
# #   type = "cairo",
# #   units = "cm",
# #   width = 100,
# #   height = 40,
# #   pointsize = 11,
# #   res = 300
# # )
# # theme_set(theme_bw())
# # plot.mostImportantVar.ggp <-
# #   ggpairs(
# #     TempCat,
# #     mapping = aes(color = target, alpha = .4),
# #     lower = list(
# #       continuous = "smooth",
# #       #wrap(ggp_bin, bins = 4),
# #       combo = "facetdensity",
# #       mapping = aes(color = target, alpha = .4)
# #     )
# #   ) + theme_bw()
# # plot.mostImportantVar.ggp
# # dev.off()
# 
# 
# 
# 
# 
# TheMostImportantVars.Transformed <- union(TheMostImportantVars.Cont.Transformed, TheMostImportantVars.Cat.Transformed)
# 





#====================================================================================
#====================================================================================
#====================================================================================
#====================================================================================
# Ensemble learning
#====================================================================================
#====================================================================================
#====================================================================================
#====================================================================================
#====================================================================================



#================================================================================
# Initialization
#================================================================================
set.seed(1234)
outcome <- "target"
outcome_factor <- "outcome_factor"
data.train[outcome_factor] <-
  ifelse(data.train[, outcome] == 1, "Yes", "No")
data.train[, outcome_factor] <-
  as.factor(data.train[, outcome_factor])

currentTrainedModel <-
  NULL #tobe used in predictions functions for saving the trained model


Temp <- NULL
modelsPredictors <- NULL


#================================================================================
# Start with XGBoost
#================================================================================

library(xgboost)



xgb.boostingDefaultParameters <-
  list(
    objective           = "binary:logistic",
    booster             = "gbtree",
    eval_metric         = "logloss",
    eta                 = 0.002,
    gamma               = 0.01,
    max_depth           = 10,
    subsample           = 0.8,
    colsample_bytree    = 0.8,
    min_child_weight    = 1,
    num_parallel_tree   = 1
  )

#**************************************************************
# Define a generic xgb prediction function
#**************************************************************
xgb.predictions.do <-
  function(xgbParameters = xgb.boostingDefaultParameters,
           nbrRounds           = 5000,
           earlyStop           = 60,
           metricToMaximize    = FALSE, #this depends on the metric used in the parameter list
           xgbData             = data.train,
           training_rows       = NA,
           xgbTestingData      = data.test,
           xgbPredictors       = NA,
           outcomeVar          = outcome,
           predictions_col_name = NA, 
           saveInFile          = FALSE) {
    xgbData.Temp <- xgbData[, c(xgbPredictors, outcomeVar)]
    xgbData.Temp.y <- xgbData.Temp[, outcomeVar]
    xgbData.Temp.x <- data.matrix(xgbData.Temp[, xgbPredictors])
    xgbTrainData <- xgb.DMatrix(data = xgbData.Temp.x[training_rows, ],
                                label = xgbData.Temp.y[training_rows])
    xgbEvalData <- xgb.DMatrix(data = xgbData.Temp.x[-training_rows, ],
                               label = xgbData.Temp.y[-training_rows])
    
    xgb_Model <- xgb.train(
      params              = xgbParameters,
      data                = xgbTrainData,
      nrounds             = nbrRounds,
      verbose             = 1,
      print.every.n       = 30,
      watchlist           = list(val = xgbEvalData, train = xgbTrainData),
      early.stop.round    = earlyStop,
      maximize            = metricToMaximize,
      nthread             = 3
    )
    
    predictions_column <-
      ifelse(is.na(predictions_col_name),
             "Pre_xgb",
             predictions_col_name)
    
    xgb_Model.predections.train <- predict(xgb_Model,
                                           data.matrix(xgbData[, xgbPredictors]),
                                           ntreelimit = xgb_Model$bestInd)
    xgb_Model.predections.test <- predict(xgb_Model,
                                          data.matrix(xgbTestingData[, xgbPredictors]),
                                          ntreelimit = xgb_Model$bestInd)
    data.train[predictions_column] <<- xgb_Model.predections.train
    data.test[predictions_column] <<- xgb_Model.predections.test
    #============================================================
    
    if(saveInFile) {
      cat("\nSave prediction probabilities on test dataset in a separated file\n")
      predictionsFile <-
        data.frame(ID = xgbTestingData$ID, PredictedProb = xgb_Model.predections.test)
      write.csv(
        predictionsFile,
        file = paste("predictionsFile_", predictions_column, ".csv", sep = ""),
        row.names = FALSE
      )
    }
    
    currentTrainedModel <<- xgb_Model
    modelsPredictors <<- union(modelsPredictors, predictions_column)
    
    rm(xgb_Model)
    gc()
  }
#================================================================================
# End XGBoost predictions function
#================================================================================



#================================================================================
# New function to get the average prediction values of xgb using several prediction iterations
#================================================================================
xgb.predictions.iter <- function(predictors,
                                 niter = 10,
                                 ensemble_name,
                                 training_rows_list = NA) {
  xgb_Predictions_Ensemble.test <- rep(0, nrow(data.test))
  xgb_Predictions_Ensemble.train <- rep(0, nrow(data.train))
  for (i in 1:niter) {
    set.seed(i + 12345)
    if (is.na(training_rows_list)) {
      training_rows <- sample(nrow(data.train), 10000)
    } else{
      training_rows <- training_rows_list[[i]]
    }
    col_name = paste(ensemble_name, i, sep = "_")
    
    xgb.predictions.do(
      xgbPredictors = predictors,
      predictions_col_name = col_name,
      xgbParameters = xgb_parameters,
      training_rows = training_rows,
      nbrRounds = 100000,
      earlyStop = 400
    )
    
    xgb_Predictions_Ensemble.test <-
      xgb_Predictions_Ensemble.test + data.test[, col_name]
    xgb_Predictions_Ensemble.train <-
      xgb_Predictions_Ensemble.train + data.train[, col_name]
  } # End of iterations
  
  cat(
    "Save the average of prediction probabilities produced along all iterations on test dataset in a separated file\n"
  )
  file_name = paste(ensemble_name, "avg", sep = "_")
  
  data.test[file_name] <<- xgb_Predictions_Ensemble.test / niter
  data.train[file_name] <<- xgb_Predictions_Ensemble.train / niter
  modelsPredictors <<- union(modelsPredictors, file_name)
  
  predictionsFile <- data.frame(ID = data.test$ID,
                                PredictedProb = xgb_Predictions_Ensemble.test /
                                  niter)
  write.csv(
    predictionsFile,
    file = paste(file_name, ".csv", sep = ""),
    row.names = FALSE
  )
  
  return(xgb_Predictions_Ensemble.test / niter)
}
#================================================================================
# End function xgb.predictions.iter
#================================================================================



#================================================================================
# Start with CARET models
#================================================================================
defaultCaretControl <- trainControl(method='repeatedcv', 
                                    number=2,  repeats = 2, 
                                    returnResamp='none',  
                                    classProbs = TRUE,
                                    summaryFunction = twoClassSummary, 
                                    savePredictions = TRUE,
                                    allowParallel = TRUE,
                                    verboseIter=TRUE)


#**************************************************************
# Define a generic caret prediction function
#**************************************************************
caret.predictions.do <- function(prediction_model = "gbm", # = method for caret.train
                                 predictors,
                                 whatOutcome = NA, 
                                 training_rows = NA,
                                 predictions_col_name = NA,
                                 saveInFile = FALSE,
                                 ...) {
  useOutcome <- whatOutcome
  if(is.na(useOutcome)) {
    useOutcome <- outcome_factor
  }
  data.Temp <- data.train[, c(predictors, useOutcome)]
  #===============================================
  if (is.na(predictions_col_name)) {
    predictions_column <- paste("Pre", prediction_model, sep = "_")
  } else{
    predictions_column <- predictions_col_name
  }
  
  cat("Creating internal training and testing datasets\n")
  data.Temp.train <- data.Temp[training_rows, ]
  data.Temp.test  <- data.Temp[-training_rows, ]
  
  cat(paste("Training a ((", prediction_model, ")) model\n", sep = ""))
  cat("==========================================\n")
  # if (is.na(tuneGrid)) {
  #   model <- train(
  #     data.Temp.train[, predictors],
  #     data.Temp.train[, useOutcome],
  #     method = prediction_model,
  #     metric = "ROC",
  #     ...
  #   )
  # } else{
    model <- train(
      data.Temp.train[, predictors],
      data.Temp.train[, useOutcome],
      method = prediction_model,
      metric = "logLoss",  #"ROC",
      ...
    )
  # }
  #============================================================
  
  cat("Evaluation on data.Temp.test using AUC\n")
  cat("==========================================\n")
  model.predictions.eval <- predict(object = model,
                                    data.Temp.test[, predictors],
                                    type = 'raw')
  print(postResample(pred = model.predictions.eval,
                     obs = data.Temp.test[, useOutcome]))
  model.predictions.eval.prob <- predict(object = model,
                                         data.Temp.test[, predictors],
                                         type = 'prob')
  model.predictions.eval.prob <- model.predictions.eval.prob[[2]]
  #To get the AUC score, you need to pass the yes column to the roc function (each row adds up to 1 but we're interested in the yes, the survivors):
  auc <- roc(ifelse(data.Temp.test[, useOutcome] == "Yes", 1, 0),
             model.predictions.eval.prob)
  print(auc$auc)
  #============================================================
  
  cat("Getting prediction probabilities on train and test datasets\n")
  cat("==========================================\n")
  model.predictions.train <- predict(object = model,
                                     data.train[, predictors],
                                     type = 'prob')
  model.predictions.train <- model.predictions.train[[2]]
  data.train[predictions_column] <<- model.predictions.train
  model.predictions.test <- predict(object = model,
                                    data.test[, predictors],
                                    type = 'prob')
  model.predictions.test <- model.predictions.test[[2]]
  data.test[predictions_column] <<- model.predictions.test
  
  modelsPredictors <<- union(modelsPredictors, predictions_column)
  
  print(
    paste(
      "A new column ((",
      predictions_column,
      ")) was added to data.train and data.test",
      sep = ""
    )
  )
  #============================================================
  
  if(saveInFile) {
    cat("Save prediction probabilities on test dataset in a separated file\n")
    predictionsFile <-
      data.frame(ID = data.test$ID, PredictedProb = model.predictions.test)
    write.csv(
      predictionsFile,
      file = paste("predictionsFile_", predictions_column, ".csv", sep = ""),
      row.names = FALSE
    )
  }
  
  currentTrainedModel <<- model
  
  rm(model)
  gc()
}
#==============================================================================
# End Caret predictions functions
#==============================================================================



#==============================================================================
# Utilities
#==============================================================================

discretized_predictionsProb <- NULL

#####################
predictionsProb_3bins <- function(dataset,
                                  predictionsProbCol,
                                  newCol = NA,
                                  yesThreshold = 1,
                                  noThreshold = 0) {
  newColName <- newCol
  if (is.na(newColName)) {
    newColName <- paste(predictionsProbCol, "disc", sep = "_")
  }
  df <- dataset
  df[, newColName] <-
    ifelse(df[, predictionsProbCol] >= yesThreshold,
           1,
           ifelse(df[, predictionsProbCol] > noThreshold,
                  0.5,
                  0))
  discretized_predictionsProb <- union(discretized_predictionsProb, newColName)
  return(df)
}

#####################
discretize_predictionsProb <- function(...){
  data.train <<- predictionsProb_3bins(dataset = data.train, ...)
  data.test <<- predictionsProb_3bins(dataset = data.test, ...)
}

#####################
compute_average_predictionsProb <- function(dataset,
                                          predictionsProb = modelsPredictors,
                                          newCol = "Avg_Predictions") {
  df <- dataset
  sum_predictions <- rowSums(df[, predictionsProb])
  df[, newColName] <-  sum_predictions / length(predictionsProb)
  return(df)
}

#####################
get_averge_predictionsProb <- function(...){
  data.train <<- compute_average_predictionsProb(dataset = data.train, ...)
  data.test <<- compute_average_predictionsProb(dataset = data.test, ...)
}


#####################
get_averge_discretized_predictionsProb <- function(predictionsProb = discretized_predictionsProb,
                                                newCol = "Avg_DiscPredictions"){
  data.train <<- compute_average_predictionsProb(dataset = data.train, predictionsProb = predictionsProb, newCol = newCol)
  data.test <<- compute_average_predictionsProb(dataset = data.test, predictionsProb = predictionsProb, newCol = newCol)
}



####################
get_specific_trainingRows <-
  function(previousDiscPredictions = "Avg_DiscPredictions",
           maxSize = 15000) {
    zeroTargetRows <- which(data.train$target == 0)
    oneTargetRows <- which(data.train$target == 1)
    badPredictionsRows <-
      which(data.train[previousDiscPredictions] == 0.5)
    notbadPredictionsRows <-
      which(data.train[previousDiscPredictions] != 0.5)
    selectedRows <- NULL
    if (length(badPredictionsRows) > 1) {
      badPredictions_zeroTarget_Rows <-
        subset(badPredictionsRows,
               badPredictionsRows %in% zeroTargetRows)
      badPredictions_oneTarget_Rows <-
        subset(badPredictionsRows, badPredictionsRows %in% oneTargetRows)
      #badPredictions_zeroTarget_l <- length(badPredictions_zeroTarget_Rows)
      if (length(badPredictions_zeroTarget_Rows) > 0.5 * maxSize) {
        #selected_badPredictions_zeroTarget_l <- 0.5 * maxSize
        selectedRows <-
          sample(badPredictions_zeroTarget_Rows, 0.5 * maxSize)
      } else{
        #selected_badPredictions_zeroTarget_l <- badPredictions_zeroTarget_l
        selectedRows <- badPredictions_zeroTarget_Rows
      }
      selectedRows <- c(selectedRows,
                        sample(badPredictions_oneTarget_Rows,
                               0.25 * maxSize))
    }
    selectedRows <- c(selectedRows,
                      sample(notbadPredictionsRows,
                             maxSize - length(selectedRows)))
    return(selectedRows)
  }



####################
get_specific_trainingRows1 <-
  function(previousDiscPredictions = "Avg_DiscPredictions",
           maxSize = 16000) {
    badPredictionsRows <-
      which(data.train[previousDiscPredictions] == 0.5)
    onePredictionsRows <-
      which(data.train[previousDiscPredictions] == 1)
    zeroPredictionsRows <-
      which(data.train[previousDiscPredictions] == 0)
    #
    badPredictionsRows_l <- length(badPredictionsRows)
    zeroPredictionsRows_l <- length(zeroPredictionsRows)
    if (is.na(maxSize) || maxSize > (2 * badPredictionsRows_l)) {
      maxSize <- 2 * badPredictionsRows_l
    }
    if (badPredictionsRows_l > 0.5 * maxSize) {
      badPredictionsRows_l <- 0.5 * maxSize
    }
    if (zeroPredictionsRows_l > 0.25 * maxSize) {
      zeroPredictionsRows_l <- 0.25 * maxSize
    }
    onePredictionsRows_l <- maxSize - (badPredictionsRows_l + zeroPredictionsRows_l)
    #
    r1 <- sample(badPredictionsRows, badPredictionsRows_l)
    r2 <- sample(zeroPredictionsRows, zeroPredictionsRows_l)
    r3 <- sample(onePredictionsRows, onePredictionsRows_l)
    selectedRows <- c(r1, r2, r3)
    return(selectedRows)
  }



####################
get_specific_trainingRows_basedOutcome <-
  function(outcomeVar = "target",
           zeroOutcomeRatio = 0.5,
           useSubRows = NULL, 
           maxSize = NA) {
    if (is.null(useSubRows)){
      useSubRows <- c(1:nrow(data.train))
    }
    zeroOutcomeRows <- which(data.train[useSubRows, outcomeVar] == 0)
    oneOutcomeRows <- which(data.train[useSubRows, outcomeVar] == 1)
    zeroOutcomeRows_l <- length(zeroOutcomeRows)
    selectedRows <- NULL
    if (is.na(maxSize)) {
      maxSize <- (1 / zeroOutcomeRatio) * zeroOutcomeRows_l
    }
    maxSize <- min(maxSize, length(useSubRows))
    if (zeroOutcomeRows_l <= (zeroOutcomeRatio * maxSize)) {
      selectedRows <- zeroOutcomeRows
    } else{
      selectedRows <- sample(zeroOutcomeRows, zeroOutcomeRatio * maxSize)
    }
    zeroOutcomeRatio_actuel <- length(selectedRows) / maxSize
    oneOutcomeRatio <-
      ((1 - zeroOutcomeRatio) * zeroOutcomeRatio_actuel) / zeroOutcomeRatio
    selectedRows <- c(selectedRows,
                      sample(oneOutcomeRows, oneOutcomeRatio * maxSize))
    return(selectedRows)
  }


##########################
get_specific_trainingRows_2steps <-
  function(previousDiscPredictions = "Avg_DiscPredictions",
           maxSize = 16000,
           outcomeVar = "target",
           zeroOutcomeRatio = 0.5) {
    subrows <-
      get_specific_trainingRows1(previousDiscPredictions = previousDiscPredictions,
                                 maxSize = maxSize)
    selectedRows <-
      get_specific_trainingRows_basedOutcome(
        outcomeVar = outcomeVar,
        zeroOutcomeRatio = zeroOutcomeRatio,
        useSubRows = subrows,
        maxSize = maxSize
      )
    return(selectedRows)
  }
    

####################
median.quartile <- function(x){
  out <- round(quantile(x, probs = c(0.25,0.5,0.75)), 3)
  names(out) <- c("ymin","y","ymax")
  return(out) 
}


####################
plot_predictionsProb <- function(predictionsProb){
  theme_set(theme_bw())
  gg <- ggplot(data.train[,c(predictionsProb, "target")], 
               aes(x = factor(target), y=data.train[, predictionsProb], group=target)
  ) +
    geom_boxplot(aes(fill= factor(target))) + 
    scale_fill_grey() +
    scale_y_continuous(limits= c(0,1), breaks= seq(0, 1, by= 0.1))
  gg <- gg + stat_summary(aes(label=..y..), 
                          fun.y= median.quartile, 
                          geom= "text", 
                          size= 6,
                          colour="red2"
  )
  gg <- gg + ylab(predictionsProb) + xlab("target value")
  gg
  return(gg)
}


####################
plot_discretized_predictionsProb <- function(discretized_predictionsProb){
  theme_set(theme_bw())
  gg <- ggplot(data.train[,c(discretized_predictionsProb, "target")], 
               aes(factor(data.train[, discretized_predictionsProb]), fill = factor(target))
  ) +
    geom_bar(aes( y = (..count..)/sum(..count..) )) + 
    scale_fill_grey()
  gg <- gg + ylab("Frequency") + xlab(predictionsProb)
  gg
  return(gg)
}




#==============================================================================
# End Utilities
#==============================================================================




#==============================================================================
# Start by using caret to train different models
#==============================================================================

# as training data, we will use only 5000 rows 
caret.trainingDataLength <- 15000
# these rows are each time randomly selected from 1/2 data.train rows

# for all caret models, we use the following trainControl
myControl <- trainControl(method='cv', 
                          number=4,  
                          returnResamp='final',  
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary, 
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)



##########################
# gbm
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat.Dummy,
    TheMostImportantVars.BigCat
  )
training_rows <-
  sample(nrow(data.train) / 2, caret.trainingDataLength)
gbmgrid <-  expand.grid(
  interaction.depth = 3, #c(3, 4, 5), 4 also gives sometimes slightly better results
  #c(4, 6, 8),
  n.trees = 800,
  shrinkage = 0.075, #seq(0.075, 0.2, length.out = 3), also tried 0.05
  n.minobsinnode = 150 #c(100, 150, 250) #c(0.01, 0.05, 0.10)  * caret.trainingDataLength and 0.05 was the best
)
caret.predictions.do(
  prediction_model = "gbm",
  predictors = predictors,
  predictions_col_name = "gbm_MostImpVars",
  training_rows = training_rows,
  trControl = myControl,
  preProc = c("YeoJohnson"),
  tuneGrid = gbmgrid
)
# Fitting n.trees = 800, interaction.depth = 3, shrinkage = 0.075, n.minobsinnode = 150
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7776905 0.2099667 
# Area under the curve: 0.7367

plot_predictionsProb("gbm_MostImpVars")


x <- nrow(subset(data.train, data.train$gbm_MostImpVars < 0.8 & data.train$gbm_MostImpVars > 0.69))
x/nrow(data.train)
# [1] 0.23

data.train[, "gbm_MostImpVars1_disc"] <- ifelse(
  data.train[, "gbm_MostImpVars"] > 0.8, 
  1, #YES
  ifelse(data.train[, "gbm_MostImpVars"] > 0.69, 
         0.5, #DON'T KNOW
         0) #NO
)

data.test[, "gbm_MostImpVars1_disc"] <- ifelse(
  data.test[, "gbm_MostImpVars"] > 0.8, 
  1, #YES
  ifelse(data.test[, "gbm_MostImpVars"] > 0.69, 
         0.5, #DON'T KNOW
         0) #NO
)
edit(data.train[sample(nrow(data.train), 100), c("gbm_MostImpVars1_disc", "target")])
Temp <- subset(data.train, data.train$gbm_MostImpVars1_disc==0.5)
sort(table(Temp$target)/nrow(data.train))




##########################
# gamLoess
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <-
  sample(which(data.train$gbm_MostImpVars1_disc == 0.5) ,
         caret.trainingDataLength/3)
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_MostImpVars1_disc == 1) ,
                         caret.trainingDataLength / 15
                       ))
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_MostImpVars1_disc == 0) ,
                         caret.trainingDataLength / 15
                       ))
gamLoessgrid <-   expand.grid(span = 0.3, degree = 1)
  # expand.grid(span = seq(0.3, 0.5, by = 0.1),
  #                            degree = 1)
myControl <- trainControl(method='cv',
                          number=3,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
caret.predictions.do(
  prediction_model = "gamLoess",
  predictors = predictors,
  predictions_col_name = "gamLoess_MostImpVars",
  training_rows = training_rows,
  preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = gamLoessgrid
)
# Fitting span = 0.3, degree = 1 on full training set
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7733435 0.1599114 
# Area under the curve: 0.7242

plot_predictionsProb("gamLoess_MostImpVars")




##########################
# gcvEarth
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
# predictors <-
#   c(
#     features.numeric.continuous.updated.PCs,
#     features.numeric.disAndCat.updated.PCs
#   )
# predictors <- c("Disc_PC19", "Disc_PC22" , "Disc_PC15", "Cont_PC10", 
#                 "Disc_PC4", "Cont_PC12", "Cont_PC2", "Disc_PC8", 
#                 "Disc_PC17", "Disc_PC5",  "Cont_PC17", "Cont_PC18", 
#                 "Cont_PC26", "Disc_PC18", "Disc_PC1", "Cont_PC21", 
#                 "Disc_PC13", "Cont_PC1", "Cont_PC22", "Cont_PC15"
#                 )
training_rows <-
  sample(which(data.train$gbm_gamLoess_MostImpVars1_disc == 0.5) ,
         10000)
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 1) ,
                         3000
                       ))
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 0) ,
                        3000
                      ))
myControl <- trainControl(method='cv',
                          number=5,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
gcvEarthgrid <-  expand.grid(degree = 3) #expand.grid(degree = c(1, 2, 3, 4, 5))
caret.predictions.do(
  prediction_model = "gcvEarth",
  predictors = predictors,
  predictions_col_name = "gcvEarth_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = gcvEarthgrid
)
# Fitting degree = 3 on full training set
# Evaluation on data.Temp.test using AUC
# ==========================================
# Accuracy     Kappa 
# 0.7733038 0.1740488 
# Area under the curve: 0.7378

plot_predictionsProb("gcvEarth_MostImpVars")



##########################
# earth
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <-
  sample(which(data.train$gbm_gamLoess_MostImpVars1_disc == 0.5) ,
         10000)
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 1) ,
                         3000
                       ))
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 0) ,
                         3000
                       ))
myControl <- trainControl(method='cv',
                          number=5,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
earthgrid <- expand.grid(nprune= 20, degree= 2)
  #expand.grid(nprune= seq(10, 50, length.out = 5), degree= c(1, 2, 3))
caret.predictions.do(
  prediction_model = "earth",
  predictors = predictors,
  predictions_col_name = "earth_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = earthgrid
)
# Fitting nprune = 20, degree = 2 on full training set
# Evaluation on data.Temp.test using AUC
# ==========================================
# Accuracy     Kappa 
# 0.7754091 0.2004051 
# Area under the curve: 0.7422

plot_predictionsProb("earth_MostImpVars")


##########################
# bagFDA
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <-
  sample(which(data.train$gbm_gamLoess_MostImpVars1_disc == 0.5) ,
         10000)
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 1) ,
                         3000
                       ))
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 0) ,
                         3000
                       ))
myControl <- trainControl(method='cv',
                          number=5,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
bagfdagrid <- expand.grid(nprune= 70, degree= 2)
              #expand.grid(nprune= c(10, 20, 30, 40, 50, 70, 90), degree= c(1, 2, 3))
caret.predictions.do(
  prediction_model = "bagFDA",
  predictors = predictors,
  predictions_col_name = "bagFDA_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = bagfdagrid
)
# Fitting degree = 2, nprune = 70 on full training set
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7755108 0.2200562 
# Area under the curve: 0.7427

plot_predictionsProb("bagFDA_MostImpVars")


##########################
# bagEarthGCV
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <-
  sample(which(data.train$gbm_gamLoess_MostImpVars1_disc == 0.5) ,
         10000)
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 1) ,
                         3000
                       ))
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 0) ,
                         3000
                       ))
bagEarthGCVgrid <- expand.grid(degree= 2)
  # expand.grid(degree= c(1, 2, 3))
caret.predictions.do(
  prediction_model = "bagEarthGCV",
  predictors = predictors,
  predictions_col_name = "bagEarthGCV_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = bagEarthGCVgrid
)
# Fitting degree = 2 on full training set
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7758363 0.2032956 
# Area under the curve: 0.7452

plot_predictionsProb("bagEarthGCV_MostImpVars")

# bagEarthGCV variable importance
# 
# Overall
# v50                 100.0000
# v66.numericTrans     69.1113
# v110.numericTrans    58.5734
# v56.2.numericTrans   40.4637
# v56.1.numericTrans   34.6230
# v34                  29.7921
# v113.1.numericTrans  24.0425
# v10                  17.9578
# v14                   9.9198
# v31.numericTrans      3.0463
# v30.numericTrans      0.7106
# v113.2.numericTrans   0.0000
# v47.numericTrans      0.0000




##########################
# bagEarthGCV new
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <- get_specific_trainingRows1("gbm_gamLoess_MostImpVars1_disc", 16000)
bagEarthGCVgrid <- expand.grid(degree= 2)
# expand.grid(degree= c(1, 2, 3))
caret.predictions.do(
  prediction_model = "bagEarthGCV",
  predictors = predictors,
  predictions_col_name = "bagEarthGCV_new_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = bagEarthGCVgrid
)
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7775857 0.2007370 
# Area under the curve: 0.7431

plot_predictionsProb("bagEarthGCV_new_MostImpVars")

# v50                 100.000
# v110.numericTrans    84.164
# v66.numericTrans     75.944
# v34                  50.925
# v31.numericTrans     41.346
# varContNew1          34.583
# v113.1.numericTrans  30.358
# v56.numericTrans     22.465
# v79.numericTrans     15.556
# varContNew2           8.765
# varContNew3           4.669
# v10                   1.090
# v14                   0.151
# v56.2.numericTrans    0.000
# v47.numericTrans      0.000
# v30.numericTrans      0.000
# v75.numericTrans      0.000
# v56.1.numericTrans    0.000

##########################
# LMT
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <-
  sample(which(data.train$gbm_gamLoess_MostImpVars1_disc == 0.5) ,
         10000)
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 1) ,
                         3000
                       ))
training_rows <- union(training_rows,
                       sample(
                         which(data.train$gbm_gamLoess_MostImpVars1_disc == 0) ,
                         3000
                       ))
myControl <- trainControl(method='cv',
                          number=5,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
LMTgrid <- expand.grid(iter = 50)
  # expand.grid(iter = c(50, 100))
caret.predictions.do(
  prediction_model = "LMT",
  predictors = predictors,
  predictions_col_name = "LMT_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = LMTgrid
)
# Fitting iter = 50 on full training set
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7682489 0.2220741 
# Area under the curve: 0.7306

plot_predictionsProb("LMT_MostImpVars")

discretize_predictionsProb(predictionsProbCol = "LMT_MostImpVars", 
                           yesThreshold = 0.82,
                           noThreshold = 0.72)

plot_discretized_predictionsProb("LMT_MostImpVars_disc")




##########################
# LMT new
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <- get_specific_trainingRows1("gbm_gamLoess_MostImpVars1_disc", 15000)
myControl <- trainControl(method='cv',
                          number=5,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
LMTgrid <- expand.grid(iter = 50)
# expand.grid(iter = c(50, 100))
caret.predictions.do(
  prediction_model = "LMT",
  predictors = predictors,
  predictions_col_name = "LMT_new_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = LMTgrid
)
# Evaluation on data.Temp.test using AUC
# ==========================================
#  Accuracy     Kappa 
# 0.7837315 0.2275590 
# Area under the curve: 0.7194
varImportance <- varImp(currentTrainedModel)
varImportance.imp <- varImportance$importance
varImportance.imp <- varImportance.imp[ order(-varImportance.imp[, "Yes"]),]
varImportance.imp[, "Yes", drop=F]
# v50                 100.00000
# varContNew1          71.79868
# varContNew3          67.08014
# v47.numericTrans     59.37904
# v110.numericTrans    58.71310
# v66.numericTrans     58.25963
# v10                  50.62664
# v34                  46.76836
# v114                 44.70045
# varContNew2          40.59277
# v56.2.numericTrans   39.35241
# v31.numericTrans     38.95613
# v14                  37.98075
# v12                  34.33391
# v75.numericTrans     34.20847
# v107.numericTrans    33.98829
# v52.numericTrans     32.18015
# v24.numericTrans     26.78215
# v56.1.numericTrans   23.63725
# v21                  22.93650
# v56.numericTrans     19.96394
# v30.numericTrans     18.54743
# v113.2.numericTrans  16.89381
# v79.numericTrans     14.26630
# v113.1.numericTrans   0.00000

plot_predictionsProb("LMT_new_MostImpVars")


discretize_predictionsProb(predictionsProbCol = "LMT_new_MostImpVars", 
                           yesThreshold = 0.69,
                           noThreshold = 0.60)
head(data.train[, "LMT_new_MostImpVars_disc"])

plot_discretized_predictionsProb("LMT_new_MostImpVars_disc")






##########################
# treebag
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat.Dummy,
    TheMostImportantVars.BigCat
  )
training_rows <- get_specific_trainingRows1("gbm_gamLoess_MostImpVars1_disc", 16000)
myControl <- trainControl(method='cv',
                          number=5,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
#bagEarthGCVgrid <- expand.grid(degree= 2)
# expand.grid(degree= c(1, 2, 3))
caret.predictions.do(
  prediction_model = "treebag",
  predictors = predictors,
  predictions_col_name = "treebag_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl
)
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7610378 0.2056343 
# Area under the curve: 0.697

plot_predictionsProb("treebag_MostImpVars")

discretize_predictionsProb(predictionsProbCol = "treebag_MostImpVars", 
                           yesThreshold = 0.81,
                           noThreshold = 0.71)

plot_discretized_predictionsProb("treebag_MostImpVars_disc")


# Overall
# v50                 100.000
# varContNew1          94.112
# varContNew3          92.609
# varContNew2          90.925
# v10                  87.531
# v34                  83.000
# v14                  74.693
# v56.numericTrans     31.817
# v56.2.numericTrans   25.625
# v113.1.numericTrans  22.923
# v79.numericTrans     17.639
# v47.numericTrans     15.518
# v56.1.numericTrans   15.069
# v30.C                13.294
# v66.B                10.577
# v30.D                 9.551
# v30.B                 8.956
# v75.B                 8.872
# v75.D                 7.121
# v110.B                7.102




##########################
# xgbTree
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat.Dummy,
    TheMostImportantVars.BigCat
  )
training_rows <- get_specific_trainingRows1("gbm_gamLoess_MostImpVars1_disc", 16000)
myControl <- trainControl(method='cv',
                          number=2,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
xgbTreegrid <- expand.grid(nrounds = 1200, max_depth = 8, #c(4, 6, 8, 10), 
                        eta = 0.003, gamma= 0.005, #c(0.005, 0.01), 
                        colsample_bytree = 0.8, #c(0.5, 0.8, 1), 
                        min_child_weight = 20 #c(20, 40, 80, 120)
                        )
caret.predictions.do(
  prediction_model = "xgbTree",
  predictors = predictors,
  predictions_col_name = "xgbTree_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = xgbTreegrid
)
# Fitting nrounds = 1200, max_depth = 8, 
# eta = 0.003, gamma = 0.005, colsample_bytree = 0.8, min_child_weight = 20 
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7794571 0.2033423 
# Area under the curve: 0.7441



plot_predictionsProb("xgbTree_MostImpVars")

discretize_predictionsProb(predictionsProbCol = "xgbTree_MostImpVars", 
                           yesThreshold = 0.77,
                           noThreshold = 0.70)

plot_discretized_predictionsProb("xgbTree_MostImpVars_disc")





##########################
# xgbTree new
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat.Dummy,
    TheMostImportantVars.BigCat
  )
# training_rows <- get_specific_trainingRows1("gbm_gamLoess_MostImpVars1_disc", 16000)
training_rows <- get_specific_trainingRows_basedOutcome(zeroOutcomeRatio = 0.4, maxSize = 16000)
# training_rows <- get_specific_trainingRows_2steps(
#   previousDiscPredictions = "gbm_gamLoess_MostImpVars1_disc",
#   zeroOutcomeRatio = 0.4,
#   maxSize = 16000
# )
myControl <- trainControl(method='cv',
                          number=3,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
xgbTreegrid <- expand.grid(nrounds = 3000, max_depth = 8, #c(4, 6, 8, 10), 
                           eta = 0.003, gamma= 0.005, #c(0.005, 0.01), 
                           colsample_bytree = 0.8, #c(0.5, 0.8, 1), 
                           min_child_weight = min(0.01 * length(training_rows), 100)  #c(10, 20, 30, 40, 80, 120) 30 gives the best
)
caret.predictions.do(
  prediction_model = "xgbTree",
  predictors = predictors,
  predictions_col_name = "xgbTree_new_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = xgbTreegrid
)
# Evaluation on data.Temp.test using AUC
# ==========================================
# Accuracy     Kappa 
# 0.7399437 0.2826690 
# Area under the curve: 0.7412

plot_predictionsProb("xgbTree_new_MostImpVars")

discretize_predictionsProb(predictionsProbCol = "xgbTree_new_MostImpVars", 
                           yesThreshold = 0.625,
                           noThreshold = 0.354)

plot_discretized_predictionsProb("xgbTree_new_MostImpVars_disc")





##########################
# xgbTree new new
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat.Dummy,
    TheMostImportantVars.BigCat
  )
# training_rows <- get_specific_trainingRows1("gbm_gamLoess_MostImpVars1_disc", 16000)
training_rows <- get_specific_trainingRows_basedOutcome(zeroOutcomeRatio = 0.4, maxSize = 40000)
# training_rows <- get_specific_trainingRows_2steps(
#   previousDiscPredictions = "gbm_gamLoess_MostImpVars1_disc",
#   zeroOutcomeRatio = 0.4,
#   maxSize = 16000
# )
myControl <- trainControl(method='cv',
                          number=3,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
xgbTreegrid <- expand.grid(nrounds = 3000, max_depth = 8, #c(4, 6, 8, 10), 
                           eta = 0.003, gamma= 0.005, #c(0.005, 0.01), 
                           colsample_bytree = 0.8, #c(0.5, 0.8, 1), 
                           min_child_weight = min(0.01 * length(training_rows), 50)  #c(10, 20, 30, 40, 80, 120) 30 gives the best
)
caret.predictions.do(
  prediction_model = "xgbTree",
  predictors = predictors,
  predictions_col_name = "xgbTree_new_new_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = xgbTreegrid
)
# Evaluation on data.Temp.test using AUC
# ==========================================
# Accuracy     Kappa 
# 0.7633643 0.2562880 
# Area under the curve: 0.7469

plot_predictionsProb("xgbTree_new_new_MostImpVars")

discretize_predictionsProb(predictionsProbCol = "xgbTree_new_new_MostImpVars", 
                           yesThreshold = 0.585,
                           noThreshold = 0.397)

plot_discretized_predictionsProb("xgbTree_new_new_MostImpVars_disc")




##########################
# LMT new new
##########################
predictors <-
  c(
    "xgbTree_new_new_MostImpVars_disc",
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <- get_specific_trainingRows_basedOutcome(zeroOutcomeRatio = 0.4, maxSize = 40000)
# training_rows <- get_specific_trainingRows1("xgbTree_new_new_MostImpVars_disc", 20000)
myControl <- trainControl(method='cv',
                          number=5,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
LMTgrid <- expand.grid(iter = 50)
# expand.grid(iter = c(50, 100))
caret.predictions.do(
  prediction_model = "LMT",
  predictors = predictors,
  predictions_col_name = "LMT_new_new_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = LMTgrid
)
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7821058 0.1848573 
# Area under the curve: 0.7482
#
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7683966 0.2604822 
# Area under the curve: 0.7356

plot_predictionsProb("LMT_new_new_MostImpVars")

discretize_predictionsProb(predictionsProbCol = "LMT_new_new_MostImpVars", 
                           yesThreshold = 0.625,
                           noThreshold = 0.354)

plot_discretized_predictionsProb("LMT_new_new_MostImpVars_disc")






##########################
# LMT new new new
##########################
predictors <-
  c(
    TheMostImportantVars.Cont,
    TheMostImportantVars.SmallCat,
    TheMostImportantVars.BigCat
  )
training_rows <- get_specific_trainingRows_basedOutcome(zeroOutcomeRatio = 0.4, maxSize = 40000)
# training_rows <- get_specific_trainingRows1("xgbTree_new_new_MostImpVars_disc", 20000)
myControl <- trainControl(method='cv',
                          number=5,
                          returnResamp='final',
                          classProbs = TRUE,
                          summaryFunction = multiClassSummary,
                          savePredictions = TRUE,
                          allowParallel = TRUE,
                          verboseIter=TRUE)
LMTgrid <- expand.grid(iter = 50)
# expand.grid(iter = c(50, 100))
caret.predictions.do(
  prediction_model = "LMT",
  predictors = predictors,
  predictions_col_name = "LMT_new_new_new_MostImpVars",
  training_rows = training_rows,
  #preProc = c("expoTrans"),
  trControl = myControl,
  tuneGrid = LMTgrid
)
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy    Kappa 
# 0.765275 0.242772 
# Area under the curve: 0.7372

plot_predictionsProb("LMT_new_new_new_MostImpVars")

discretize_predictionsProb(predictionsProbCol = "LMT_new_new_new_MostImpVars", 
                           yesThreshold = 0.632,
                           noThreshold = 0.547)

plot_discretized_predictionsProb("LMT_new_new_new_MostImpVars_disc")




###########################
# xgboost
###########################
# predictors <-
#   c(
#     TheMostImportantVars.Cont,
#     TheMostImportantVars.SmallCat,
#     TheMostImportantVars.BigCat
#   )
predictors <-
  c(
    features.numeric.continuous.updated,
    features.categoric.updated.numericTrans,
    features.numeric.discrete.updated,
    "gbm_MostImpVars1_disc", "gamLoess_MostImpVars1_disc"
  )
# predictors <-
#   c(
#     features.numeric.continuous.updated.PCs,
#     features.numeric.disAndCat.updated.PCs
#   )
#
training_rows <-
  sample(which(data.train$gbm_gamLoess_MostImpVars1_disc == 0.5) ,
         10000)
training_rows <- union(training_rows,
                       sample(which(
                         data.train$gbm_gamLoess_MostImpVars1_disc == 1
                       ) ,
                       2000))
training_rows <- union(training_rows,
                       sample(which(
                         data.train$gbm_gamLoess_MostImpVars1_disc == 0
                       ) ,
                       2000))
#
xgb_parameters <- list(
  objective           = "binary:logistic",
  booster             = "gbtree",
  eval_metric         = "logloss",
  eta                 = 0.005,
  gamma               = 0.007,
  max_depth           = 7,
  subsample           = 0.8,
  colsample_bytree    = 30 / length(predictors),
  min_child_weight    = 50, #0.01 * length(training_rows),
  max_delta_step      = 1,
  tree_method         = "approx",
  sketch_eps          = 0.005
)
#
xgb.predictions.do(
  xgbParameters = xgb_parameters,
  nbrRounds           = 10000,
  earlyStop           = 300,
  training_rows       = training_rows,
  xgbPredictors       = predictors,
  outcomeVar          = outcome,
  predictions_col_name = "xgboost_AllVars",
  saveInFile          = FALSE
)


gg <- ggplot(data.train[,c("earth_MostImpVars", "target")], 
             aes(x = factor(target), y=earth_MostImpVars, group=target)
) +
  geom_boxplot(aes(fill= target)) +
  scale_y_continuous(limits= c(0,1), breaks= seq(0, 1, by= 0.1))
gg <- gg + stat_summary(aes(label=..y..), 
                        fun.y= median.quartile, 
                        geom= "text", 
                        size= 7,
                        colour="red2"
)
gg



##########################
# xgbLinear
##########################
training_rows <- sample(nrow(data.train)/2, caret.trainingDataLength)
xgbLineargrid <- expand.grid(nrounds = 3000,
                             lambda = 0.003,
                             alpha = 0.003)
caret.predictions.do(prediction_model = "xgbLinear", 
                     predictors = predictors,
                     predictions_col_name = "xgbLinear_MostImpVars",
                     training_rows = training_rows,
                     trControl = myControl,
                     tuneGrid = xgbLineargrid)
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7356501 0.1885802 
# Area under the curve: 0.6692


##########################
# LogitBoost
##########################
training_rows <- sample(nrow(data.train)/2, caret.trainingDataLength)
LogitBoostgrid <- expand.grid(.nIter = 500);
caret.predictions.do(prediction_model = "LogitBoost", 
                     predictors = predictors,
                     predictions_col_name = "LogitBoost_MostImpVars",
                     training_rows = training_rows,
                     trControl = myControl,
                     tuneGrid = LogitBoostgrid)
# Evaluation on data.Temp.test using AUC
# ==========================================
#   Accuracy     Kappa 
# 0.7952332 0.2171206 
# Area under the curve: 0.6579

#######################################################################################
#######################################################################################
# Let's discretize the output probabilities of previous predictions

print(modelsPredictors)

library(CORElearn)
modelsPredictors.Discretization <- discretize(
  outcome_factor,
  data.train[sample(nrow(data.train), 10000), c(modelsPredictors, outcome_factor)],
  method = "greedy",
  estimator = "DistEuclid",
  discretizationLookahead = 300,
  discretizationSample = 0,
  maxBins = 2
)
modelsPredictors.bins <- paste(modelsPredictors, "bins", sep = "_")
data.train[, c(modelsPredictors.bins)] <-
  applyDiscretization(data.train[, modelsPredictors],
                      modelsPredictors.Discretization)
data.test[, c(modelsPredictors.bins)] <-
  applyDiscretization(data.test[, modelsPredictors],
                      modelsPredictors.Discretization)

for (feature in modelsPredictors.bins) {
  data.train[, feature] <- as.numeric(data.train[, feature])
  data.test[, feature] <- as.numeric(data.test[, feature])
}
data.train[1:50, c(modelsPredictors.bins, outcome_factor)]
str(data.train[, c(modelsPredictors.bins, outcome_factor)])

data.train["modelsPredictors.bins.sum"] <- rowSums(data.train[, modelsPredictors.bins]) 
data.test["modelsPredictors.bins.sum"] <- rowSums(data.test[, modelsPredictors.bins]) 

modelsPredictors.bins <- union(modelsPredictors.bins, "modelsPredictors.bins.sum")

ggpairs(data.train[, c("modelsPredictors.bins.sum", outcome_factor)],
        mapping = aes(color = outcome_factor, alpha = .4))
#==============================================================================
# Now, I use the above discretized predictions as new features 
# and train several xgboost models on different sub training sets 
#==============================================================================

predictors <-
  subset(
    features.updated,
    !(
      features.updated %in% features.numeric.continuous.updated.cor
    )
  )
predictors <- union(predictors, features.updated.stats)
predictors <-   union(predictors,
                      "modelsPredictors.bins.sum")
ensemble_name = "xgb_AllFeatures_Ensemble_ControlledParam"
iterations <- 10
training_rows_list <- list()
for(i in 1:iterations){
  n <- sample(seq(trunc(nrow(data.train)/2), nrow(data.train), by= 1), 1)
  print(n)
  if((nrow(data.train) - n) > 10000){
    training_rows_list[[i]] <- seq(n, nrow(data.train), length.out = 10000)
  }else{
    training_rows_list[[i]] <- seq(trunc(nrow(data.train)/2), n, length.out = 10000)
  }
}
xgb_parameters <- list(objective           = "binary:logistic",
                       booster             = "gbtree",
                       eval_metric         = "logloss",
                       eta                 = 0.005,
                       gamma               = 0.007, #0.007
                       max_depth           = 8, 
                       subsample           = 1, 
                       colsample_bytree    = 30/length(predictors), 
                       min_child_weight    = 10, 
                       max_delta_step      = 1, #should be between 1 and 10
                       # num_parallel_tree   = 1,
                       tree_method         = "approx",
                       sketch_eps          = 0.005
                       )
xgb_ensemble <- xgb.predictions.iter1(niter= iterations, 
                                     ensemble_name = ensemble_name, 
                                     training_rows_list = training_rows_list)