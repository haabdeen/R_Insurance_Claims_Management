if(!exists("trimspaces")) {
  source(paste(dir_lib, "/characterDataFuncs.r", sep = ""))
}

#define a new functions for missing values
is.missing <- function(x){
  is.na(x) | x==NaN | x==Inf | x==-Inf | trimspaces(x)==""
}

missing.omit <- function(x){
  subset(x, !is.missing(x))
}

is.missing.data.frame <- function(x){
  do.call(cbind, lapply(x, is.missing))
}

# functions for replacing missing/incomplete values
#===================================================
#replace missing items by the median or mean value from a data.frame column/var (the var must be numeric)
missings2Numeric <- function(df, var, fun =c('mean', 'median'), verbos = F){
  colItems <- df[, var]
  try(if (!is.numeric(colItems))
    stop(paste(
      "var must be a numeric variable in df:", 
      var, 
      "is not numeric.", 
      sep = " ")
    )
  )
  missingItems <- is.missing(colItems)
  numeric_value <- NULL
  if(fun == 'median'){
    numeric_value <- median(subset(colItems, !is.missing(colItems)))
  }else{
    numeric_value <- mean(subset(colItems, !is.missing(colItems)))
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

#convert the item with infinite to max value from a data.frame column/var (the var must be numeric)
infinite2max <- function(df, var, verbos = F){
  colItems <- df[, var]
  try(if (!is.numeric(colItems))
    stop(paste("var must be a numeric variable in df:",
               var,
               "is not numeric.",
               sep = " "
    ))
  )
  infItems <- df[, var]==Inf | df[, var]==-Inf
  mx <- max(subset(df[, var], !is.missing(df[, var])), na.rm=TRUE)
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
    Temp <- missings2Numeric(infinite2max(Temp, var, fun, verbos), var, verbos)
  }
  return(Temp)
}


replace_missingsBy <- function(df, by){
  # This variable should hold the value used for replacing incomplete cases
  missingValueDefault <<- by 
  df[is.missing.data.frame(df)] <- by
  return(df)
}

notmisssings <- function(x){
  if(exists("missingValueDefault")){
    if(!is.na(missingValueDefault)){
      subset(x, x!= missingValueDefault)
    }else{
      subset(x, !is.na(x))
    }
  }else{
    subset(x, !is.missing(x))
  }
}

