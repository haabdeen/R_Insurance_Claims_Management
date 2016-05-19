trimspaces <- function(x){ 
  gsub("\\s", "", x) 
}

strsplitV <- function(v, s=".", missingValue = as.character(missingValueDefault)){
  vc <- trimspaces(as.character(v))
  for (i in 1:length(vc)) {
    if(vc[i]!=missingValue & nchar(vc[i]) > 1){
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

