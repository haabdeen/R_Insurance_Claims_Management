#====================================================================
# Functions and Constants
#====================================================================



is.discreteVar <- function(x){
  return( is.numeric(x) & (length(unique(x)) < 30) )
}

is.continuousVar <- function(x){
  return( is.numeric(x) & !is.discreteVar(x) )
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
