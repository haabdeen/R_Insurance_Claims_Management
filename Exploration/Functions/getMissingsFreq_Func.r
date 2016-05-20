getMissingsFreq <- function(df, subfeatures = NULL) {
  if (is.null(subfeatures))
    subfeatures <- names(df)
  df <- df[, subfeatures, drop = F]
  Temp <- apply(df,
                2,
                function(x) {
                  sum(is.na(x)) / nrow(df) #IMPORTANT: missing data must be uniformed and represented by NAs
                })
  return(Temp)
}