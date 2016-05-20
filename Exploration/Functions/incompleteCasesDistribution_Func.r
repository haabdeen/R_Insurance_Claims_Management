getZeroResponseRatio <- function(binaryVar) {
  zeroResponse <- subset(binaryVar, binaryVar == 0)
  oneResponse <- subset(binaryVar, binaryVar == 1)
  return(length(zeroResponse) / length(oneResponse))
}

getIncompleteCasesDistribution <- 
  function(df, binaryResponse = NULL, subfeatures = NULL, minThr = 0.9, maxThr = 1.1){
  if(is.null(binaryResponse)) binaryResponse <- names(df[, ncol(df), drop=F])
  response <- df[, binaryResponse]
  if(is.null(subfeatures)) subfeatures <- subset(names(df), names(df)!= binaryResponse)
  zeroResponseRatio <- getZeroResponseRatio(response)
  #
  subfeatures.withIncompleteCases <- NULL
  incompleteCasesDistribution <- NULL
  ratioIncompleteCases <- NULL
  ratioZeroResponse <- NULL
  normalizedRatioZeroResponse <- NULL
  biased <- NULL
  for(var in subfeatures){
    incompletecases <- subset(df[, c(var, binaryResponse)], is.na(df[, var]))
    if(nrow(incompletecases) > 0) {
      subfeatures.withIncompleteCases <- c(subfeatures.withIncompleteCases,
                                           var)
      ratioIncompleteCases <- c(ratioIncompleteCases,
                                nrow(incompletecases) / nrow(df))
      incompletecases.zeroResponse <-
        subset(incompletecases, incompletecases[, binaryResponse] == 0)
      incompletecases.oneResponse <-
        subset(incompletecases, incompletecases[, binaryResponse] == 1)
      incompletecases.zeroResponseRatio <-
        nrow(incompletecases.zeroResponse) /
        nrow(incompletecases.oneResponse)
      ratioZeroResponse <- c(ratioZeroResponse,
                             incompletecases.zeroResponseRatio)
      incompletecases.normalizedZeroResponseRatio <-
        incompletecases.zeroResponseRatio / zeroResponseRatio
      normalizedRatioZeroResponse <- c(normalizedRatioZeroResponse,
                                       incompletecases.normalizedZeroResponseRatio)
      if (incompletecases.normalizedZeroResponseRatio < minThr |
          incompletecases.normalizedZeroResponseRatio > maxThr) {
        biased <- c(biased, TRUE)
      } else{
        biased <- c(biased, FALSE)
      }
    }
  }
  incompleteCasesDistribution <- data.frame(var = subfeatures.withIncompleteCases,
                                                  incompleteCases = round(ratioIncompleteCases, digits = 3),
                                                  ratio0Response = round(ratioZeroResponse, digits = 3),
                                                  normalizedRatio = round(normalizedRatioZeroResponse, digits = 3),
                                                  biased)
  incompleteCasesDistribution <- 
    incompleteCasesDistribution[with(incompleteCasesDistribution, 
                                     order(-incompleteCases, -normalizedRatio, -incompleteCases)), ]
  return(incompleteCasesDistribution)
}

