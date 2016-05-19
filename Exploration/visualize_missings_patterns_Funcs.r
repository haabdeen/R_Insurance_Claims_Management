# Exploring/visualizing how missing values are present in data using the VIM package
visualize_missings_patterns <-
  function(df,
           subfeatures = NULL,
           sampleSize = NULL,
           fileName) {
    require(VIM)
    if(is.null(subfeatures)){
      subfeatures <- names(df)
    }else{
      df <- df[, subfeatures]
    }
    if(!is.null(sampleSize)){
      df <- df[sample(nrow(df), sampleSize), ]
    }
    png(
      filename = fileName,
      type = "cairo",
      units = "cm",
      width = 50,
      height = 25,
      pointsize = 11,
      res = 300
    )
    missings_plot <- aggr(
      df,
      col = c('white', 'black'),
      border = "gray60",
      numbers = T,
      sortVars = T,
      combined = T,
      sortCombs = F,
      only.miss = T,
      prop = T,
      labels = names(df),
      cex.axis = .7,
      gap = 3,
      ylab = c("Missing data Pattern")
    )
    dev.off()
  }