source(paste(dir_exploration_functions, "/getMissingsFreq_Func.r", sep=""))

# let's have a look on the variables which involve high frequency of missing data
# let's show variables whereas NA frequency >= 40%
Temp <- getMissingsFreq(data.all, subfeatures = features.updated)
Temp <- sort(round(subset(Temp, Temp >= 0.4), 3), 
             decreasing =  T)
Temp <- data.frame(var = names(Temp), NAFreq = as.numeric(Temp), stringsAsFactors = F)
Temp <- Temp[order(-Temp$NAFreq),]

png(
  filename = paste(dir_exploration_figures, "/Vars_with_freq_NAs.png", sep=""),
  type = "cairo",
  units = "cm",
  width = 50,
  height = 25,
  pointsize = 11,
  res = 300
)
theme_set(theme_bw())
vars_with_freq_NAs_plot <- 
  ggplot(Temp, aes(x=var, y=NAFreq)) +
  geom_bar(stat="identity", fill="gray50") +
  scale_y_continuous(breaks=seq(0,1, by=0.1), limits=c(0,1))
vars_with_freq_NAs_plot
dev.off()

features.withFreqMissings <- Temp$var
length(features.withFreqMissings)
# in 102 variables missing data represents more than 40% of the variable data

# let's analyze if missing data in these varaibles have sens 
# w.r.t the target variable in the train dataset
#
source(paste(dir_exploration_functions, "/incompleteCasesDistribution_Func.r", sep = ""))
incompleteCasesDistribution <- 
  getIncompleteCasesDistribution(data.train, 
                              binaryResponse = outcome, 
                              subfeatures = features.withFreqMissings, 
                              minThr = 0.9, maxThr = 1.1)
html.data.frame(incompleteCasesDistribution, 
                file = paste(dir_exploration_figures, "/incompleteCasesDistribution.html", sep=""))
incompleteCasesDistribution <- 
  incompleteCasesDistribution[incompleteCasesDistribution$biased,]
html.data.frame(incompleteCasesDistribution, 
                file = paste(dir_exploration_figures, "/incompleteCasesDistribution_biased.html", sep=""))
#   var   incompleteCases ratio0Response normalizedRatio biased
# 2 v113           0.484          0.214           0.681   TRUE
#
# it seems that only v113 could be an interesting prediction feature

Temp <- data.train[, c("v113", outcome)]
Temp$v113[is.na(Temp$v113)] <- "#na"
Temp[outcome] <- as.factor(Temp[, outcome])
png(
  filename = paste(dir_exploration_figures, "/v113_target.png", sep=""),
  type = "cairo",
  units = "cm",
  width = 50,
  height = 25,
  pointsize = 11,
  res = 300
)
theme_set(theme_bw())
v113_target_plot <- 
  ggpairs(Temp, mapping = aes(fill=target, colour=target))
v113_target_plot
dev.off()

# let's drop features with frequent missings except v113 
dropFeatures(subset(features.withFreqMissings, 
                    features.withFreqMissings !="v113")) 

#let's have a look on missing data in remaining features only
source(paste(dir_exploration_functions, "/visualize_missings_patterns_Func.r", sep=""))
visualize_missings_patterns(data.all, 
                            subfeatures = features.updated,
                            sampleSize = 20000,
                            fileName = paste(dir_exploration_figures, "/MissingDataPattern_rmFreqMissings.png", sep = ""))

# The obtained figure shows that some features could be grouped together
# w.r.t the presence of incomplete cases
# these groups are: 
# v12, v50
# v34, v40
# v34, v40, v12, v50, v10


# let's repeat our analysis of incomplete cases distribution 
# w.r.t the target variable in the train dataset
# only in remained features
incompleteCasesDistribution <- 
  getIncompleteCasesDistribution(data.train, 
                              binaryResponse = outcome, 
                              subfeatures = features.updated, 
                              minThr = 0.9, maxThr = 1.1)
incompleteCasesDistribution <- 
  incompleteCasesDistribution[incompleteCasesDistribution$biased,]
html.data.frame(incompleteCasesDistribution, 
                file = paste(dir_exploration_figures, "/incompleteCasesDistribution_rmFreMissings_biased.html", sep=""))

# The output shows that for the remainig features which involve incomplete cases 
# the distributio of the respose (target binary variable) is noticiably different
# for instance:
# --for variable v113, 48.4% of rows involve icomplete cases 
#   and the ratio of 0-response over 1-response is 0.214, 
#   which is different than the unconstrained distribution of the response var 
#   whereas the aforementioned ratio is approx. 0.27
# --for variable v56, 6% of rows involve icomplete cases 
#   and the ratio of 0-response over 1-response is 0.606, 
#   which is different than the unconstrained distribution of the response var 
#   whereas the aforementioned ratio is approx. 0.27
# As a summary:
#   - these variables may represent important predictors, specially those which involve many icomplete cases
#     such as v113, v56 and v31
#   - incomplete cases in these variables should be replaced by a specific value that keep these cases as flagged

features.withMissings <- as.character(incompleteCasesDistribution$var)
