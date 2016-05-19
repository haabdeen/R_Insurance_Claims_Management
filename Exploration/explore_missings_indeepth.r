

# let's have a look on the variables which involve high frequency of missing data
# let's show variables whereas NA frequency >= 40%
Temp <- apply(all_data[, 
                       features, 
                       drop = FALSE ], 
              2,
              function(x){
                sum(is.missing(x))/nrow(all_data)
              })
Temp <- sort(round(subset(Temp, Temp >= 0.4), 
                   3), 
             decreasing =  T)
# v30  v113  v102   v23   v51   v85  v119  v123   v16   v69   v78  v115  v131    v1    v2    v4 
# 0.527 0.484 0.449 0.443 0.443 0.443 0.443 0.443 0.437 0.437 0.437 0.437 0.437 0.436 0.436 0.436 
# v6    v7    v9   v11   v13   v15   v17   v18   v19   v20   v26   v27   v28   v29   v32   v33 
# 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 
# v35   v37   v39   v41   v42   v43   v44   v45   v48   v49   v53   v55   v57   v58   v59   v60 
# 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 
# v61   v64   v65   v67   v68   v73   v76   v77   v80   v83   v84   v86   v88   v90   v92   v93 
# 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 
# v94   v95   v96   v97   v99  v100  v101  v103  v104  v106  v111  v116  v118  v120  v121  v122 
# 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 0.436 
# v126  v127  v130   v87   v98  v105    v5    v8   v25   v36   v46   v54   v63   v70   v81   v82 
# 0.436 0.436 0.436 0.426 0.426 0.426 0.425 0.425 0.425 0.425 0.425 0.425 0.425 0.425 0.425 0.425 
# v89  v108  v109  v117  v124  v128 
# 0.425 0.425 0.425 0.425 0.425 0.425

#features.withFreqMissings <- names(Temp)
#hardly coding it:
features.withFreqMissings <- c("v30", "v113", "v102", "v23", "v51", "v85", "v119", "v123", "v16", "v69", 
                               "v78", "v115", "v131", "v1", "v2", "v4", "v6", "v7", "v9", "v11", "v13", 
                               "v15", "v17", "v18", "v19", "v20", "v26", "v27", "v28", "v29", "v32", "v33", 
                               "v35", "v37", "v39", "v41", "v42", "v43", "v44", "v45", "v48", "v49", "v53", 
                               "v55", "v57", "v58", "v59", "v60", "v61", "v64", "v65", "v67", "v68", "v73", 
                               "v76", "v77", "v80", "v83", "v84", "v86", "v88", "v90", "v92", "v93", "v94", 
                               "v95", "v96", "v97", "v99", "v100", "v101", "v103", "v104", "v106", "v111", 
                               "v116", "v118", "v120", "v121", "v122", "v126", "v127", "v130", "v87", "v98", 
                               "v105", "v5", "v8", "v25", "v36", "v46", "v54", "v63", "v70", "v81", "v82", 
                               "v89", "v108", "v109", "v117", "v124", "v128")
length(features.withFreqMissings)
# in 102 variables missing data represents more than 40% of the variable data


# let's analyze if missing data in these varaibles have sens w.r.t the target variable in the train dataset
Temp <- train_data[sample(nrow(train_data), 5000), c(features.withFreqMissings, "target") ]
Temp <- melt(Temp, id.vars = "target")
Temp <- subset(Temp, is.na(Temp$value))   # select only records with missing values
Temp$value[is.na(Temp$value)] <- "missing value"
str(Temp)

# for each variable what is the ratio of records that have 1 as a response (1 in the target variable)
# if the ratio is 
#   -very high (considerably larger than the non-conditional ratio of 1 values in the target variable, which is approx. 0.76) --see @distribution_of_target_variable_values
#   -or very low (considerably smaller than the non-conditional ratio of 0 values in the target variable, which is approx. 0.24) --see @distribution_of_target_variable_values   
# then we think that the corresponding variable may be an interesting prediction feature 
Temp <- aggregate(Temp$target, 
                  by=list(variable = Temp$variable), 
                  FUN = function(x){ round(sum(x)/length(x), digits = 2)})
Temp
# it seems that v113 could be an interesting prediction feature

# let's drop features with frequent missings except v113 
features.updated <- subset(features, !(features %in% features.withFreqMissings[which(features.withFreqMissings!="v113")] ))
length(features.updated) # hereafter we consider only the remaining 30 features 
features.numeric.updated <- subset(features.numeric, features.numeric %in% features.updated )
features.categoric.updated <- subset(features.categoric, features.categoric %in% features.updated )


#let's have a look on missing data in remaining features only
# png(filename="MissingDataInSelectedFeautres.png",
#     type="cairo",
#     units="cm",
#     width=50,
#     height=25,
#     pointsize=11,
#     res=300)
# missings_plot <- aggr(
#   all_data[, features.updated],
#   col=c('white','black'),
#   border= "gray60",
#   numbers= T, sortVars= T, combined= T, sortCombs = F, prop = T,
#   labels=features, cex.axis=.7,
#   gap=3, ylab=c("Missing data Pattern"))
# dev.off()
