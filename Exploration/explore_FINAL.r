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

# Drop features with frequent missings except v113 
dropFeatures(subset(features.withFreqMissings, 
                    features.withFreqMissings !="v113")) 

features.withMissings <- c("v113", "v56", "v31", "v21", "v22", "v112", 
                           "v125", "v34", "v40", "v10", "v12", "v50", 
                           "v114", "v14", "v52", "v91", "v107")

# Drop numeric vars which have strong colinearity with other numeric vars 
colinearVars <- c("v10", "v34")
dropFeatures(colinearVars)
