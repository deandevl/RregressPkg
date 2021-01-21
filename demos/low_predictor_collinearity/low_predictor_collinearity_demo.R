library(data.table)
library(caret)
library(AppliedPredictiveModeling)

length_fun <- function(xx = NULL){
  if(length(unique(xx)) != 1){
    return(xx)
  }else{
    return(NULL)
  }
}

data(segmentationOriginal)
seg_orig_dt <- data.table::setDT(segmentationOriginal)
seg_train_dt <- seg_orig_dt[Case == "Train"]

pre_process_lst <- caret::preProcess(seg_train_dt, method = "BoxCox")
seg_train_transform_dt <- data.table::setDT(stats::predict(pre_process_lst, seg_train_dt))

#Remove those predictors that have only a single value
seg_train_transform_dt[,colnames(seg_train_transform_dt) := lapply(seg_train_transform_dt, length_fun)]
#Compute the correlation matrix
seg_cor <- stats::cor(seg_train_transform_dt[, !c("Class")])

results <- RregressPkg::low_predictor_collinearity(corr_matrix = seg_cor)


# Out of 111 variables, RregressPkg::low_predictor_collinearity() found 68
# with low collinearity. This suggest removing 43 predictors.
# "Applied Predictive Modeling" page 47
