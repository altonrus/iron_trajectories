library(data.table)
library(rsample)
library(pROC)
library(xgboost)
library(dplyr)
library(randomForest)
library(glmnet)
library(rpart)


#GENERATE NESTED CV OBJECTS -----------
#
# We have 3 versions of the prediction task:
#  *AP - all patients, predicting without xtra biomarkers
#  *withXB- subset of patients with xtra biomarkers, predicting without xtra biomarkers
#  *noXB - subset of patients with xtra biomarkers, predicting with xtra biomarkers
#
# Additionally, we have different versions of the rsplit objects based on
#   requirements for each of the prediction models:
#    *onehot - categorical variable onehot encoded
#    *factor - categorical features as factors
#    *interactions - includes interactions (is one hot)
# So a total of 9 rsplit objects will be created

#Read data for model development (md) dataset
dt.md <- fread( "./1_data/ml_training_data.csv")
identifiers <- c("RandID", "VisitDate", "VisitNum", "DER_VisitResult", "DV_Donproc", "DER_RBCLoss_Units")
extra_biomarkers <- c('ARUP_Ferritin', 'ARUP_STR', 'DER_ARUP_log_Ferr', 
                      'DER_ARUP_log_STfR_Ferr', 'DER_BodyIron')

#remove extraneous fields
dt.md[, c(identifiers) := NULL]
#remove index donations missing hemoglobin
dt.md<- dt.md[!is.na(FingerstickHGB_equiv)]

#FORMAT ALL PATIENTS DATASET
dt.AP <- cbind(dt.md)[,c(extra_biomarkers):=NULL]
#Distribution of outcomes
table(dt.AP$fu_outcome)
table(dt.AP$fu_outcome)/nrow(dt.AP)

# Create 2 versions: one with characters as factors and
#  one with categorical variables 1-hot encoded
#  for model types that require that

#ONE HOT FOR xgb
#Create dummy one-hot variables for categoricals
dt.OH <- data.table(model.matrix(fu_outcome ~ ., data=dt.AP))
dt.OH <- cbind(dt.OH, 
               "fu_outcome" = dt.AP$fu_outcome)



# library(lares)
# lares::corr_cross(dt.OH)
#Look at variance in predictors, near-zero variance

dt.OH <- dt.OH[ , gender_menstrating_cohortsM := NULL]


#CHAR as FACTOR
#Convert all characters to factors
char_cols <- colnames(dt.AP)[unlist(dt.AP[ , lapply(.SD, is.character),][1,])]

for (col in char_cols) set(dt.AP, j=col, value=as.factor(dt.AP[[col]]))
str(dt.AP)
#Make fu_outcome factor
dt.AP[, fu_outcome := as.factor(paste0("Z", fu_outcome))]

## INTERACTIONS (all patients)
f <- as.formula(y ~ .*.)
y <- dt.OH$fu_outcome
# Second step: using model.matrix to take advantage of f
#dt.interactions <- data.table(model.matrix(f, dt.OH[,c(-1, -1*ncol(dt.OH))]))
dt.interactions <- data.table(model.matrix(f, dt.OH[,c(-1, -53)]))
dt.interactions <- cbind(dt.interactions, "fu_outcome" = dt.OH$fu_outcome)


#CV SPLIT (all patients)
set.seed(250)
rsplit_factor <- nested_cv(dt.AP,
                           outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                           inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_factor, "./1_data/rsplits/rsplit_factors_AP.rds")

set.seed(250)
rsplit_OH <- nested_cv(dt.OH,
                       outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                       inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_OH, "./1_data/rsplits/rsplit_OH_AP.rds")


set.seed(250)
rsplit_ints <- nested_cv(dt.interactions,
                         outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                         inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_ints, "./1_data/rsplits/rsplit_ints_AP.rds")

#XTRA BIOMARKERS VERSIONS
dt.withXB <- cbind(dt.md)[!is.na(ARUP_Ferritin),]
#Distribution of outcomes
table(dt.withXB$fu_outcome)
table(dt.withXB$fu_outcome)/nrow(dt.XB)
dt.noXB <- cbind(dt.withXB)[,c(extra_biomarkers):=NULL]
# Create 2 versions: one with characters as factors and
#  one with categorical variables 1-hot encoded
#  for model types that require that

#ONE HOT FOR xgb
#Create dummy one-hot variables for categoricals
dt.OH_withXB <- data.table(model.matrix(fu_outcome ~ ., data=dt.withXB))
dt.OH_withXB <- cbind(dt.OH_withXB, 
                      "fu_outcome" = dt.withXB$fu_outcome)
#Fields "gender_menstrating_cohortsM" "DD_GenderM"  the same due to one-hot encoding. removing the menstrating one.
dt.OH_withXB <- dt.OH_withXB[ , gender_menstrating_cohortsM := NULL]
#Create dummy one-hot variables for categoricals
dt.OH_noXB <- data.table(model.matrix(fu_outcome ~ ., data=dt.noXB))
dt.OH_noXB <- cbind(dt.OH_noXB, 
                    "fu_outcome" = dt.noXB$fu_outcome)
#Fields "gender_menstrating_cohortsM" "DD_GenderM"  the same due to one-hot encoding. removing the menstrating one.
dt.OH_noXB <- dt.OH_noXB[ , gender_menstrating_cohortsM := NULL]


#CHAR as FACTOR
#Convert all characters to factors
char_cols <- colnames(dt.withXB)[unlist(dt.withXB[ , lapply(.SD, is.character),][1,])]
for (col in char_cols) set(dt.withXB, j=col, value=as.factor(dt.withXB[[col]]))
for (col in char_cols) set(dt.noXB, j=col, value=as.factor(dt.noXB[[col]]))
#str(dt.withXB)
#Make fu_outcome factor
dt.withXB[, fu_outcome := as.factor(paste0("Z", fu_outcome))]
dt.noXB[, fu_outcome := as.factor(paste0("Z", fu_outcome))]


## INTERACTIONS
f <- as.formula(y ~ .*.)
y <- dt.OH_withXB$fu_outcome
ncol(dt.OH_withXB); ncol(dt.OH_noXB)
# Second step: using model.matrix to take advantage of f
dt.Ints_withXB <- data.table(model.matrix(f, dt.OH_withXB[,c(-1, -58)]))
dt.Ints_withXB <- cbind(dt.Ints_withXB, "fu_outcome" = dt.OH_withXB$fu_outcome)
dt.Ints_noXB <- data.table(model.matrix(f, dt.OH_noXB[,c(-1, -53)]))
dt.Ints_noXB <- cbind(dt.Ints_noXB, "fu_outcome" = dt.OH_noXB$fu_outcome)

#CV SPLIT 
#XB subset - with and without xtra biomarkers
set.seed(250)
rsplit_factor_withXB <- nested_cv(dt.withXB,
                                  outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                                  inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_factor_withXB, "./1_data/rsplits/rsplit_factors_withXB.rds")
set.seed(250)
rsplit_factor_noXB <- nested_cv(dt.noXB,
                                outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                                inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_factor_noXB, "./1_data/rsplits/rsplit_factors_noXB.rds")

set.seed(250)
rsplit_OH_withXB <- nested_cv(dt.OH_withXB,
                              outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                              inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_OH_withXB, "./1_data/rsplits/rsplit_OH_withXB.rds")
set.seed(250)
rsplit_OH_noXB <- nested_cv(dt.OH_noXB,
                            outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                            inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_OH_noXB, "./1_data/rsplits/rsplit_OH_noXB.rds")

set.seed(250)
rsplit_ints_withXB <- nested_cv(dt.Ints_withXB,
                                outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                                inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_ints_withXB, "./1_data/rsplits/rsplit_ints_withXB.rds")
rsplit_ints_noXB <- nested_cv(dt.Ints_noXB,
                              outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                              inside = vfold_cv(v=5, strata="fu_outcome")
)
saveRDS(rsplit_ints_noXB, "./1_data/rsplits/rsplit_ints_noXB.rds")

#Save the full training sets as CSV
fwrite(dt.AP, "./1_data/model_dev_data/mdset_factors_AP.csv")
fwrite(dt.OH, "./1_data/model_dev_data/mdset_OH_AP.csv")
fwrite(dt.interactions, "./1_data/model_dev_data/mdset_ints_AP.csv")

fwrite(dt.withXB, "./1_data/model_dev_data/mdset_factors_withXB.csv")
fwrite(dt.OH_withXB, "./1_data/model_dev_data/mdset_OH_withXB.csv")
fwrite(dt.Ints_withXB, "./1_data/model_dev_data/mdset_ints_withXB.csv")

fwrite(dt.noXB, "./1_data/model_dev_data/mdset_factors_noXB.csv")
fwrite(dt.OH_noXB, "./1_data/model_dev_data/mdset_OH_noXB.csv")
fwrite(dt.Ints_noXB, "./1_data/model_dev_data/mdset_ints_noXB.csv")

features_list<-list(
  "OH" = dt.OH_withXB,
  "Ints" = dt.Ints_withXB,
  "factor" = dt.withXB
)
saveRDS(features_list, "./1_data/model_dev_data/mdset_features_list.RDS")


#DEFINE HYPERPARAMETER SETS -------

# XGB MODEL
xgb_param_sets <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.3),
  max_depth = seq(2, 20, 2),
  min_child_weight = c(0, 1, 2, 4, 8),
  subsample = c(0.65, 0.8, 1),
  colsample_bytree = c(0.8, 0.9, 1)
)
fwrite(xgb_param_sets, "./1_data/hyperparam_sets/xgb_param_sets.csv")

# RANDOM FOREST 
rf_param_sets <- expand.grid(
  nodesize = c(1,2,4,8),
  ntree = seq(250, 2500, 250),
  replace = c(TRUE, FALSE)
)
fwrite(rf_param_sets, "./1_data/hyperparam_sets/rf_param_sets.csv")

# ELASTICNET
en_param_sets <- rbind(
  data.table(alpha=0, lambda=0),
  expand.grid(
    alpha = c(0, .25, .5, .75, 1),
    lambda = seq(0.01, 0.1, 0.01)
  )
)
fwrite(en_param_sets, "./1_data/hyperparam_sets/en_param_sets.csv")

enint_param_sets <- expand.grid(
  alpha = c(0, .25, .5, .75, 1),
  lambda = seq(0.01, 0.1, 0.01)
)
fwrite(enint_param_sets, "./1_data/hyperparam_sets/enint_param_sets.csv")


# REGRESSION TREES 
rpart_param_sets <- expand.grid(
  cp = c(0.001, 0.005, 0.01, 0.05, 0.1),
  minsplit = c(10, 15, 20, 25, 30)
)
fwrite(rpart_param_sets, "./1_data/hyperparam_sets/rpart_param_sets.csv")
