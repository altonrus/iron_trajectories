#This file also gets uploaded to sherlock for training on computing
#  cluster
library(stringr)
library(xgboost)
library(randomForest)
library(glmnet)
library(rsample)
## DATA MUNGING --------

#Creates 3 versions of a given features table: onehot encoded, factor, and interactions
gen_features_list <- function(dt, withXB=TRUE){
  identifiers <- c("RandID", "VisitDate", "VisitNum", "DER_VisitResult", "DV_Donproc", "DER_RBCLoss_Units")
  extra_biomarkers <- c('ARUP_Ferritin', 'ARUP_STR', 'DER_ARUP_log_Ferr', 
                        'DER_ARUP_log_STfR_Ferr', 'DER_BodyIron')
  dt<-rbind(dt)
  #remove extraneous fields
  dt[, c(identifiers) := NULL]
  if(withXB==FALSE){
    dt[, c(extra_biomarkers):=NULL]
  }
  #remove index donations missing hemoglobin
  dt<- dt[!is.na(FingerstickHGB_equiv)]
  

  #remove index donations missing hemoglobin
  dt<- dt[!is.na(FingerstickHGB_equiv)]
  dt.OH <- data.table(model.matrix(fu_outcome ~ ., data=dt))
  dt.OH <- cbind(dt.OH, 
                 "fu_outcome" = dt$fu_outcome)
  #Fields "gender_menstrating_cohortsM" "DD_GenderM"  the same due to one-hot encoding. removing the menstrating one.
  dt.OH <- dt.OH[ , gender_menstrating_cohortsM := NULL]
  
  #CHAR as FACTOR
  #Convert all characters to factors
  char_cols <- colnames(dt)[unlist(dt[ , lapply(.SD, is.character),][1,])]
  
  for (col in char_cols) set(dt, j=col, value=as.factor(dt[[col]]))
  #str(dt)
  #Make fu_outcome factor
  dt[, fu_outcome := as.factor(paste0("Z", fu_outcome))]
  
  ## INTERACTIONS (all patients)
  f <- as.formula(y ~ .*.)
  y <- dt.OH$fu_outcome
  # Second step: using model.matrix to take advantage of f
  #dt.interactions <- data.table(model.matrix(f, dt.OH[,c(-1, -1*ncol(dt.OH))]))
  dt.interactions <- data.table(model.matrix(f, dt.OH[,c(-1, -53)]))
  dt.interactions <- cbind(dt.interactions, "fu_outcome" = dt.OH$fu_outcome)
  
  return(list("OH"=dt.OH,
              "Ints"=dt.interactions,
              "factor"=dt))
  
}




## DATA PROCESSING



# #Load top model
# top.model <- xgb.load("./data/topmodel")
# calib_weights <- readRDS("./data/calib_weights_noplatt.RDS")

# MODEL TRAIN AND SELECTION FUNCTIONS ---------------

# MODEL ASSESSMENT using specified single model configuration (non ensemble)
# and set of hyperparameters to assess in nested CV.
# must also feed the appropriate format of the dataset split for CV
# and provide path for saving the results
# idx_start: When  splitting a large set of hyperparams
#  across multiple jobs run on a comuting cluster, set id_start
#  to the number of the first row in hyperparam table under assessment
#  in this job.
run_mod_assess <- function(mod_name, param_sets, dt_split, 
                           idx_start = 1, 
                           fname_results = "assess_results.csv"){
  #Construct table for storing results
  AUC_cols <-  matrix(ncol = 15, nrow = 0)
  colnames(AUC_cols) <- paste0("AUC_",formatC(1:15, width=2, flag="0"))
  
  dt.results <- cbind(
    data.table(
      model = character(),
      hyperparam_idx = integer()),
    AUC_cols
  )
  #Loop over all hyperparam sets and compute AUC for each of 15 partitions
  print(Sys.time())
  for (row in 1:nrow(param_sets)){
    #print(paste0("Param row ", row))
    params <- as.list(unlist(param_sets[row,]))
    dt.results <- rbind(dt.results,
                        cv_config(dt_split, params, 
                                  row+idx_start-1, mod_name)
    )
    #Overwrite every time in case job times out
    fwrite(dt.results, fname_results)
    print(paste0("Completed hyperparam set ",row,". ", Sys.time()))
  }
  dt.results[ , AUC_mean := rowMeans(.SD), .SDcols = 
                paste0("AUC_",formatC(1:15, width=2, flag="0"))]
  #Overwrite at end with mean AUC added
  fwrite(dt.results, fname_results)
}
  


tune_subset <- function(mod_name, param_sets, dt_split, 
                        fname_stump = "idi/XGB_assess_results_",
                        start, end) {
  
  fname <- paste0(fname_stump, formatC(start, width=4, flag=0), "to", 
                  formatC(end, width=4, flag=0), ".csv")
  
  run_mod_assess(mod_name, 
                 param_sets[start:end, ], dt_split, 
                 idx_start = start, 
                 fname_results = fname)
}


# CV CONFIGURATION (called by model assessment function)
#  For param set, perform CV across all inner folds
#  and return one-vs-all-auc for each of 3X5=15 inner CV sets
cv_config <- function(outer_split, params, row_num, mod_name){
  
  outcome <- list(
    model=mod_name,
    hyperparam_idx = row_num
    #hyperparams = params
  )
  
  
  for (row_outer in 1:nrow(outer_split)){
    #print(paste0(" outer row ", row_outer))
    inner_split <- outer_split$inner_resamples[[row_outer]]
    #Table to store predictions across each inner fold
    dt_inner_fold_preds <- data.table(
      "Z0"= numeric(),
      "Z1"= numeric(),
      "Z2"= numeric(),
      "Z3"= numeric(),
      "fu_outcome"= numeric()
    )
    
    for (row_inner in 1:nrow(inner_split)){
      #print(paste0("  inner row ", row_inner))
      tryCatch({
        dt_inner_fold_preds <- rbind(
          dt_inner_fold_preds,
          mod_eval(inner_split$splits[[row_inner]], params, mod_name)
        )
      }, error = function(error_condition){
        cat("Could not fit on this inner fold")
      })
    }
    #Calc 1vsAll AUC across all folds
    outcome[paste0("AUC_",formatC(row_outer, width=2, flag="0"))] <- 
      multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3, data = dt_inner_fold_preds)$auc
  }
  return(outcome)
}



# MODEL EVALUATION (called by cv_config)
#  for a given train-test split in the rsplit_object, set of hyperparameters,
#  assess performance. When used for ensembles, set return_train_pred to 
#  true so those can be accessed to tune the ensemble
mod_eval <- function(rsplit_obj, params, mod_name, return_train_pred=FALSE){
  ncol_rsplit <- ncol(analysis(rsplit_obj))
  #RANDOM FOREST
  if (mod_name == "random_forest"){
    rf_fit <- randomForest(
      fu_outcome~.,
      data = analysis(rsplit_obj),
      nodesize = params$nodesize,
      ntree = params$ntree,
      replace = params$replace
    )
    #generate predictions
    preds = as.data.table(predict(rf_fit, assessment(rsplit_obj), type="prob"))
    if(return_train_pred==TRUE){
      pred_train <- as.data.table(predict(rf_fit, analysis(rsplit_obj), type="prob"))
      preds<-rbind(
        cbind(Set="Train", pred_train),
        cbind(Set="Test", preds)
      )
      colnames(preds) <- c("Set", paste0("Z", 0:3))
      fu_outcome = rbind(analysis(rsplit_obj)[,"fu_outcome"],
                         assessment(rsplit_obj)[,"fu_outcome"])
    } else{
      colnames(preds) <- paste0("Z", 0:3)
      fu_outcome = assessment(rsplit_obj)[,"fu_outcome"]
    }
    fu_outcome <- unlist(lapply(fu_outcome,function(x) as.numeric(substr(x, 2, 2))))
  #ELASTIC NET (NO INTERACTIONS)
  } else if (mod_name == "elastic_net"){
    fit <- glmnet(
      x = as(data.matrix(analysis(rsplit_obj)[ , 1:(ncol_rsplit-1)]), "dgCMatrix"),
      y = analysis(rsplit_obj)$fu_outcome,
      lambda = params$lambda,
      alpha = params$alpha,
      family="multinomial"
    )
    #generate predictions
    preds = as.data.table(predict(fit, newx = data.matrix(assessment(rsplit_obj)[ , 1:(ncol_rsplit-1)]), type = "response")[,,1])
    if(return_train_pred==TRUE){
      pred_train <- as.data.table(predict(fit, newx = data.matrix(analysis(rsplit_obj)[ , 1:(ncol_rsplit-1)]), type = "response")[,,1])
      preds<-rbind(
        cbind(Set="Train", pred_train),
        cbind(Set="Test", preds)
      )
      colnames(preds) <- c("Set", paste0("Z", 0:3))
      fu_outcome = unlist(rbind(analysis(rsplit_obj)[,"fu_outcome"],
                         assessment(rsplit_obj)[,"fu_outcome"]))
    } else{
      colnames(preds) <- paste0("Z", 0:3)
      fu_outcome = unlist(assessment(rsplit_obj)[,"fu_outcome"])
    }
  #ELASTIC NET WITH INTERACTIONS
  } else if (mod_name == "elastic_net_interactions"){
    Sys.time()
    fit <- glmnet(
      x =as(data.matrix(analysis(rsplit_obj)[ , 1:(ncol_rsplit-1)]), "dgCMatrix"),
      y = analysis(rsplit_obj)$fu_outcome,
      lambda = params$lambda,
      alpha = params$alpha,
      family="multinomial"
    )
    Sys.time()
    #generate predictions
    preds = as.data.table(predict(fit, newx = data.matrix(assessment(rsplit_obj)[ , 1:(ncol_rsplit-1)]), type = "response")[,,1])
    if(return_train_pred==TRUE){
      pred_train <- as.data.table(predict(fit, newx = data.matrix(analysis(rsplit_obj)[ , 1:(ncol_rsplit-1)]), type = "response")[,,1])
      preds<-rbind(
        cbind(Set="Train", pred_train),
        cbind(Set="Test", preds)
      )
      colnames(preds) <- c("Set", paste0("Z", 0:3))
      fu_outcome = unlist(rbind(analysis(rsplit_obj)[,"fu_outcome"],
                         assessment(rsplit_obj)[,"fu_outcome"]))
    } else{
      colnames(preds) <- paste0("Z", 0:3)
      fu_outcome = unlist(assessment(rsplit_obj)[,"fu_outcome"])
    }
  #RPART DECISION TREE
  } else if (mod_name == "rpart"){
    fit <- rpart(
      fu_outcome~.,
      data = analysis(rsplit_obj),
      control = rpart.control(cp = params$cp,
                              minsplit = params$minsplit)
    )
    #generate predictions
    preds = as.data.table(predict(fit, assessment(rsplit_obj), type="prob"))
    # 
    fu_outcome = unlist(assessment(rsplit_obj)[,"fu_outcome"])
  #GENERALIZED LINEAR MODEL
  } else if (mod_name%in% c("glm", "glm_interactions") ){
    fit <- glm(
      fu_outcome~.,
      data = analysis(rsplit_obj),
      family="",
    )
    #generate predictions
    preds = as.data.table(predict(fit, assessment(rsplit_obj), type="response"))
    if(return_train_pred==TRUE){
      pred_train <- as.data.table(fit$fitted.values)#as.data.table(predict(fit, analysis(rsplit_obj), type="response"))
      preds<-rbind(
        cbind(Set="Train", pred_train),
        cbind(Set="Test", preds)
      )
      colnames(preds) <- c("Set", paste0("Z", 0:3))
      fu_outcome = rbind(analysis(rsplit_obj)[,"fu_outcome"],
                         assessment(rsplit_obj)[,"fu_outcome"])
    } else{
      colnames(preds) <- paste0("Z", 0:3)
      fu_outcome = assessment(rsplit_obj)[,"fu_outcome"]
    }
  #XGB GRADIENT BOOSTED MACHINE
  } else if (mod_name == "XGB"){
    #Extract train and test sets in xgb's special format
    xgb.train = xgb.DMatrix(data = as.matrix(analysis(rsplit_obj)[,-"fu_outcome"]),
                            label = as.matrix(analysis(rsplit_obj)[,"fu_outcome"]))
    xgb.test = xgb.DMatrix(data = as.matrix(assessment(rsplit_obj)[,-"fu_outcome"]),
                           label = as.matrix(assessment(rsplit_obj)[,"fu_outcome"]))
    #Fit model
    xgb.fit=xgb.train(
      params=params,
      data=xgb.train,
      nrounds=10000,
      early_stopping_rounds=10,
      watchlist=list(val1=xgb.train,val2=xgb.test),
      verbose=0,
      booster="gbtree",
      gamma=3,
      objective="multi:softprob",
      eval_metric="mlogloss",
      num_class=4
    )
    preds = as.data.table(predict(xgb.fit, xgb.test, reshape = T))
    if(return_train_pred==TRUE){
      pred_train <- as.data.table(predict(xgb.fit, xgb.train, reshape = T))
      preds<-rbind(
        cbind(Set="Train", pred_train),
        cbind(Set="Test", preds)
      )
      colnames(preds) <- c("Set", paste0("Z", 0:3))
      fu_outcome = unlist(rbind(analysis(rsplit_obj)[,"fu_outcome"],
                         assessment(rsplit_obj)[,"fu_outcome"]))
      
    } else{
      colnames(preds) <- paste0("Z", 0:3)
      fu_outcome = unlist(assessment(rsplit_obj)[,"fu_outcome"])
    }
  }
  if(nchar(as.character(fu_outcome[1])) == 1){
    fu_outcome<-paste0("Z",fu_outcome)
  }
  
  return(cbind(preds,fu_outcome))
}




AUC <- function(Z0, Z1, Z2, Z3, fu_outcome){
  return(multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc)
}


# ENSEMBLE MODELS ------

#Construct model specs as lists
#RF uses dt_split_factor
#GBM uses dt_split_xgb
#EN (no interaction) uses dt_split_xgb
#EN interaction uses dt_split_interactions
#GBM
mod_spec_as_list <- function(mod_id, version="withXB"){
  mod_id_split <- unlist(str_split(mod_id, "\\."))
  mod_name <-mod_id_split[1]
  hyperparam_idx <- as.numeric(mod_id_split[2])
  if (mod_name=="XGB"){
    dt_split = get(paste0("rsplit_OH_",version))
    hyperparams = as.list(xgb_param_sets[hyperparam_idx, ])
  } else if (mod_name=="random_forest"){
    dt_split = get(paste0("rsplit_factor_",version))
    hyperparams = as.list(rf_param_sets[hyperparam_idx, ])
  } else if (mod_name=="elastic_net"){
    dt_split = get(paste0("rsplit_OH_",version))
    hyperparams = as.list(en_param_sets[hyperparam_idx, ])
  } else if (mod_name=="elastic_net_interactions"){
    dt_split = get(paste0("rsplit_ints_",version))
    hyperparams = as.list(enint_param_sets[hyperparam_idx, ])
  }
  return(list(mod_name=mod_name,
              dt_split=dt_split,
              hyperparams = hyperparams))
}

##ENSEMBLE MODEL ASSESSMENT
run_ensemble_assess <- function(base_model_specs, path = "./data/", version="withXB",
                                ensemble_type="CAWPE"){
  outcome<-list()
  outcome_base_mods <- list()
  
  for (row_outer in 1:nrow(base_model_specs[[1]]$dt_split)){
    # #Table to store predictions across each outer fold
    #Table to store predictions across each inner fold
    dt_inner_fold_preds <- data.table(
      "donor_idx"=numeric(),
      "Z0"= numeric(),
      "Z1"= numeric(),
      "Z2"= numeric(),
      "Z3"= numeric(),
      "fu_outcome"= numeric()
    )
    dt_inner_fold_preds_base <- data.table(
      "base_model" = character(),
      "Z0"= numeric(),
      "Z1"= numeric(),
      "Z2"= numeric(),
      "Z3"= numeric(),
      "fu_outcome"= numeric()
    )
    for (row_inner in 1:nrow(base_model_specs[[1]]$dt_split$inner_resamples[[1]])){
      #Table to store predictions across each inner fold
      dt_single_fold_preds <- data.table(
        "base_model" = character(),
        "Set"=character(),
        "Z0"= numeric(),
        "Z1"= numeric(),
        "Z2"= numeric(),
        "Z3"= numeric(),
        "fu_outcome"= numeric()
      )
      #Train base models
      for (base_model_idx in 1:length(base_model_specs)){
        #Generate predictions on single inner fold
        # Keep raw predictions from both train and test data within the fold
        #Save accuracy from training data
        dt_single_fold_preds <- rbind(
          dt_single_fold_preds,
          cbind("base_model" = names(base_model_specs)[base_model_idx],
                mod_eval(rsplit_obj = base_model_specs[[base_model_idx]]$dt_split$inner_resamples[[row_outer]]$splits[[row_inner]],
                         params = base_model_specs[[base_model_idx]]$hyperparams, 
                         mod_name = base_model_specs[[base_model_idx]]$mod_name,
                         return_train_pred=TRUE))
        )
      }
      #
      #
      
      dt_single_fold_preds[, donor_idx := rep(1:(nrow(dt_single_fold_preds)/length(base_model_specs)), length(base_model_specs))]
      dt_single_fold_preds[ , prediction := paste0("Z",max.col(dt_single_fold_preds[,3:6])-1)]
      train_acc_in_fold <- dt_single_fold_preds[Set=="Train", list("train_acc"=sum(prediction==fu_outcome)/.N), by=base_model]
      dt_single_fold_preds<-dt_single_fold_preds[train_acc_in_fold, on="base_model"]
      dt_inner_fold_preds_base<-rbind(dt_inner_fold_preds_base,
                                      dt_single_fold_preds[Set=="Test", .SD, .SDcols=c("base_model", "Z0", "Z1", "Z2", "Z3", "fu_outcome")])
      if(ensemble_type=="CAWPE"){
        # Using Cross-validation accuracy weighted probabilistic ensemble (CAWPE) from Large 2019 DOI 10.1007/s10618-019-00638-y
        
        
        #Calculate ensemble weights based on accuracy from test data

        #dt_single_fold_preds[ , weight_correct :=ifelse(fu_outcome=="Z0", Z0, ifelse(fu_outcome=="Z1", Z1, ifelse(fu_outcome=="Z2", Z2, Z3))),]
        #train_acc_in_fold <- dt_single_fold_preds[Set=="Test", list("train_acc"=sum(weight_correct)/.N), by=base_model]
        
        CAWPE_unnorm <- dt_single_fold_preds[Set=="Test", list(
          "Z0"=sum(train_acc^4*Z0),
          "Z1"=sum(train_acc^4*Z1),
          "Z2"=sum(train_acc^4*Z2),
          "Z3"=sum(train_acc^4*Z3),
          "fu_outcome"=min(fu_outcome)
        ), by=c("donor_idx")]
        dt_inner_fold_preds<-rbind(dt_inner_fold_preds,
                                   CAWPE_unnorm[, list(
                                     "donor_idx" = donor_idx,
                                     "Z0"=Z0/(Z0+Z1+Z2+Z3),
                                     "Z1"=Z1/(Z0+Z1+Z2+Z3),
                                     "Z2"=Z2/(Z0+Z1+Z2+Z3),
                                     "Z3"=Z3/(Z0+Z1+Z2+Z3),
                                     "fu_outcome"=fu_outcome)])
        
      } else {
      #model average
      dt_inner_fold_preds<-rbind(dt_inner_fold_preds,
                                 dt_single_fold_preds[Set=="Test", list(
                                   "Z0"=mean(Z0),
                                   "Z1"=mean(Z1),
                                   "Z2"=mean(Z2),
                                   "Z3"=mean(Z3),
                                   "fu_outcome"=min(fu_outcome)),
                                   by=donor_idx])
      }
    
    }
    #Calc 1vsAll AUC across all folds for ensemble
    outcome[paste0("AUC_",formatC(row_outer, width=2, flag="0"))] <- 
      multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3, data = dt_inner_fold_preds)$auc
    print(paste0("outer fold complete: ",row_outer))
    outcome_base_mods[[paste0("AUC_",formatC(row_outer, width=2, flag="0"))]] <- 
      dt_inner_fold_preds_base[, list("AUC"=AUC(Z0, Z1, Z2, Z3, fu_outcome)), by=base_model]
  }
  
  ####
  dt.results <- as.data.table(outcome)
  dt.results[ , AUC_mean := rowMeans(.SD), .SDcols = 
                paste0("AUC_",formatC(1:15, width=2, flag="0"))]
  
  dt.results_basemods <- as.data.table(outcome_base_mods)
  
  fwrite(dt.results, paste0(path, paste0("ensemble_assess_results_3RF3GBM_",version,"_", ensemble_type,".csv")))
  fwrite(dt.results_basemods, paste0(path, paste0("ensemble_basemods_assess_results_3RF3GBM_",version,"_", ensemble_type,".csv")))

}



outer_fold_assess <- function(base_mod_spec,
                              path = "./data/", version="withXB"){
  
  top_model_aucs <- list()
  
  dt_preds_all_repeats <- data.table(
    "rpt" = numeric(),
    "fold" = numeric(),
    "Z0"= numeric(),
    "Z1"= numeric(),
    "Z2"= numeric(),
    "Z3"= numeric(),
    "fu_outcome"= numeric()
  )
  
  Sys.time()
  for (idx_rpt in 1:3){
    #Table to store predictions across each fold of each resample
    dt_rpt_preds <- data.table(
      "fold" = numeric(),
      "Z0"= numeric(),
      "Z1"= numeric(),
      "Z2"= numeric(),
      "Z3"= numeric(),
      "fu_outcome"= numeric()
    )
    for (idx_fld in 1:5){
      rsplit_obj<-filter(base_mod_spec$dt_split, id==paste0("Repeat",idx_rpt) & id2==paste0("Fold",idx_fld))$splits[[1]]
      dt_rpt_preds <- rbind(
        dt_rpt_preds,
        cbind(
          "fold" = idx_fld,
          mod_eval(rsplit_obj, 
                   base_mod_spec$hyperparams, 
                   base_mod_spec$mod_name, 
                   return_train_pred=FALSE)
        ))
      
      print(paste0("Fold ", idx_fld, " Repeat ", idx_rpt, " complete ",Sys.time()))
    }
    dt_preds_all_repeats <- rbind(dt_preds_all_repeats, 
                                  cbind(
                                    "rpt" = idx_rpt,
                                    dt_rpt_preds))
    #Calc 1vsAll AUC across all folds
    top_model_aucs[paste0("AUC_",formatC(idx_rpt, width=2, flag="0"))] <- multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3, data = dt_rpt_preds)$auc
  }
  fwrite(dt_preds_all_repeats, file = paste0(path,
                                             "top_model_assess_",
                                             version,
                                             ".csv"))
  #dt_preds_all_repeats[ , multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc, by= c("rpt")]
}



outer_fold_assess_ensemble <- function(base_model_specs,
                              path = "./data/", version="withXB"){
  #SIMPLE MODEL AVG- NOW CAWPE
  top_model_aucs <- list()
  
  dt_preds_all_repeats <- data.table(
    "rpt" = numeric(),
    "fold" = numeric(),
    "donor_idx"=numeric(),
    "Z0"= numeric(),
    "Z1"= numeric(),
    "Z2"= numeric(),
    "Z3"= numeric(),
    "fu_outcome"= numeric()
  )
  
  Sys.time()
  for (idx_rpt in 1:3){
    #Table to store predictions across each fold of each resample
    dt_rpt_preds <- data.table(
      "fold" = numeric(),
      "donor_idx"=numeric(),
      "Z0"= numeric(),
      "Z1"= numeric(),
      "Z2"= numeric(),
      "Z3"= numeric(),
      "fu_outcome"= numeric()
    )
    for (idx_fld in 1:5){
      #Table to store predictions across each inner fold
      dt_single_fold_preds <- data.table(
        "base_model" = character(),
        "Z0"= numeric(),
        "Z1"= numeric(),
        "Z2"= numeric(),
        "Z3"= numeric(),
        "fu_outcome"= numeric()
      )
      #Train base models
      for (base_model_idx in 1:length(base_model_specs)){
        #Generate predictions on single inner fold
        # Keep raw predictions from both train and test data within the fold
        #Save accuracy from training data
        rsplit_obj<-filter(base_model_specs[[base_model_idx]]$dt_split, 
                           id==paste0("Repeat",idx_rpt) & 
                             id2==paste0("Fold",idx_fld))$splits[[1]]
        
        
        dt_single_fold_preds <- rbind(
          dt_single_fold_preds,
          cbind("base_model" = names(base_model_specs)[base_model_idx],
                mod_eval(rsplit_obj = rsplit_obj,
                         params = base_model_specs[[base_model_idx]]$hyperparams, 
                         mod_name = base_model_specs[[base_model_idx]]$mod_name,
                         return_train_pred=FALSE))
        )
      }
      
      dt_single_fold_preds[, donor_idx := rep(1:(nrow(dt_single_fold_preds)/length(base_model_specs)), length(base_model_specs))]
      
      #model average
      dt_rpt_preds<-rbind(dt_rpt_preds,
                          cbind("fold"=idx_fld,
                                 dt_single_fold_preds[, list(
                                   "Z0"=mean(Z0),
                                   "Z1"=mean(Z1),
                                   "Z2"=mean(Z2),
                                   "Z3"=mean(Z3),
                                   "fu_outcome"=min(fu_outcome)),
                                   by=donor_idx]))
      
      print(paste0("Fold ", idx_fld, " Repeat ", idx_rpt, " complete ",Sys.time()))
    }
    dt_preds_all_repeats <- rbind(dt_preds_all_repeats, 
                                  cbind(
                                    "rpt" = idx_rpt,
                                    dt_rpt_preds))
    #Calc 1vsAll AUC across all folds
    #top_model_aucs[paste0("AUC_",formatC(idx_rpt, width=2, flag="0"))] <- multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3, data = dt_rpt_preds)$auc
  }
  fwrite(dt_preds_all_repeats, file = paste0(path,
                                             "top_model_assess_",
                                             version,
                                             ".csv"))
}


train_mod <- function(mod_name,
                      params,
                      features){
  if (mod_name=="XGB"){
    features[, fu_outcome := as.numeric(ifelse(nchar(fu_outcome)==2,
                                      substr(fu_outcome, 2, 2),fu_outcome))]
    
    #Extract train and test sets in xgb's special format
    train_matrix = xgb.DMatrix(data = as.matrix(features[,-"fu_outcome"]),
                            label = as.matrix(features[,"fu_outcome"]))
    model=xgb.train(
      params=params,
      data=train_matrix,
      nrounds=10000,
      early_stopping_rounds=10,
      watchlist=list(val1=train_matrix),
      verbose=0,
      booster="gbtree",
      gamma=3,
      objective="multi:softprob",
      eval_metric="mlogloss",
      num_class=4
    )
  } else if (mod_name=="random_forest"){
    model=randomForest(
      fu_outcome~.,
      data = features,
      nodesize = params$nodesize,
      ntree = params$ntree,
      replace = params$replace
    )
  } else if (mod_name=="elastic_net"){
    model=glmnet(
      x = as(data.matrix(features[,-"fu_outcome"]), "dgCMatrix"),
      y = features$fu_outcome,
      lambda = params$lambda,
      alpha = params$alpha,
      family="multinomial"
    )
  } else if (mod_name=="elastic_net_interactions"){
    model=glmnet(
      x = as(data.matrix(features[,-"fu_outcome"]), "dgCMatrix"),
      y = features$fu_outcome,
      lambda = params$lambda,
      alpha = params$alpha,
      family="multinomial"
    )
  }
  return(model)
}



# FEATURE IMORTANCE ----------------------


#Function for calculating overall AUC and each one-vs-rest AUC
auc_calcs <- function(dt.scores){
  #dt.scores<-dt.scores[fu_outcome!="Z-1"]
  dt.scores[, is_Z0 := ifelse(fu_outcome=="Z0",1,0)]
  dt.scores[, is_Z1 := ifelse(fu_outcome=="Z1",1,0)]
  dt.scores[, is_Z2 := ifelse(fu_outcome=="Z2",1,0)]
  dt.scores[, is_Z3 := ifelse(fu_outcome=="Z3",1,0)]
  
  AUCs <- dt.scores[ , list(Overall = multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc,
                            Z0 = roc(is_Z0~Z0)$auc,
                            Z1 = roc(is_Z1~Z1)$auc,
                            Z2 = roc(is_Z2~Z2)$auc,
                            Z3 = roc(is_Z3~Z3)$auc)
  ]
  return(AUCs)
}

# Generate feature list, run  ensemble, return AUCs
gen_metric_row<-function(base_mods,
                         test_data,
                         idx_rpt, idx_fld, feat_name){
  #Generate risk scores on test data - no perturbations
  risk_scores <- risk_scores_ensemble(features_list=gen_features_list(test_data),
                                            base_mods = base_mods)
  
  risk_scores<-cbind(risk_scores,
                     "fu_outcome"=test_data$fu_outcome)
  
  risk_scores[ , prediction := paste0("Z",max.col(risk_scores[,2:5])-1)]
  
  
  #Calc metrics and append to table
  AUCs <- suppressMessages(auc_calcs(risk_scores))
  acc <- risk_scores[, sum(prediction==fu_outcome)/.N]
  
  output = c(idx_rpt, idx_fld, feat_name,
             unlist(AUCs),
             acc)
  return(t(output))
}




##Performing varaible importance across all model selection partitions
# Using the permutation method
ensemble_feature_importance <- function(base_model_specs, dt,
                                       path = "./1_data/",version="noXB"){
  #data table to store results
  dt_feat_metrics <- data.table(
    "rpt" = numeric(),
    "fold" = numeric(),
    "feature"=numeric(),
    "AUC_multi"= numeric(),
    "AUC0vall"= numeric(),
    "AUC1vall"= numeric(),
    "AUC2vall"= numeric(),
    "AUC3vall"= numeric(),
    "accuracy"= numeric()
  )
  
  #rsplit
  set.seed(250)
  rsplit_unformatted <- nested_cv(dt,
                                  outside = vfold_cv(v=5, repeats=3, strata="fu_outcome"),
                                  inside = vfold_cv(v=5, strata="fu_outcome"))
  feat_names <- colnames(dt[,-"fu_outcome"])
  
  Sys.time()
  for (idx_rpt in 1:3){
    for (idx_fld in 1:5){
      #Generate trianing data
      train_data <- data.table(
        analysis(
          filter(
            rsplit_unformatted,
            id==paste0("Repeat",idx_rpt) &
              id2==paste0("Fold",idx_fld))$splits[[1]]
        ))
      features_list <- gen_features_list(train_data)

      
      #Train base models
      base_mods<-list()
      for (mod in names(base_model_specs)){
        print(mod)
        #extract model type and hyperparam set from id
        mod_id_split <- unlist(str_split(mod, "\\."))
        mod_name <-mod_id_split[1]
        
        # #Extract training data for model assessment partition
        # rsplit_obj<-filter(base_model_specs[[mod]]$dt_split, 
        #                    id==paste0("Repeat",idx_rpt) & 
        #                      id2==paste0("Fold",idx_fld))$splits[[1]]
        
        
        if(mod_name=="elastic_net_interactions"){
          features = features_list$Ints
        } else if(mod_name=="random_forest"){
          features = features_list$factor
        } else{
          features = features_list$OH
        }

        
        #Develop base model
        base_mods[[mod]] <- train_mod(mod_name, 
                                      params = base_model_specs[[mod]]$hyperparams,
                                      features = features)
      }
      
      #Extract test set for model assessment partition
      test_data <- data.table(
        assessment(
          filter(
          rsplit_unformatted,
          id==paste0("Repeat",idx_rpt) &
            id2==paste0("Fold",idx_fld))$splits[[1]]
        ))
        
      
      #append baseline metrics to table
      dt_feat_metrics <- rbind(dt_feat_metrics, 
                               gen_metric_row(base_mods, test_data,
                                              idx_rpt, idx_fld, "baseline"), 
                               use.names=FALSE)

      #loop over features to perturb and calculate performance
      for (idx_feat in 1:(ncol(dt)-1)){
        #shuffle selected feature
        dt_temp <- cbind(test_data)[,feat_names[idx_feat] := sample(get(feat_names[idx_feat]),replace=FALSE)]
        #Run model and calculate risk scores
        dt_feat_metrics <- rbind(dt_feat_metrics, 
                                 gen_metric_row(base_mods, dt_temp,
                                                idx_rpt, idx_fld, 
                                                feat_names[idx_feat]), 
                                 use.names=FALSE)
      }
      
      
      
      fwrite(dt_feat_metrics, file = paste0(path,
                                                 "feature_importance_",
                                                 version,
                                                 ".csv"))
      print(paste0("Fold ", idx_fld, " Repeat ", idx_rpt, " complete ",Sys.time()))
    }
  }
}










#  PREDICTING WITH TRAINED MODELS ---------------------
#Generate uncalibrated scores from a model object, name, and features
raw_scores <- function(mod_name, model, features){
  if (mod_name=="XGB"){
    xgb.test = xgb.DMatrix(data = as.matrix(features[,-"fu_outcome"]),
                           label = as.matrix(features[,"fu_outcome"]))
    preds = as.data.table(predict(model, xgb.test, reshape = T))
  } else if (mod_name=="random_forest"){
    preds = as.data.table(predict(model, features, type="prob"))
  } else if (mod_name=="elastic_net"){
    preds = as.data.table(predict(model, newx = data.matrix(features[,-"fu_outcome"]), type = "response")[,,1])
  } else if (mod_name=="elastic_net_interactions"){
    preds = as.data.table(predict(model, newx = data.matrix(features[,-"fu_outcome"]), type = "response")[,,1])
  }
  colnames(preds)<-paste0("Z",0:3)
  return(preds)
}

weight_score <- function(preds, weights){
  preds[, Q0 := weights[1]*Z0/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  preds[, Q1 := weights[2]*Z1/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  preds[, Q2 := weights[3]*Z2/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  preds[, Q3 := weights[4]*Z3/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  

  return(preds[, paste0("Q",0:3)])
}


#Generates calibrated predictions using top model if if it's an XGB model
risk_scores_XGB <- function(features, 
                              model,
                              weights = rep(1,4)
){
  # #ONE HOT
  # dt.xgb <- data.table(model.matrix(fu_outcome ~ ., data=features))
  # dt.xgb <- dt.xgb[ , gender_menstrating_cohortsM := NULL]
  #GBM matrix
  dt.DMatrix = xgb.DMatrix(data = as.matrix(features))
  #Uncalibrated predictions
  preds = as.data.table(predict(model, dt.DMatrix, reshape = T))
  colnames(preds) <- paste0("Z", 0:3)
  return(weight_score(dt_ensemb_pred, weights))
}

#Generates calibrated predictions using top model if if it's an XGB model
risk_scores_rf <- function(features, 
                            model,
                            weights = NA
){
    # rf_fit <- randomForest(
    #   fu_outcome~.,
    #   data = analysis(rsplit_obj),
    #   nodesize = params$nodesize,
    #   ntree = params$ntree,
    #   replace = params$replace
    # )
    #generate predictions
  preds = as.data.table(predict(model, features, type="prob"))
  #Uncalibrated predictions
  colnames(preds) <- paste0("Z", 0:3)
  
  if(is.na(weights)){
    return(preds)
  } else{
    return(weight_score(preds, weights))
  }
}




risk_scores_ensemble <- function(features_list, #3 versions: OH=one hot,
                                  #  factor, Ints=onehot with interaction terms         
                                 base_mods, #LIST OF MODELS WITH MODID AS NAME
                                 weights = NA,
                                 incl_base_preds=FALSE){
  dt_base_preds <- data.table(
    "base_model" = character(),
    "Z0"= numeric(),
    "Z1"= numeric(),
    "Z2"= numeric(),
    "Z3"= numeric()
  )
  
  #Gen predictions for each base model
  for (base_mod_idx in 1:length(base_mods)){
    #extract model name
    mod_name <- unlist(strsplit(names(base_mods)[base_mod_idx], "\\."))[1]
    if(mod_name=="elastic_net_interactions"){
      features = features_list$Ints
    } else if(mod_name=="random_forest"){
      features = features_list$factor
    } else{
      features = features_list$OH
    }
    #Generate raw (uncallibrated) scores and concat
    
    dt_base_preds <- rbind(
      dt_base_preds,
      cbind("base_model" = names(base_mods)[[base_mod_idx]],
            raw_scores(mod_name, 
                       base_mods[[base_mod_idx]], features)))
  }

  dt_base_preds[,donor_idx := rep(1:(nrow(dt_base_preds)/base_mod_idx), base_mod_idx)]
  #model average
  dt_ensemb_pred <- dt_base_preds[, list(
    "Z0"=mean(Z0),
    "Z1"=mean(Z1),
    "Z2"=mean(Z2),
    "Z3"=mean(Z3)),
    by=donor_idx]
  
  if(incl_base_preds==TRUE){
    if(is.na(weights)){
      return(list("ensemble"=dt_ensemb_pred,
                  "base_mods"=dt_base_preds))
    } else{
      return(list("ensemble"=weight_score(dt_ensemb_pred, weights),
                  "base_mods"=dt_base_preds))
    }
  } else {
    if(is.na(weights)){
      return(dt_ensemb_pred)
    } else{
      return(weight_score(dt_ensemb_pred, weights))
    }
  }
  
}





