library(data.table)
source("./2_scripts/utility_functions.R")
#Read in models, dev data
base_mods_noXB <- readRDS("./1_data/mod_final_base_noXB.RDS")
base_mods_withXB <- readRDS("./1_data/mod_final_base_withXB.RDS")
# features_list <- readRDS("./data/model_dev_data/mdset_features_list.RDS")
# dev_labels <- features_list$factor$fu_outcome

#Read in first return dataset and format
dt.fr <- fread("./1_data/first_return_dataset.csv")
dt.fr <- dt.fr[!is.na(FingerstickHGB_equiv)]
dt.fr<-dt.fr[time_to_fu >=56]
dt.frXB <- dt.fr[!is.na(ARUP_Ferritin)]
#for this analysis only using the patients with the extra biomarkers

#FOR first return visits for those with xtra biomarkers (dt.frXB)
# 2 versions: with extra biomarkers (ensemble) and without (RF)
#For first return visits all patients (dt.frAP)
# without extra biomarkers only


# Generate features list for first return dataset
features_list_frXB <- gen_features_list(dt.frXB, withXB = TRUE)
features_list_frexcludeXB <- gen_features_list(dt.frXB, withXB = FALSE)
# features_list_md <- gen_features_list(dt.md, withXB = TRUE)
#Predict on firstreturn dataset
#Predict on all of sim dataset, even those missing labels


#Generate scores(dt.frXB)
dt.scores_frXB <- risk_scores_ensemble(features_list=features_list_frXB,
                     base_mods = base_mods_withXB)

dt.scores_frXB<-cbind(dt.scores_frXB,
                      "fu_outcome"=features_list_frXB$factor$fu_outcome)

dt.scores_frexcludeXB <- risk_scores_ensemble(features_list=features_list_frexcludeXB,
                                              base_mods = base_mods_noXB)

dt.scores_frexcludeXB<-cbind(dt.scores_frexcludeXB, 
                            "fu_outcome"=features_list_frexcludeXB$factor$fu_outcome)

# dt.fr_preds_all <- rbind(dt.frXB_uncalibrated,
#                          dt.frnoXB_uncalibrated)


#Calculate expected distribution
sim_outcome_dist <- table(dt.frXB$fu_outcome)
frac_of_completed <- sim_outcome_dist[c("0", "2", "3")]/sum(sim_outcome_dist[c("0", "2", "3")])

target_dist <- c(sim_outcome_dist["0"] + frac_of_completed["0"]*sim_outcome_dist["-1"],
                 sim_outcome_dist["1"],
                 sim_outcome_dist["2"] + frac_of_completed["2"]*sim_outcome_dist["-1"],
                 sim_outcome_dist["3"] + frac_of_completed["3"]*sim_outcome_dist["-1"] )



gen_cal_weights <- function(dt, target_dist){
    return(
    optim(par = c(1.360566, 0.438853, 1.016506, 1.239142),
          fn = calib_error,
          dt = dt,
          target_dist=target_dist)
  )
}

calib_error <- function(weights, dt, target_dist){#, target_dist=target_dist, dt.fr=dt.fr){
  dt[,Q0 := weights[1]*Z0/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  dt[,Q1 := weights[2]*Z1/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  dt[,Q2 := weights[3]*Z2/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  dt[,Q3 := weights[4]*Z3/( weights[1]*Z0+ weights[2]*Z1+ weights[3]*Z2+ weights[4]*Z3)]
  return(sum(abs(dt[, colSums(.SD), .SDcols = paste0("Q",0:3)] - target_dist)))
}

weight_opt_XB <- gen_cal_weights(dt.scores_frXB, target_dist)
weight_opt_excludeXB <- gen_cal_weights(dt.scores_frexcludeXB, target_dist)

weights<-list(
  XB = weight_opt_XB$par,
  excludeXB = weight_opt_excludeXB$par
)

names(weights$XB) <- paste0("Q", 0:3)
names(weights$excludeXB) <- paste0("Q", 0:3)

saveRDS(weights, "./4_output/calib_weights.RDS")


#ADD CALIBRATED PREDICTIONS TO FRXB DATASET
features_list_frXB[["scores"]]<- dt.scores_frXB
features_list_frexcludeXB[["scores"]]<- dt.scores_frexcludeXB

saveRDS(features_list_frXB, "./1_data/features_scores_frXB.RDS")
saveRDS(features_list_frexcludeXB, "./1_data/features_scores_frexcludeXB.RDS")
