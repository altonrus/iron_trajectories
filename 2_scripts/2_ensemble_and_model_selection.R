library(data.table)
library(pROC)
library(ggplot2)
library(readxl)
library(scales)
library(tidyverse)
library(ggforce)
library(rsample)
library(xgboost)
library(randomForest)
library(gridExtra)
library(glmnet)
theme_set(theme_bw())
source('https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R')


## TUNING RESULTS
#Assessing model selection results
##Read in tuning results
# tune_resultS_AP <- 
#   list.files(path="./3_intermediate/tuning_results", pattern=paste0("*","AP.csv"), full.names = TRUE) %>% 
#   map_df(~fread(.))
tune_results_noXB <- 
  list.files(path="./3_intermediate/tuning_results", pattern="^tun.*.noXB.csv", full.names = TRUE) %>% 
  map_df(~fread(.))
tune_results_XB <- 
  list.files(path="./3_intermediate/tuning_results", pattern="^tun.*.withXB.csv", full.names = TRUE) %>% 
  map_df(~fread(.))


tune_results_all <- rbind(
  cbind(tune_results_noXB, version="Without extra biomarkers"),
  cbind(tune_results_XB, version="With extra biomarkers")
)

tune_results_all[ , AUC_sd := sd(.SD), .SDcols = paste0("AUC_",formatC(1:15, width=2, flag="0")), by=1:nrow(tune_results_all)]
tune_results_all[ , AUC_lb := AUC_mean - AUC_sd]
tune_results_all[ , AUC_ub := AUC_mean + AUC_sd]
tune_results_all[, modelID := paste0(model, ".", hyperparam_idx)]
setorder(tune_results_all, version, -AUC_mean)
fwrite(tune_results_all, "./3_intermediate/tuning_results/tuning_results_all.csv")


# PLOT TUNING RESULTS FOR ALL HYPERPARAMETER SETS
# ggplot(tune_results_all, aes(x = model, y = AUC_mean, fill=version, color=version))+
#   geom_sina()+
#   theme(legend.position = "bottom")+
#   ylab("Mean overall AUC\ncross validation across 15 tuning sets")+xlab("Model types")+
#   scale_x_discrete(labels = c("Elastic Net", "Elastic net\nwith interactions", "Random forest", "Regression trees", "Gradient\nboosted trees"))
# 

mod_names <- c("Gradient boosted trees", 
               "Random forest",
               "Elastic net with interactions", 
               "Elastic Net", 
               "Regression trees")
names(mod_names)<-c("XGB",
                    "random_forest", 
                    "elastic_net_interactions",
                    "elastic_net",
                    "rpart")

top_mods <- tune_results_all[, list(top_mean_AUC = max(AUC_mean)), by = version]
tune_results_all[, model := factor(model, levels = names(mod_names))]

ggplot(tune_results_all, aes(x = version, y = AUC_mean, fill=version))+
  facet_wrap(vars(model), ncol=1, labeller = labeller(model=mod_names))+
  coord_flip()+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8)+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=version))+
  scale_x_discrete(name="", labels=c("",""))+
  scale_y_continuous(name="Mean AUC of each candidate model configuration", labels = percent_format(),
                     limits=c(.5,1))+
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank())+
  geom_hline(data=top_mods, aes(color=version, yintercept = top_mean_AUC))
  
ggsave("./4_output/figs/AUC_tuning_no_ensemble.png",
       width = 6,
       height = 5.5,
       unit = "in")

# # # # # 
#### SELECT MODELS FOR ENSEMBLE & CONSTRUCT ####
# # # # #

# 5 models:
# Top RF, EN, EN with interactions 
# 2 GBM within 1 SD of top that are the least correlated


#vers = "Without extra biomarkers"

tune_results_all <- fread("./3_intermediate/tuning_results/tuning_results_all.csv")

top_mods <- function(tune_results_all, vers,
                     ensemb_type="3GBM3FR"){
  setorder(tune_results_all, version, -AUC_mean)
  #2 GBM with mean AUC within SD of top that are least correlated
  Top_gbm_mean_AUC <- unlist(tune_results_all[version==vers & model=="XGB", "AUC_mean"][1])
  Top_gbm_AUC_sd <- unlist(tune_results_all[version==vers & model=="XGB", "AUC_sd"][1])
  top_gbms <- tune_results_all[version==vers & model=="XGB" & 
                                 AUC_mean >= (Top_gbm_mean_AUC - Top_gbm_AUC_sd),]
  top_gbms<-top_gbms[, .SD, .SDcols=c("modelID", paste0("AUC_",str_pad(1:15, 2, pad="0")))]
  top_gbms<-dcast(melt(top_gbms, id.vars="modelID"), variable ~ modelID)
  c_gbm <- cor(top_gbms[,-1])
  ## c is the correlations matrix
  ## keep only the lower triangle by 
  ## filling upper with NA
  c_gbm[upper.tri(c_gbm, diag=TRUE)] <- NA
  m_gbm <- data.table(melt(c_gbm))
  ## sort by descending absolute correlation
  m_gbm <- m_gbm[order(abs(m_gbm$value)), ]
  
  
  #2 RFS with mean AUC within SD of top that are least correlated
  Top_fr_mean_AUC <- unlist(tune_results_all[version==vers & model=="random_forest", "AUC_mean"][1])
  Top_rf_AUC_sd <- unlist(tune_results_all[version==vers & model=="random_forest", "AUC_sd"][1])
  top_rfs <- tune_results_all[version==vers & model=="random_forest" & 
                                 AUC_mean >= (Top_fr_mean_AUC - Top_rf_AUC_sd),]
  top_rfs<-top_rfs[, .SD, .SDcols=c("modelID", paste0("AUC_",str_pad(1:15, 2, pad="0")))]
  top_rfs<-dcast(melt(top_rfs, id.vars="modelID"), variable ~ modelID)
  c_rf <- cor(top_rfs[,-1])
  ## c is the correlations matrix
  ## keep only the lower triangle by 
  ## filling upper with NA
  c_rf[upper.tri(c_rf, diag=TRUE)] <- NA
  m_rf <- data.table(melt(c_rf))
  ## sort by descending absolute correlation
  m_rf <- m_rf[order(abs(m_rf$value)), ]
  
  
  if(ensemb_type=="3GBM3FR"){
    mods_for_ensemble <- c(tune_results_all[version==vers & model=="XGB", "modelID"][[1]][1],
                           as.character(m_gbm[1, Var1]),
                           as.character(m_gbm[1, Var2]),
                           tune_results_all[version==vers & model=="random_forest", "modelID"][[1]][1],
                           as.character(m_rf[1, Var1]),
                           as.character(m_rf[1, Var2]))
  } else {
    mods_for_ensemble <- c(as.character(m_gbm[1, Var1]),
                           as.character(m_gbm[1, Var2]),
                           tune_results_all[version==vers & model=="random_forest", modelID][1],
                           tune_results_all[version==vers & model=="elastic_net", modelID][1],
                           tune_results_all[version==vers & model=="elastic_net_interactions", modelID][1]
    )
  }

  return(mods_for_ensemble)
}

mods_for_ensemble_XB <- top_mods(tune_results_all, "With extra biomarkers",
                                 ensemb_type="all_mods")
mods_for_ensemble_noXB <- top_mods(tune_results_all, "Without extra biomarkers")



#read in splits and hyperparam sets
xgb_param_sets <- fread("./3_intermediate/hyperparam_sets/xgb_param_sets.csv")
rf_param_sets<-fread("./3_intermediate/hyperparam_sets/rf_param_sets.csv")
en_param_sets<-fread("./3_intermediate/hyperparam_sets/en_param_sets.csv")
enint_param_sets<-fread("./3_intermediate/hyperparam_sets/enint_param_sets.csv")

rsplit_factor_withXB<- readRDS("./1_data/rsplits/rsplit_factors_withXB.rds")
rsplit_factor_noXB<-readRDS("./1_data/rsplits/rsplit_factors_noXB.rds")
rsplit_OH_withXB<-readRDS("./1_data/rsplits/rsplit_OH_withXB.rds")
rsplit_OH_noXB<-readRDS("./1_data/rsplits/rsplit_OH_noXB.rds")
rsplit_ints_withXB<-readRDS("./1_data/rsplits/rsplit_ints_withXB.rds")
rsplit_ints_noXB<-readRDS("./1_data/rsplits/rsplit_ints_noXB.rds")


#COMPILE LIST OF MODELS FOR ENSEMBLE
base_mod_spec_XB <- list()
for(mod_id in mods_for_ensemble_XB){
  base_mod_spec_XB[[mod_id]]<-mod_spec_as_list(mod_id, version="withXB")
}
base_mod_spec_noXB <- list()
for(mod_id in mods_for_ensemble_noXB){
  base_mod_spec_noXB[[mod_id]]<-mod_spec_as_list(mod_id, version="noXB")
}

saveRDS(base_mod_spec_XB, "./1_data/base_mod_spec_XB.RDS")
saveRDS(base_mod_spec_noXB, "./1_data/base_mod_spec_noXB.RDS")

#Run ensemble assessment
source("2_scripts/utility_functions.R")
run_ensemble_assess(base_mod_spec_noXB, path="./3_intermediate/tuning_results/", version="noXB")
run_ensemble_assess(base_mod_spec_XB, path="./3_intermediate/tuning_results/", version="withXB")

run_ensemble_assess(base_mod_spec_noXB, path="./3_intermediate/tuning_results/", version="noXB",
                    ensemble_type="average")
run_ensemble_assess(base_mod_spec_XB, path="./3_intermediate/tuning_results/", version="withXB",
                    ensemble_type="average")


#Compare ensemble to individual top models
ensemble_noXB <- fread("./3_intermediate/tuning_results/ensemble_assess_results_3RF3GBM_noXB_average.csv")
ensemble_XB <- fread("./3_intermediate/tuning_results/ensemble_assess_results_withXB_average.csv")

tune_results_all<-fread("./3_intermediate/tuning_results/tuning_results_all.csv")

top_mods_ensemble<-rbind(
  tune_results_all[modelID %in% mods_for_ensemble_XB &
                     version=="With extra biomarkers", .SD,
                   .SDcols=c("version", "modelID", paste0("AUC_",c(str_pad(1:15, 2, pad="0"), "mean")))],
  cbind(version="With extra biomarkers",
        modelID="Ensemble", 
        ensemble_XB),
  tune_results_all[modelID %in% mods_for_ensemble_noXB &
                     version=="Without extra biomarkers", .SD,
                   .SDcols=c("version", "modelID", paste0("AUC_",c(str_pad(1:15, 2, pad="0"), "mean")))],
  cbind(version="Without extra biomarkers",
        modelID="Ensemble", 
        ensemble_noXB)
)

top_mods_ensemble[, top:=ifelse(AUC_mean==max(AUC_mean),"Top",""), by=version]

top_mods_ensemble_plt<-melt(top_mods_ensemble[,.SD,.SDcols=!c("AUC_mean")], 
                            id.vars=c("version","modelID","top"))

top_mods_ensemble_plt[, modelID:=factor(modelID, 
                                        levels=c("Ensemble",
                                                 "elastic_net.1",
                                                 "elastic_net_interactions.4",
                                                 "random_forest.76",
                                                 "random_forest.39",
                                                 "random_forest.4",
                                                 "random_forest.46",
                                                 "XGB.684",
                                                 "XGB.1267",
                                                 "XGB.109",
                                                 "elastic_net.2",
                                                 "elastic_net_interactions.16",
                                                 "XGB.55","XGB.851"
                                                 ))]


ggplot(top_mods_ensemble_plt, aes(x = modelID, y = value, fill=top,))+
  facet_wrap(vars(version), nrow=2, scales="free_y")+coord_flip()+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8)+
  geom_point(position = position_jitter(width = .15), size = .5, alpha = 0.8,
             aes(color=top))+
  stat_summary(fun = mean, geom = "point",
               position = position_nudge(x = .2, y = 0)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_nudge(x = .2, y = 0))+
  xlab("")+
  scale_y_continuous(name="AUC for 15 tuning sets (Mean, standard error, and distribution)", labels = percent_format())+
  theme(legend.position = "None")

ggsave("./4_output/figs/AUC_top_models.png",
       width = 6,
       height = 4.5,
       unit = "in")




# # # # # 
#### ASSESS TOP MODELS (withXB and noXB) on outer folds ####
# # # # #

#outer_fold_assess(base_mod_spec = base_mod_spec_noXB$random_forest.39, version="withXB")
#outer_fold_assess(base_mod_spec = base_mod_spec_noXB$random_forest.39, version="noXB")
outer_fold_assess_ensemble(base_model_specs = base_mod_spec_XB, 
                           path = "./3_intermediate/",
                           version="withXB")
outer_fold_assess_ensemble(base_model_specs = base_mod_spec_noXB, 
                           path = "./3_intermediate/",
                           version="noXB")
#TOP OVERALL MODEL
#Multiclass AUCs (avg pairwise Hand and Till 2001)
dt_outer_preds_all_repeats <- rbind(
  cbind(version="withXB",
        fread("./3_intermediate/top_model_assess_withXB.csv")),
        cbind(version="noXB",
              fread("./3_intermediate/top_model_assess_noXB.csv")))

dt_outer_preds_all_repeats[, is_Z0 := ifelse(fu_outcome=="Z0",1,0)]
dt_outer_preds_all_repeats[, is_Z1 := ifelse(fu_outcome=="Z1",1,0)]
dt_outer_preds_all_repeats[, is_Z2 := ifelse(fu_outcome=="Z2",1,0)]
dt_outer_preds_all_repeats[, is_Z3 := ifelse(fu_outcome=="Z3",1,0)]

AUCs <- dt_outer_preds_all_repeats[ , list(Overall = multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc,
                                     Z0 = roc(is_Z0~Z0)$auc,
                                     Z1 = roc(is_Z1~Z1)$auc,
                                     Z2 = roc(is_Z2~Z2)$auc,
                                     Z3 = roc(is_Z3~Z3)$auc),
                      by= c("version", "rpt")]


AUCs_mean <- AUCs[,lapply(.SD, mean), by=version, .SDcols=c("Overall", paste0("Z",0:3))]



roc_objects <- list()
outcomes <- c("None", "HGB_defer", "Low", "Absent")

for (vsn in c("withXB", "noXB")){
  for (outcome_num in 0:3){
    for (rpt_idx in 1:3){
      response = dt_outer_preds_all_repeats[version== vsn & rpt == rpt_idx, 
                                      fifelse(fu_outcome==paste0("Z",outcome_num),1,0)]
      predictor = dt_outer_preds_all_repeats[version== vsn & rpt == rpt_idx, 
                                       .SD, 
                                       .SDcols = c(paste0("Z",outcome_num))][[1]]
      roc_obj <- roc(response = response, predictor = predictor)
      roc_objects[[vsn]][paste0(outcomes[outcome_num+1],"_",rpt_idx)] <- list(roc_obj)
    }
  }
}



ROC_1vall_withXB<- ggroc(roc_objects$withXB) + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
  theme(legend.position = "none")+
  ggtitle("With extra biomarkers")+
  #guides(col = guide_legend(nrow = 2))+
  scale_color_manual(
    values = c("turquoise2", "turquoise2", "turquoise2",
               "yellow2", "yellow2", "yellow2",
               "darkorange1", "darkorange1", "darkorange1",
               "red1","red1", "red1"),
    labels = rep(c("None", "HGB deferral", "Low iron donation", "Absent iron donation"),each=3))+
  xlab("Specificity")+ylab("Sensitivity")+
  geom_point(aes(x=0.75, y=0.75), fill="black", color="black", size = 2)

# ggsave("ROC_withXB.png",
#        width = 4.5,
#        height = 4.5,
#        unit = "in")

ROC_1vall_noXB<- ggroc(roc_objects$noXB) + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
  theme(legend.position = "none")+
  ggtitle("Without extra biomarkers")+
  #guides(col = guide_legend(nrow = 2))+
  scale_color_manual(
    values = c("turquoise2", "turquoise2", "turquoise2",
               "yellow2", "yellow2", "yellow2",
               "darkorange1", "darkorange1", "darkorange1",
               "red1","red1", "red1"),
    labels = rep(c("None", "HGB deferral", "Low iron donation", "Absent iron donation"),each=3))+
  xlab("Specificity")+ylab("Sensitivity")+
  geom_point(aes(x=0.75, y=0.75), fill="black", color="black", size = 2)


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  }

mylegend<-g_legend(
  ggplot(data = data.table(Cat=c("None", "HGB deferral", "Low iron donation", "Absent iron donation")))+
    geom_bar(aes(x=Cat, fill=Cat))+
    scale_fill_manual(values=c("turquoise2","yellow2","darkorange1","red1"),
                               name="")+
    guides(fill=guide_legend(nrow=1))
)



ggsave("./4_output/figs/ROC_compare.png",
       plot=grid.arrange(arrangeGrob(ROC_1vall_noXB,ROC_1vall_withXB,nrow=1),
                         mylegend,nrow=2, heights=c(7,1)),
       width = 6.5,
       height = 3.5,
       unit = "in")



#AUCS by fold
AUCs_by_fold <- dt_outer_preds_all_repeats[ , list(Overall = multiclass.roc(fu_outcome~Z0+Z1+Z2+Z3)$auc,
                                           Z0 = roc(is_Z0~Z0)$auc,
                                           Z1 = roc(is_Z1~Z1)$auc,
                                           Z2 = roc(is_Z2~Z2)$auc,
                                           Z3 = roc(is_Z3~Z3)$auc),
                                    by= c("version", "rpt", "fold")]


AUCs_by_fold_mean_CI <- AUCs_by_fold[ , list(
  overall_mean = mean(Overall),
  overall_lb = mean(Overall) - sd(Overall)/sqrt(.N),
  overall_ub = mean(Overall) + sd(Overall)/sqrt(.N),
  Z0_mean = mean(Z0),
  Z0_lb = mean(Z0) - sd(Z0)/sqrt(.N),
  Z0_ub = mean(Z0) + sd(Z0)/sqrt(.N),
  Z1_mean = mean(Z1),
  Z1_lb = mean(Z1) - sd(Z1)/sqrt(.N),
  Z1_ub = mean(Z1) + sd(Z1)/sqrt(.N),
  Z2_mean = mean(Z2),
  Z2_lb = mean(Z2) - sd(Z2)/sqrt(.N),
  Z2_ub = mean(Z2) + sd(Z2)/sqrt(.N),
  Z3_mean = mean(Z3),
  Z3_lb = mean(Z3) - sd(Z3)/sqrt(.N),
  Z3_ub = mean(Z3) + sd(Z3)/sqrt(.N)),
  by = "version"]

fwrite(AUCs_by_fold_mean_CI, "./4_output/AUC_results_meanCI.csv")






