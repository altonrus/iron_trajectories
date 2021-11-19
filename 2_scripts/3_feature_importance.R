library(data.table)
library(pROC)
library(ggplot2)
library(scales)
source("2_scripts/utility_functions.R")
theme_set(theme_bw())

# # Run feature importance calculations - no XB
# base_mod_specs_noXB <- readRDS("./1_data/base_mod_spec_noXB.RDS")
# dt.noXB <- fread("./1_data/model_dev_data/mdset_factors_noXB.csv")
# ensemble_feature_importance(
#   base_mod_specs_noXB,
#   dt.noXB,
#   path = "./1_data/"
# )
# 
# # Run feature importance calculations - with XB
# dt.withXB <- fread("./1_data/model_dev_data/mdset_factors_withXB.csv")
# base_mods_specs_withXB <- readRDS("./1_data/base_mod_spec_XB.RDS")
# ensemble_feature_importance(
#   base_mods_specs_withXB,
#   dt.withXB,
#   path = "./1_data/",
#   version="withXB"
# )


# PROCESS DATA


featimp_noXB <- fread("./4_output/feature_importance_noXB.csv")
featimp_XB <- fread("./4_output/feature_importance_withXB.csv")
featname_lookup <- fread("./1_data/feature_name_lookup.csv")

#No XB
baselines_noXB <- featimp_noXB[feature == "baseline"]
colnames(baselines_noXB)[4:9] <- paste0("bl_",colnames(baselines_noXB)[4:9])
featimp_noXB <-featimp_noXB[feature != "baseline"]

setDT(featimp_noXB)[baselines_noXB, 
                    AUC_multi_pctchg := (bl_AUC_multi - AUC_multi)/bl_AUC_multi, 
                    on=.(rpt, fold)]
featimp_noXB<-featimp_noXB[featname_lookup, on="feature",nomatch=0]



ggplot(featimp_noXB)+
  geom_boxplot(aes(x=reorder(display_name, AUC_multi_pctchg, FUN = median), y=AUC_multi_pctchg))+
  coord_flip()+geom_hline(yintercept=0, color="red")+
  scale_y_continuous(labels = label_percent(), name = "Decrease in multiclass AUC")+
  xlab("")

ggsave("./4_output/figs/feat_imp_noXB.png",
       width = 5, height = 6, units = "in")

median_noXB <- featimp_noXB[, list(median_imp = median(AUC_multi_pctchg)), by=display_name][order(-median_imp),]
top15_noXB <- median_noXB$display_name[1:15]

ggplot(featimp_noXB[display_name %in% top15_noXB])+
  geom_boxplot(aes(x=reorder(display_name, AUC_multi_pctchg, FUN = median), y=AUC_multi_pctchg))+
  coord_flip()+geom_hline(yintercept=0, color="red")+
  scale_y_continuous(labels = label_percent(), name = "Decrease in multiclass AUC")+
  xlab("")

ggsave("./4_output/figs/feat_imp_noXB_top15.png",
       width = 5, height = 4, units = "in")

#With XB
baselines_XB <- featimp_XB[feature == "baseline"]
colnames(baselines_XB)[4:9] <- paste0("bl_",colnames(baselines_XB)[4:9])
featimp_XB <-featimp_XB[feature != "baseline"]

setDT(featimp_XB)[baselines_XB, 
                  AUC_multi_pctchg := (bl_AUC_multi - AUC_multi)/bl_AUC_multi, 
                  on=.(rpt, fold)]

featimp_XB<-featimp_XB[featname_lookup, on="feature",nomatch=0]


# ggplot(featimp_XB, aes(y=reorder(feature, AUC_multi_pctchg, FUN = median), 
#                          x=AUC_multi_pctchg))+
#   geom_density_ridges()+
#   geom_vline(xintercept=0)

ggplot(featimp_XB)+
  geom_boxplot(aes(x=reorder(display_name, AUC_multi_pctchg, FUN = median), y=AUC_multi_pctchg))+
  coord_flip()+geom_hline(yintercept=0, color="red")+
  scale_y_continuous(labels = label_percent(), name = "Decrease in multiclass AUC")+
  xlab("")

ggsave("./4_output/figs/feat_imp_XB.png",
       width = 5, height = 6, units = "in")

median_XB <- featimp_XB[, list(median_imp = median(AUC_multi_pctchg)), by=display_name][order(-median_imp),]
top15_XB <- median_XB$display_name[1:15]

ggplot(featimp_XB[display_name %in% top15_XB])+
  geom_boxplot(aes(x=reorder(display_name, AUC_multi_pctchg, FUN = median), y=AUC_multi_pctchg))+
  coord_flip()+geom_hline(yintercept=0, color="red")+
  scale_y_continuous(labels = label_percent(), name = "Decrease in multiclass AUC")+
  xlab("")

ggsave("./4_output/figs/feat_imp_XB_top15.png",
       width = 5, height = 4, units = "in")


#Combined fig
featimp_both <- rbind(
  cbind(mod = "Extra biomarkers", featimp_XB),
  cbind(mod = "Standard biomarkers", featimp_noXB)
)


ggplot(featimp_both[display_name %in% c(top15_noXB, top15_XB)])+
  geom_boxplot(aes(x=reorder(display_name, AUC_multi_pctchg, FUN = median), y=AUC_multi_pctchg))+
  facet_grid(cols = vars(mod))+
  coord_flip()+geom_hline(yintercept=0, color="red")+
  scale_y_continuous(labels = label_percent(), name = "Decrease in multiclass AUC")+
  xlab("")

ggsave("./4_output/figs/feat_imp_both_top15.png",
       width = 5, height = 4, units = "in")


#combined median table
featimp_XB_median <- featimp_XB[, list(median_AUC_pctchg = median(AUC_multi_pctchg)), by = feature]
featimp_noXB_median <- featimp_noXB[, list(median_AUC_pctchg = median(AUC_multi_pctchg)), by = feature]

featimp_median <- rbind(
  cbind(featimp_XB_median, model = "XB"),
  cbind(featimp_noXB_median, model = "noXB")
)
fwrite(featimp_median, "./4_output/feature_importance_medians.csv")


#Mini plot for Grant
feats_for_mini <- c("Ferritin", median_noXB[median_imp >.005]$display_name)
dt_mini <- featimp_both[display_name %in% feats_for_mini]
dt_mini[, mod:=ifelse(mod=="Extra biomarkers", "With\nferritin", "Without")]

ggplot(dt_mini)+
  geom_boxplot(aes(x=reorder(display_name, AUC_multi_pctchg, FUN = median), y=AUC_multi_pctchg))+
  facet_grid(cols = vars(mod))+
  coord_flip()+geom_hline(yintercept=0, color="red")+
  scale_y_continuous(labels = label_percent(accuracy=1), breaks = c(0, .03, .06), name = "Relative variable\nimportance")+
  xlab("")
ggsave("./4_output/figs/feat_imp_mini.png",
       width = 3.4, height = 2, units = "in")
