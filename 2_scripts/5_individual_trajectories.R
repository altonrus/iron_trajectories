library(data.table)
library(ggplot2)
library(ggExtra)
library(cowplot)
library(scales)
library(grid)
library(gridExtra)
theme_set(theme_bw())
source("2_scripts/utility_functions.R")

# #Read in data
# weights <- readRDS("./4_output/calib_weights.RDS")
features_list_frXB<-readRDS("./1_data/features_scores_frXB.RDS")
# features_list_frexcludeXB<-readRDS("./1_data/features_scores_frexcludeXB.RDS")
# 
# #Read in models
# base_mods_noXB <- readRDS("./1_data/mod_final_base_noXB.RDS")
# base_mods_withXB <- readRDS("./1_data/mod_final_base_withXB.RDS")

dt.fr <- fread("./1_data/first_return_dataset.csv")
dt.fr <- dt.fr[!is.na(FingerstickHGB_equiv)]
dt.fr<-dt.fr[time_to_fu >=56]
dt.frXB <- dt.fr[!is.na(ARUP_Ferritin)]
##
# GENERATE RISK TRAJECTORIES MATRIX -------------------
##

# Generate risk estimate as 3d array --------------------------
# Dim 1 has lenght n_donors where m is # donors
# Dim 2 has length max_IDI - min_IDI + 1
# Dim 3 has length 4 for donation outcomes (Y_0 = no adverse-iron outcome; Y
#                                           Y_1 = Hg deferral; Y
#                                           Y_2 = low iron donation; 
#                                           Y_3 = absent iron stores donation)


#Risk trajectory matrix
n_donations = nrow(dt.frXB)
max_IDI = 250; min_IDI = 56
# 
# #CREATE RISK MATRIX WITH EXTRA BIOMARKERS
# risk_matrix_XB <- array(data=NA,
#                      dim = c(nrow(features_list_frXB$factor), max_IDI - min_IDI + 1, 4),
#                      dimnames = list(Idx_donation = 1:nrow(features_list_frXB$factor),
#                                      time_to_fu = min_IDI:max_IDI,
#                                      outcome = c("None", "HGB_defer", "Low", "Absent"))
#                      )
# 
# dt.temp<-rbind(dt.frXB)
# for (t_return in min_IDI:max_IDI){
#   feature_list_temp <- gen_features_list(dt.temp[ , time_to_fu := t_return],withXB=TRUE)
#   preds <- risk_scores_ensemble(features_list=feature_list_temp,
#                                 base_mods = base_mods_withXB,
#                                 weights = weights$XB)
# 
#   risk_matrix_XB[, as.character(t_return), ] <- data.matrix(preds)
#   if(t_return %% 50 == 0){print(t_return)}
# }
# 
# saveRDS(risk_matrix_XB, "./4_output/3d_risk_matrix_XB.RDS")
risk_matrix_XB<- readRDS("./4_output/3d_risk_matrix_XB.RDS")

# #COMPARE DAY 56 and 250 BASE MODELS
# t_return=56
# feature_list_temp <- gen_features_list(dt.temp[ , time_to_fu := t_return],withXB=TRUE)
# preds_56 <- risk_scores_ensemble(features_list=feature_list_temp,
#                               base_mods = base_mods_withXB,
#                               weights = weights$XB,
#                               incl_base_preds = TRUE)
# 
# t_return=250
# feature_list_temp <- gen_features_list(dt.temp[ , time_to_fu := t_return],withXB=TRUE)
# preds_250 <- risk_scores_ensemble(features_list=feature_list_temp,
#                                  base_mods = base_mods_withXB,
#                                  weights = weights$XB,
#                                  incl_base_preds = TRUE)


# # #CREATE RISK MATRIX WITHOUT EXTRA BIOMARKERS
# risk_matrix_excludeXB <- array(data=NA,
#                         dim = c(nrow(features_list_frXB$factor), max_IDI - min_IDI + 1, 4),
#                         dimnames = list(Idx_donation = 1:nrow(features_list_frXB$factor),
#                                         time_to_fu = min_IDI:max_IDI,
#                                         outcome = c("None", "HGB_defer", "Low", "Absent"))
# )
# dt.temp<-rbind(dt.frXB)
# for (t_return in min_IDI:max_IDI){
#   feature_list_temp <- gen_features_list(dt.temp[ , time_to_fu := t_return],withXB=FALSE)
#   preds <- risk_scores_ensemble(features_list=feature_list_temp,
#                                 base_mods = base_mods_noXB,
#                                 weights = weights$XB)
# 
#   risk_matrix_excludeXB[, as.character(t_return), ] <- data.matrix(preds)
# }
# saveRDS(risk_matrix_excludeXB, "./4_output/3d_risk_matrix_excludeXB.RDS")
# risk_matrix_excludeXB<- readRDS("./4_output/3d_risk_matrix_excludeXB.RDS")
# 
# #Risk day 56 to 250
# 1-summary(risk_matrix_excludeXB[ , "56", "None"])
# 1-summary(risk_matrix_excludeXB[ , "250", "None"])
# summary(risk_matrix_excludeXB[ , "250", "None"] - risk_matrix_excludeXB[ , "56", "None"])
# 
# 


##
# ANALYZE TRAJECTORIES -----------
##


#Calculate risk any AE day 56 and day 250 and plot
#Extract p(any AE) on day 56 and day 250 from risk matrix
dt.firstreturn <- cbind(features_list_frXB$factor,
      "Any_AE_day_56" = 1-risk_matrix_XB[ , "56", "None"],
      "Any_AE_day_250" = 1-risk_matrix_XB[ , "250", "None"]
)

summary(dt.firstreturn$Any_AE_day_56)
summary(dt.firstreturn$Any_AE_day_250)

fwrite(dt.firstreturn, "./1_data/dt_fr_withrisk")

##SCATTER OF day 56 vs. day 250
plot <- ggplot(data=dt.firstreturn, 
               aes(x=Any_AE_day_56, y=Any_AE_day_250))+
  geom_point(alpha=0.3, size = 0.5)+
  #geom_rug()+
  geom_abline(slope=1, intercept=0, color="red")+
  scale_x_continuous(breaks = seq(0,1,.1), limits = c(0,1), labels = label_percent(accuracy=2))+
  scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1), labels = label_percent(accuracy=2))+
  xlab("Prob. any adverse event\n56 days after index donation")+
  ylab("Prob. any adverse event\n250 days after index donation")

plot <- ggMarginal(plot, type="histogram", bins=11, size = 8)

ggsave("4_output/figs/trajectory_56_vs_250.png",
       plot,
       width = 4.6,
       height = 4.5,
       units = "in"
)


##Individual trajectories

am_adt <- function(inarray) { #turns 3d matrix into 2d
  if (!is.array(inarray)) stop("input must be an array")
  dims <- dim(inarray)
  if (is.null(dimnames(inarray))) {
    inarray <- provideDimnames(inarray, base = list(as.character(seq_len(max(dims)))))
  }
  FT <- if (any(class(inarray) %in% "ftable")) inarray else ftable(inarray) 
  out <- data.table(as.table(ftable(FT)))
  nam <- names(out)[seq_along(dims)]
  setorderv(out[, (nam) := lapply(.SD, type.convert), .SDcols = nam], nam)[]
}

dt_long_risk_matrix <- am_adt(risk_matrix_XB)
setnames(dt_long_risk_matrix, "N", "Risk")
set.seed(998)
dt_long_risk_matrix_sm <- dt_long_risk_matrix[Idx_donation %in% sample.int(3685, 300)]
dt_long_risk_matrix_sm[, ex_group := ""]

#Quick recoverer
Idx_quick_recoverers <- dt_long_risk_matrix_sm[time_to_fu==56 & outcome=="None"&Risk>.9, unique(Idx_donation)] 
Idx_chronic <- dt_long_risk_matrix_sm[time_to_fu==250 & outcome=="None" & Risk<.15, unique(Idx_donation)] #Chronic high-risk
idx_low_start <- dt_long_risk_matrix_sm[time_to_fu==56 & outcome=="None"&Risk<.35]$Idx_donation
Idx_slow_recoverers <- dt_long_risk_matrix_sm[Idx_donation %in% idx_low_start & 
                         outcome=="None" &
                         time_to_fu==250 &
                         Risk>.60, unique(Idx_donation)] #slow recoverer



# Idx_chronic <- c(8, 304, 712, 1763, 3267)
# Idx_quick_recoverers <- c(1372, 1750, 3036, 3684, 1374)
# Idx_slow_recoverers <- c(129, 394, 2278, 2978, 3596)
dt_long_risk_matrix_sm[,  ex_group := ifelse(Idx_donation %in% tail(Idx_chronic,5), 
                                             "Chronic high risk",
                                             ex_group)]
dt_long_risk_matrix_sm[,  ex_group := ifelse(Idx_donation %in% tail(Idx_quick_recoverers,5), 
                                             "Quick recoverer",
                                             ex_group)]
dt_long_risk_matrix_sm[,  ex_group := ifelse(Idx_donation %in% tail(Idx_slow_recoverers,5), 
                                             "Slow recoverer",
                                             ex_group)]

dt_any_AE <- cbind(dt_long_risk_matrix_sm[outcome=="None", ])
dt_any_AE[, Risk := 1-Risk]
dt_any_AE[, outcome := "Any adverse event"]
p_top <- ggplot(dt_any_AE, aes(x=time_to_fu, y=Risk))+
  geom_line(aes(group = Idx_donation, color = ex_group, 
                alpha= ex_group, size = ex_group))+
  facet_wrap(vars(outcome), nrow=1)+
  scale_y_continuous(name="Probability", labels=percent_format())+
  scale_color_manual(values=c("grey", "red2", "springgreen4", "darkorange3"), name="Example trajectories")+
  scale_x_continuous(limits = c(56, 250), breaks = c(56, 150, 250), name="")+
  scale_alpha_manual(values=c(0.4, 1, 1, 1), name="Example trajectories")+
  scale_size_manual(values=c(0.2, 1, 1, 1))+
  theme(legend.position = "None")

outcome_labs <-c(
  `Absent` = "Absent iron",
  `Low` = "Low iron",
   `HGB_defer` = "HGB deferral"
)


p_bottom <- ggplot(dt_long_risk_matrix_sm[outcome!="None"], aes(x=time_to_fu, y=Risk))+
  geom_line(aes(group = Idx_donation, color = ex_group, 
                alpha= ex_group, size = ex_group))+
  facet_wrap(vars(outcome), nrow=1, labeller = as_labeller(outcome_labs))+
  scale_y_continuous(name="Probability", labels=percent_format())+
  scale_x_continuous(limits = c(56, 250), breaks = c(56, 100, 150, 200, 250), name="Days until next donation")+
  scale_color_manual(values=c("grey", "red2", "springgreen4", "darkorange3"), name="Example\nTrajectories")+
  scale_alpha_manual(values=c(0.4, 1, 1, 1), name="Example\nTrajectories")+
  scale_size_manual(values=c(0.2, 1, 1, 1), name="Example\nTrajectories")+
  theme(legend.position = "bottom")#+
  #guides(color=guide_legend(nrow=2,byrow=TRUE))


grid.arrange(p_top, p_bottom, ncol=1)
ggsave("4_output/figs/each_ae_traject.png",
       grid.arrange(p_top, p_bottom, ncol=1),
       width = 6,
       height = 5,
       units = "in"
)

ggplot(dt_any_AE, aes(x=time_to_fu, y=Risk))+
  geom_line(aes(group = Idx_donation, color = ex_group, 
                alpha= ex_group, size = ex_group))+
  #facet_wrap(vars(outcome), nrow=1)+
  scale_y_continuous(name="Prob. adverse event", labels=percent_format())+
  scale_x_continuous(name="Days until next donation attempt")+
  scale_color_manual(values=c("grey", "red2", "springgreen4", "darkorange3"), name="")+
  scale_alpha_manual(values=c(0.4, 1, 1, 1), name="")+
  scale_size_manual(values=c(0.2, 1, 1, 1), name="")+
  theme(legend.position = "bottom")


ggsave("4_output/figs/any_ae_traject.png",
       width = 5,
       height = 2.3,
       units = "in"
)




##
# PLOT INDIVIDUAL TRAJECTORIES -----------
##

set.seed(10)
random_donor_nums <- ceiling(runif(60, min=0, max = nrow(dt.frXB)))

for(plot_num in 1:60){
  donor_num <- random_donor_nums[plot_num]
  assign(paste0("plot_traj_", plot_num), 
         ggplot(melt(cbind(data.table(risk_matrix_XB[donor_num, , ]), t = min_IDI:max_IDI), id.vars = "t", measure.vars = c("None", "HGB_defer", "Low", "Absent"))
                , aes(x=t, y=value, fill=variable)) + 
           geom_area()+ 
           scale_y_continuous(expand=c(0,0))+
           scale_x_continuous(breaks=c(56, 150, 244), expand=c(0,0))+
           theme(legend.position="none",
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.line.y = element_blank(),
                 axis.title = element_blank(),
                 axis.text.x = element_text(size=10))+
           scale_fill_manual(values = c("#00FFFF", "#FBD808", "#FF9005", "#FF0000"))
         #xlab("Days until donation attempt")+
         #ylab("Probability of outcome")
         
  )
}



ggsave("./4_output/figs/trajectories_60_random.png",
       plot = plot_grid(plot_traj_1, plot_traj_2, plot_traj_3, plot_traj_4, plot_traj_5,
                        plot_traj_6, plot_traj_7, plot_traj_8, plot_traj_9, plot_traj_10, 
                        plot_traj_11, plot_traj_12, plot_traj_13, plot_traj_14, plot_traj_15,
                        plot_traj_16, plot_traj_17, plot_traj_18, plot_traj_19, plot_traj_20,
                        plot_traj_21, plot_traj_22, plot_traj_23, plot_traj_24, plot_traj_25,
                        plot_traj_26, plot_traj_27, plot_traj_28, plot_traj_29, plot_traj_30, 
                        plot_traj_31, plot_traj_32, plot_traj_33, plot_traj_34, plot_traj_35,
                        plot_traj_36, plot_traj_37, plot_traj_38, plot_traj_39, plot_traj_40,
                        plot_traj_41, plot_traj_42, plot_traj_43, plot_traj_44, plot_traj_45,
                        plot_traj_46, plot_traj_47, plot_traj_48, plot_traj_49, plot_traj_40, 
                        plot_traj_51, plot_traj_52, plot_traj_53, plot_traj_54, plot_traj_55,
                        plot_traj_56, plot_traj_57, plot_traj_58, plot_traj_59, plot_traj_60,
                        ncol = 5),
       width = 6.5,
       height = 8.9,
       units = "in"
)

ggsave("./4_output/figs/trajectories_3.png",
       plot = plot_grid(plot_traj_4, plot_traj_29, plot_traj_6,
                        ncol = 3),
       width = 5,
       height = 1,
       units = "in"
)


#Trajectories for 3 archetypes
archetype_donor_nums <- c(Idx_chronic[1:10],
                          Idx_quick_recoverers[1:10],
                          Idx_slow_recoverers[1:10])


for(donor_num in archetype_donor_nums){
  assign(paste0("plot_archetype_", donor_num), 
         ggplot(melt(cbind(data.table(risk_matrix_XB[donor_num, , ]), t = min_IDI:max_IDI), id.vars = "t", measure.vars = c("None", "HGB_defer", "Low", "Absent"))
                , aes(x=t, y=value, fill=variable)) + 
           geom_area()+ 
           scale_y_continuous(expand=c(0,0))+
           scale_x_continuous(breaks=c(56, 150, 244), expand=c(0,0))+
           theme(legend.position="none",
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.line.y = element_blank(),
                 axis.title = element_blank(),
                 axis.text.x = element_text(size=10))+
           scale_fill_manual(values = c("#00FFFF", "#FBD808", "#FF9005", "#FF0000"),
                             labels = c("No adverse outcome",
                                        "Hemoglobin deferral",
                                        "Low iron donation",
                                        "Absent iron donation"
                             ))
         #xlab("Days until donation attempt")+
         #ylab("Probability of outcome")
         
  )
}

title1 <- ggdraw() + 
  draw_label(
    "  Fast recoverers",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) 
plot_row_fr <- plot_grid(plot_archetype_247, plot_archetype_849,plot_archetype_1234,plot_archetype_1654,
                         ncol = 4)
title2 <- ggdraw() + 
  draw_label(
    "  Slow recoverers",
    fontface = 'bold',
    x = 0,
    hjust = 0
  )
plot_row_sr <- plot_grid(plot_archetype_121, plot_archetype_394,plot_archetype_787,plot_archetype_1781,
                         ncol = 4)
title3 <- ggdraw() + 
  draw_label(
    "  Chronic high risk",
    fontface = 'bold',
    x = 0,
    hjust = 0
  )
plot_row_chr <- plot_grid(plot_archetype_63, plot_archetype_989,plot_archetype_1431,plot_archetype_679,
                          ncol = 4)


#Get legend
legend <- get_legend(
  plot_archetype_121+
  theme(legend.position = "bottom")+
    guides(fill=guide_legend(title="Donation outcome",
                             nrow =2))
)
  
ggsave(
  "./4_output/figs/indiv_plots_archetypes.png",
  plot_grid(title1,
            plot_row_fr,
            title2,
            plot_row_sr,
            title3,
            plot_row_chr,
            legend,
            ncol =1,
            rel_heights = c(rep(c(0.22, 1),3),.5)),
  width = 5, height = 4.5, units="in"
)





### AVERAGE TRAJECTORY BY SUBGROUPS  -----
stratified_outcomes_plt <- function(group_vec, #vector of groups
                                dt_long_risk_matrix,
                                strat_name = "Group"){
  
  dt_temp<- data.table("group"= group_vec,
                       "Idx_donation" = 1:length(group_vec))
  dt_long_grouped<-dt_long_risk_matrix[dt_temp, on = "Idx_donation"]
  dt_long_grouped<-dt_long_grouped[ , list("Mean"= mean(Risk),
                                           "LB" = mean(Risk) - qt(.95,.N-1)*sd(Risk)/sqrt(.N),
                                           "UB" = mean(Risk) + qt(.95,.N-1)*sd(Risk)/sqrt(.N)),
                                    by=.(group, time_to_fu, outcome)]
  dt_long_grouped[,group:=factor(group)]
  dt_any_AE <- cbind(dt_long_grouped[outcome=="None", ])
  dt_any_AE[, c("Mean","LB","UB") := list(1-Mean, 1-UB, 1-LB)]
  dt_any_AE[, outcome := "Any adverse event"]
  
  p_top <- ggplot(dt_any_AE, aes(x=time_to_fu, y=Mean, color = group,
                                 linetype=group))+
    geom_ribbon(aes(ymin=LB, ymax=UB, fill=group), size=0, alpha = 0.2)+
    geom_line()+
    facet_wrap(vars(outcome), nrow=1)+
    scale_y_continuous(name=element_blank(), labels=percent_format(),limits=c(0,1))+
    scale_x_continuous(limits = c(56, 250), breaks = c(56, 150, 250), name=element_blank())+
    theme(legend.position = "None")
  
  p_bottom <- ggplot(dt_long_grouped[outcome!="None"], 
                     aes(x=time_to_fu, y=Mean, color=group, 
                         linetype = group, fill=group))+
    facet_wrap(vars(outcome), nrow=1, labeller = as_labeller(outcome_labs))+
    geom_ribbon(aes(ymin=LB, ymax=UB), size=0, alpha = 0.2)+
    geom_line()+
    scale_y_continuous(name=element_blank(), labels=percent_format(), limits=c(0,NA))+
    scale_x_continuous(limits = c(56, 250), breaks = c(56, 150, 250), name="Days until next donation attempt")+
    theme(legend.position = "bottom")+
    guides(fill=guide_legend(title=strat_name),
           color=guide_legend(title=strat_name),
           linetype=guide_legend(title=strat_name))
  
  yleft <- textGrob("Probability of adverse event\n(mean, 95% confidence interval)", 
                    rot = 90, gp = gpar(fontsize = 11), just = 0.4)
  
  ggsave(paste0("4_output/figs/ae_traject_by_",gsub("\n","_",gsub(" ", "_",strat_name)),".png"),
         plot = grid.arrange(p_top, p_bottom, ncol=1, left = yleft),
         width = 5,
         height = 5,
         units = "in"
  )
}



#BY GENDER
stratified_outcomes_plt(
  dt.frXB[ , ifelse(Gender_F==1, "Female", "Male")],
  dt_long_risk_matrix,
  strat_name = "Gender"
)

#BY baseline iron stores
thresh_absent_f = 12
thresh_absent_m = 12
thresh_low_f = 20
thresh_low_m = 30

dt.frXB[, bl_iron := fifelse((Gender_F==1 & ARUP_Ferritin < thresh_absent_f) |
                                             (Gender_F==0 & ARUP_Ferritin < thresh_absent_m),
                                           "Absent iron", #3=absent iron donation
                                           fifelse((Gender_F==1 & ARUP_Ferritin < thresh_low_f) |
                                                     (Gender_F==0 & ARUP_Ferritin < thresh_low_m),
                                                   "Low iron",#2=low iron donation
                                                   "Iron replete"#1=no adverse event
                                           ))]


stratified_outcomes_plt(
  dt.frXB$bl_iron,
  dt_long_risk_matrix,
  strat_name = "Index donation\niron status"
)


#By iron supplementation
iron_supp <- dt.firstreturn[, fifelse(supp_iron_pct_of_daily==0, "None",
                                      fifelse(supp_iron_pct_of_daily==1,"Daily",
                                              "Less than daily")) ]


stratified_outcomes_plt(
  iron_supp,
  dt_long_risk_matrix,
  strat_name = "Iron supplementation"
)


#Composite iron
dt.firstreturn[ , composite_iron_tertile := cut(compositeIronScore,
                        breaks = quantile(compositeIronScore, probs = c(0:2/3,1)),
                        labels = c("lowest", "middle", "highest"), 
                        include.lowest = TRUE)]


stratified_outcomes_plt(
  dt.firstreturn$composite_iron_tertile,
  dt_long_risk_matrix,
  strat_name = "Composite iron tertile"
)


#Veneous HGB
quants<-round(dt.firstreturn[,quantile(DER_AdjVenousHgb, probs = 0:3/3)],1)

dt.firstreturn[ , veneous_hgb_tertile := cut(DER_AdjVenousHgb,
                                                breaks = dt.firstreturn[,quantile(DER_AdjVenousHgb, probs = 0:3/3)],
                                             labels = c(paste0("lowest\n[",quants[1],"-",quants[2],")"),
                                                        paste0("middle\n[",quants[2],"-",quants[3],")"),
                                                        paste0("highest\n[",quants[3],"-",quants[4],"]")),  
                                             include.lowest = TRUE)]

stratified_outcomes_plt(
  dt.firstreturn$veneous_hgb_tertile,
  dt_long_risk_matrix,
  strat_name = "Venous HGB tertile"
)

#RBC lost last 24 months
quants<-round(dt.firstreturn[,quantile(DER_RBC_Last24months, probs = 0:3/3)],1)
dt.firstreturn[ , RBC_loss_24m_tertile := cut(DER_RBC_Last24months,
                                             breaks = quantile(DER_RBC_Last24months, probs = 0:3/3),
                                             labels = c(paste0("lowest\n[",quants[1],"-",quants[2],")"),
                                                        paste0("middle\n[",quants[2],"-",quants[3],")"),
                                                        paste0("highest\n[",quants[3],"-",quants[4],"]")),
                                             include.lowest = TRUE)]

stratified_outcomes_plt(
  dt.firstreturn$RBC_loss_24m_tertile,
  dt_long_risk_matrix,
  strat_name = "RBC units lost prior\n24 months tertile"
)








# dt.frXB[, sex:=ifelse(Gender_F==1, "Female", "Male")]
# plt_56v250_sex <- ggplot(data=dt.frXB,
#                aes(x=Any_AE_day_56, y=Any_AE_day_250,
#                    color=sex))+
#   geom_point(alpha=0.5, size = 1)+
#   #geom_rug()+
#   geom_abline(slope=1, intercept=0, color="red")+
#   scale_x_continuous(breaks = seq(0,1,.2), limits = c(0,1), labels = label_percent(accuracy=2))+
#   scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1), labels = label_percent(accuracy=2))+
#   xlab("Prob. any adverse event\n56 days after index donation")+
#   ylab("Prob. any adverse event\n250 days after index donation")+
#   labs(color="")+
#   #scale_fill_discrete()
#   theme(legend.position = c(0.05, .95),
#         legend.justification = c(0.05,.95),
#         legend.box.background = element_rect(color="black"),
#         legend.title = element_blank())
# 
# plt_56v250_sex <- ggMarginal(plt_56v250_sex, type="boxplot", size = 8, groupFill = TRUE)
# ggsave("output/figs/sex_56_v_250.png",
#        plt_56v250_sex,
#        width = 4,
#        height = 3,
#        units = "in"
# )
# 
# 
# 
# 
# #By ferritin at index visit
# dt.firstreturn[, ferritin_capped:=pmin(ARUP_Ferritin, 60)]
# plt_56v250_ferr <- ggplot(data=dt.firstreturn,
#                          aes(x=Any_AE_day_56, y=Any_AE_day_250,
#                              color=ferritin_capped,
#                              fill = ))+
#   geom_point(alpha=0.5, size = 1)+
#   #geom_rug()+
#   geom_abline(slope=1, intercept=0, color="red")+
#   scale_x_continuous(breaks = seq(0,1,.2), limits = c(0,1), labels = label_percent(accuracy=2))+
#   scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1), labels = label_percent(accuracy=2))+
#   xlab("Prob. any adverse event\n56 days after index donation")+
#   ylab("Prob. any adverse event\n250 days after index donation")+
#   labs(color="")+
#   scale_colour_stepsn(colors=c("red1", "darkorange","darkgreen"),
#                       guide = guide_colorbar(title="Ferritin ng/ml",
#                                              title.position = "top",
#                                              direction = "horizontal"))+
#   theme(legend.position = c(0.05, .95),
#         legend.justification = c(0.05,.95),
#         legend.box.background = element_rect(color="black"))
# 
# 
# ggsave("output/figs/ferr_56_v_250.png",
#        plt_56v250_ferr,
#        width = 4,
#        height = 3,
#        units = "in"
# )
# 
# 
# #By composit iron score
# 
# 
# 
# #By donations in last 2 years
# 
# 
# #By iron supplementation
# 
# plt_56v250_supp <- ggplot(data=dt.firstreturn,
#                           aes(x=Any_AE_day_56, y=Any_AE_day_250,
#                               color=ferritin_capped,
#                               fill = ))+
#   geom_point(alpha=0.5, size = 1)+
#   #geom_rug()+
#   geom_abline(slope=1, intercept=0, color="red")+
#   scale_x_continuous(breaks = seq(0,1,.2), limits = c(0,1), labels = label_percent(accuracy=2))+
#   scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1), labels = label_percent(accuracy=2))+
#   xlab("Prob. any adverse event\n56 days after index donation")+
#   ylab("Prob. any adverse event\n250 days after index donation")+
#   labs(color="")+
#   scale_colour_stepsn(colors=c("red1", "darkorange","darkgreen"),
#                       guide = guide_colorbar(title="Ferritin ng/ml",
#                                              title.position = "top",
#                                              direction = "horizontal"))+
#   theme(legend.position = c(0.05, .95),
#         legend.justification = c(0.05,.95),
#         legend.box.background = element_rect(color="black"))
# 
# 
# ggsave("output/figs/ferr_56_v_250.png",
#        plt_56v250_ferr,
#        width = 4,
#        height = 3,
#        units = "in"
# )
# 
# 
# 
# 
# ##
# # IRON SUPPLEMENTATION -----------
# ##
# 
# dt.features <- fread("./data/first_return_one_hot.csv")
# 
# dt.features_iron_supp <- cbind(dt_features)
# dt.features_iron_supp[ , pct_daily_iron_supp := 1]
# 
# dt.firstreturn_iron_supp <- cbind(dt.firstreturn)
# dt.firstreturn_iron_supp[ , pct_daily_iron_supp := 1]
# 
# dt.features_no_iron_supp <- cbind(dt_features)
# dt.features_no_iron_supp[ , pct_daily_iron_supp := 1]
# 
# dt.firstreturn_no_iron_supp <- cbind(dt.firstreturn)
# dt.firstreturn_no_iron_supp[ , pct_daily_iron_supp := 1]
# 
# 
# dt.firstreturn[, colSums(.SD), .SDcols=paste0("Q",0:3)]
# 
# 
# dt.firstreturn_no_iron_supp <- cbind(dt.firstreturn_no_iron_supp, 
#                         risk_scores(dt.features_no_iron_supp))
# 
# dt.firstreturn_iron_supp <- cbind(dt.firstreturn_iron_supp, 
#                         risk_scores(dt.features_iron_supp))
# 
# 
# #Risk trajectory matrix
# n_donations = nrow(dt.features_iron_supp)
# max_IDI = 250; min_IDI = 56 
# 
# risk_matrix_iron_supp <- array(data=NA,
#                      dim = c(nrow(dt.features_iron_supp), max_IDI - min_IDI + 1, 4),
#                      dimnames = list(Idx_donation = 1:nrow(dt.features_iron_supp),
#                                      time_to_fu = min_IDI:max_IDI,
#                                      outcome = c("None", "HGB_defer", "Low", "Absent"))
# )
# 
# 
# # fill in risk matrix
# dt.features.temp <- rbind(dt.features_iron_supp)
# for (t_return in min_IDI:max_IDI){
#   dt.features.temp[ , time_to_fu := t_return]
#   preds <- risk_scores(dt.features.temp)
#   
#   risk_matrix_iron_supp[, as.character(t_return), ] <- data.matrix(preds)
# }
# 
# #risk_matrix[ , c("56", "250"), "None"]
# #risk_matrix[100 , , ]
# 
# 
# saveRDS(risk_matrix_iron_supp, "./data/3d_risk_matrix_iron_supp.RDS")
