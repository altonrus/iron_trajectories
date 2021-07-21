library(data.table)
library(ggplot2)
library(dplyr)

library(jsonlite)
library("sas7bdat")
# LOAD DATA -----------------------------


# RISE dataset can only be accessed with permission from biolincc
#   it is free but need to submit request with IRB exemption or approval

d.bl <- read.sas7bdat("./1_data/RISEdata/rise_baseline.sas7bdat")
d.fu <- read.sas7bdat("./1_data/RISEdata/rise_followup.sas7bdat")
d.bl <- as.data.table(d.bl)
d.fu <- as.data.table(d.fu)

#Iron composite score provided by Bryan Spencer
comp.iron.scores <- fread(file ="./1_data/composite_iron_scores.csv")
d.bl <- merge(d.bl, comp.iron.scores[, .(RandID, compositeIronScore)], by = "RandID", all.x = TRUE, all.y = FALSE)


# colnames(don2006)==colnames(don2008)
# don2009$yr<- 2009
# don2008$yr<- 2008
# don2007$yr<- 2007
# don2006$yr<- 2006
# d <- rbind(don2009, don2008, don2007, don2006)
# saveRDS(d, "d")
# d <- readRDS("donations")
# d <- data.table(d)

# PROCESS AND CLEAN -----------------------------

# # DIFFERENCES BETWEEN BASELINE [BL] AND FOLLOWUP [FU] IN COLUMN NAMES
# setdiff(colnames(d.bl), colnames(d.fu))
# setdiff(colnames(d.fu), colnames(d.bl))

#Count bl donations with no fu
d.bl[ RandID %in% setdiff(d.bl$RandID, d.fu$RandID), .N]
#Remove baseline donations with no followup visits
d.bl <- d.bl[ ! RandID %in% setdiff(d.bl$RandID, d.fu$RandID)]

# #CREATE SUMMARIES
# library(summarytools)
# view(dfSummary(d.bl), file="data_summary_baseline.html")
# view(dfSummary(d.fu), file="data_summary_followup.html")

# LOOK FOR ERRONEOUS VALUES
# RQ19_ever_pregnant = 8
#For donor 378-5276-3 visit 1, 
# RQ19_ever_pregnant = 8 and Number of pregnancies is NA.
# Changing to never pregnant, number of pregnancies 0
# Since she has no subsequent ferritin or Hg, removing
# from dataset
d.bl[RandID=="378-5276-3", c(RQ19_Ever_Pregnant, RQ20_NumberOfPregnancies)]
d.fu[RandID=="378-5276-3", c("DER_Fingerstick_HCT", "DER_Fingerstick_HGB", "ARUP_Ferritin", "STS_VEN_HemocueHgb", "DER_AdjVenousHgb")]
d.bl<-d.bl[!(RandID=="378-5276-3")]
d.fu<-d.fu[!(RandID=="378-5276-3")]

#Assign donor with missing blood type to most common
d.bl[DD_ABO_RH=="UNT", .N] #Counting; 1 donor at baseline
d.fu[DD_ABO_RH=="UNT", .N] #They gave 2 subsequent donations
d.bl[DD_ABO_RH=="UNT",DD_ABO_RH := "O+"]
d.fu[DD_ABO_RH=="UNT",DD_ABO_RH := "O+"]


#Make days since DR loss 10 years for patients with no record
d.bl[, DER_DaysDRLoss := ifelse(is.na(DER_DaysDRLoss), 10*365, DER_DaysDRLoss)]
d.fu[, DER_DaysDRLoss := ifelse(is.na(DER_DaysDRLoss), 10*365, DER_DaysDRLoss)]
d.bl[, DER_DaysRBCLoss := ifelse(is.na(DER_DaysRBCLoss), 10*365, DER_DaysRBCLoss)]
d.fu[, DER_DaysRBCLoss := ifelse(is.na(DER_DaysRBCLoss), 10*365, DER_DaysRBCLoss)]



# #BL missing both fingerstick
# table(is.na(d.bl$DER_Fingerstick_HGB),
#       is.na(d.bl$DER_Fingerstick_HCT)) 
# #67 are missing both fingerstick HGB and HCT at baseline
# 
# #BL missing both fingerstick and venous
# table(is.na(d.bl$DER_Fingerstick_HGB) &
#         is.na(d.bl$DER_Fingerstick_HCT),
#       is.na(d.bl$STS_VEN_HemocueHgb)) 
# #none are missing venous
# 
# #FU missing both fingerstick
# table(is.na(d.fu$DER_Fingerstick_HGB),
#       is.na(d.fu$DER_Fingerstick_HCT)) 
# #1326(!) are missing both at followup
# 
# #FU missing both fingerstick by visit result
# table(is.na(d.fu$DER_Fingerstick_HGB) & is.na(d.fu$DER_Fingerstick_HCT),
#       d.fu$DER_VisitResult)
# 
# #FU missing venous and both fingerstick
# table(is.na(d.fu$DER_Fingerstick_HGB) & is.na(d.fu$DER_Fingerstick_HCT),
#       is.na(d.fu$STS_VEN_HemocueHgb))
# #1028 are missing both fingersticks and venous
# 
# #FU missing all venous and both fingerstick
# table(is.na(d.fu$DER_Fingerstick_HGB) & is.na(d.fu$DER_Fingerstick_HCT)
#       & is.na(d.fu$STS_VEN_HemocueHgb),
#       d.fu$DER_VisitResult)
# table(d.fu$DER_Fingerstick_HGB < 12.5,
#       d.fu$DER_VisitResult) #Most coded as deferrals are actual deferrals (VisitResult=3)
# 
# table(d.fu$DER_RBCLoss_Units,
#       d.fu$DER_VisitResult)
# 
# table(d.fu$DV_Outcome,
#       d.fu$DER_VisitResult)
# 
# table(d.fu$DV_Deferral_Cat1,
#       d.fu$DER_VisitResult) 
# table(d.fu$DV_Deferral_Cat2,
#       d.fu$DER_VisitResult)
# table(d.fu[DER_VisitResult==3]$DV_Deferral_Cat2,
#       d.fu[DER_VisitResult==3]$DV_Deferral_Cat1)
# table(d.bl$DER_VisitResult)

#CREATE HB deferral feild
# Hemoglobin deferral if:
#   DV_Outcome == 3 
# or
#   DV_Outcome != 1 AND
#   
#CREATE fingerstick_HGBeq
# If fingerstick HGB measured use it.
#      Otherwise use fingerstick_HCT/3.04
#      Otherwise use DER_AdjVenousHgb (venous HGB, if taken post-donation then adjusted)

d.bl[, FingerstickHGB_equiv := fifelse(is.na(DER_Fingerstick_HGB), 
                                       fifelse(is.na(DER_Fingerstick_HCT), 
                                               DER_AdjVenousHgb,
                                               DER_Fingerstick_HCT/3.04),
                                       DER_Fingerstick_HGB)]
d.fu[, FingerstickHGB_equiv := fifelse(is.na(DER_Fingerstick_HGB), 
                                       fifelse(is.na(DER_Fingerstick_HCT), 
                                               DER_AdjVenousHgb,
                                               DER_Fingerstick_HCT/3.04),
                                       DER_Fingerstick_HGB)]

table(is.na(d.bl$FingerstickHGB_equiv), useNA="always") #No bl visits missing HGB equiv

table(is.na(d.fu$FingerstickHGB_equiv), d.fu$DER_VisitResult, useNA="always")
#713 completed donations; 28 QNS, and 151 deferrals are missing any HGB

#For donations missing venous HGB, replace with fingerstick.
d.bl[, DER_AdjVenousHgb := fifelse(is.na(DER_AdjVenousHgb), FingerstickHGB_equiv, DER_AdjVenousHgb)]
d.fu[, DER_AdjVenousHgb := fifelse(is.na(DER_AdjVenousHgb), FingerstickHGB_equiv, DER_AdjVenousHgb)]


# For missing country, assume 1 (from US)
d.bl[, DD_Country := ifelse(is.na(DD_Country), 1, DD_Country)]
d.fu[, DD_Country := ifelse(is.na(DD_Country), 1, DD_Country)]

# For missing race, assume other
d.bl$DD_Raceth[which(is.na(d.bl$DD_Raceth))] = "O"
d.fu$DD_Raceth[which(is.na(d.fu$DD_Raceth))] = "O"
d.bl[,DD_Raceth:= ifelse(DD_Raceth=="", "O", DD_Raceth)]
d.fu[,DD_Raceth:= ifelse(DD_Raceth=="", "O", DD_Raceth)]

#Missing height, weight, and BMI
# table(is.na(d.bl$DER_Height), is.na(d.bl$DER_Weight), is.na(d.bl$BMI))
# table(is.na(d.fu$DER_Height), is.na(d.fu$DER_Weight), is.na(d.fu$BMI))

unique(d.fu[is.na(DER_Weight)]$RandID)
unique(d.bl[is.na(DER_Weight)]$RandID)
d.fu[is.na(DER_Weight), .N]
#Impute mean weight by gender for those age >25
weight_by_gender <- d.bl[DER_Age > 25, mean(DER_Weight, na.rm=TRUE), by=DD_Gender]
d.bl[, DER_Weight := fifelse(!is.na(DER_Weight), DER_Weight, 
                            fifelse(DD_Gender=="M", 
                                   weight_by_gender[DD_Gender == "M", V1],
                                   weight_by_gender[DD_Gender == "F", V1]))]
d.fu[, DER_Weight := fifelse(!is.na(DER_Weight), DER_Weight, 
                            fifelse(DD_Gender=="M", 
                                   weight_by_gender[DD_Gender == "M", V1],
                                   weight_by_gender[DD_Gender == "F", V1]))]
#Impute mean height by gender for those age <25
d.fu[is.na(DER_Height), .N]
height_by_gender <- d.bl[DER_Age > 25, mean(DER_Height, na.rm=TRUE), by=DD_Gender]
d.bl[, DER_Height := fifelse(!is.na(DER_Height), DER_Height, 
                            fifelse(DD_Gender=="M", 
                                   height_by_gender[DD_Gender == "M", V1],
                                   height_by_gender[DD_Gender == "F", V1]))]
d.fu[, DER_Height := fifelse(!is.na(DER_Height), DER_Height, 
                            fifelse(DD_Gender=="M", 
                                   height_by_gender[DD_Gender == "M", V1],
                                   height_by_gender[DD_Gender == "F", V1]))]
#Calc BMI if missing
d.bl[, BMI := ifelse(!is.na(BMI), BMI,
                     DER_Weight*0.453592 / (DER_Height*0.0254)^2)] #.45 kg in lb; .025 in in m

d.fu[, BMI := ifelse(!is.na(BMI), BMI,
                     DER_Weight*0.453592 / (DER_Height*0.0254)^2)] #.45 kg in lb; .025 in in m

#Recode "RQ1_Ever_Donated" to 0 (no) and 1 (yes). Add d.fu and make 1 for everyone
#  since they all gave an index donation
d.bl[, RQ1_Ever_Donated := ifelse(RQ1_Ever_Donated==1, 0, 1)]
d.fu[, RQ1_Ever_Donated := 1]



# #Missing genotype measurements: set to 0 for C282Y, G277S, and H63D
# d.bl$C282Y_Genotype[which(is.na(d.bl$C282Y_Genotype))] = 0
# d.fu$C282Y_Genotype[which(is.na(d.bl$C282Y_Genotype))] = 0
# d.bl$G277S_Genotype[which(is.na(d.bl$G277S_Genotype))] = 0
# d.fu$G277S_Genotype[which(is.na(d.bl$G277S_Genotype))] = 0
# d.bl$H63D_Genotype[which(is.na(d.bl$H63D_Genotype))] = 0
# d.fu$H63D_Genotype[which(is.na(d.bl$H63D_Genotype))] = 0
#HFE hap pr??



#Missing total lifetime donations: set to 0
d.bl[, RQ2_Total_Lifetime_Donations := fifelse(is.na(RQ2_Total_Lifetime_Donations), 0, RQ2_Total_Lifetime_Donations)]

#Ever smoke
d.bl[, RQ7_Ever_Smoked := fifelse(RQ7_Ever_Smoked==1, 1, 0, na=0)] #if blank or 8 ("Don't know"), code as no
d.bl[, RQ8_Smoked_Past_90Days := fifelse(RQ8_Smoked_Past_90Days==1, 1, 0, na=0)] #if blank or 8 ("Don't know"), code as no


# Convert the food fields to "Agerage meals/week"
# 1 = never = 0   
# 2 = less than 1/week = 0.5
# 3 = 1x/week = 1
# 4 = 2x/week = 2
# 5 = 3-4x/week = 3.5
# 6 = 5-6x/week = 5.5
# 7 = every day = 7
# 8 = 2 or more times a day = 14

# LIVER: make 99 -> 1 (no liver)
table(d.bl$RQ11_Liver, useNA="always")
sum(is.na(d.bl$RQ11_Liver))

d.bl[ , RQ11_Liver := ifelse(RQ11_Liver == 1, 0,
                             ifelse(RQ11_Liver == 2, 0.5,
                                    ifelse(RQ11_Liver == 3, 1,
                                           ifelse(RQ11_Liver == 4, 2,
                                                  ifelse(RQ11_Liver == 5, 3.5,
                                                         ifelse(RQ11_Liver == 6, 5.5,
                                                                ifelse(RQ11_Liver == 7, 7,
                                                                       ifelse(RQ11_Liver == 8, 14,
                                                                              ifelse(RQ11_Liver == 99, 1, NA)))))))))]

# BEEF: make 99 -> 2x/week
table(d.bl$RQ11_Beef, useNA="always")
sum(is.na(d.bl$RQ11_Beef))

d.bl[ , RQ11_Beef := ifelse(RQ11_Beef == 1, 0,
                            ifelse(RQ11_Beef == 2, 0.5,
                                   ifelse(RQ11_Beef == 3, 1,
                                          ifelse(RQ11_Beef == 4, 2,
                                                 ifelse(RQ11_Beef == 5, 3.5,
                                                        ifelse(RQ11_Beef == 6, 5.5,
                                                               ifelse(RQ11_Beef == 7, 7,
                                                                      ifelse(RQ11_Beef == 8, 14,
                                                                             ifelse(RQ11_Beef == 99, 2, NA)))))))))]


# Lamb pork chicken turkey: make 99 -> 3.5x/week
table(d.bl$RQ11_LPCT, useNA="always")
median(d.bl$RQ11_LPCT)
sum(is.na(d.bl$RQ11_LPCT))

d.bl[ , RQ11_LPCT := ifelse(RQ11_LPCT == 1, 0,
                            ifelse(RQ11_LPCT == 2, 0.5,
                                   ifelse(RQ11_LPCT == 3, 1,
                                          ifelse(RQ11_LPCT == 4, 2,
                                                 ifelse(RQ11_LPCT == 5, 3.5,
                                                        ifelse(RQ11_LPCT == 6, 5.5,
                                                               ifelse(RQ11_LPCT == 7, 7,
                                                                      ifelse(RQ11_LPCT == 8, 14,
                                                                             ifelse(RQ11_LPCT == 99, 3.5, NA)))))))))]

# Clam: make 99 -> never
table(d.bl$RQ11_Clams, useNA="always")
sum(is.na(d.bl$RQ11_Clams))

d.bl[ , RQ11_Clams := ifelse(RQ11_Clams == 1, 0,
                             ifelse(RQ11_Clams == 2, 0.5,
                                    ifelse(RQ11_Clams == 3, 1,
                                           ifelse(RQ11_Clams == 4, 2,
                                                  ifelse(RQ11_Clams == 5, 3.5,
                                                         ifelse(RQ11_Clams == 6, 5.5,
                                                                ifelse(RQ11_Clams == 7, 7,
                                                                       ifelse(RQ11_Clams == 8, 14,
                                                                              ifelse(RQ11_Clams == 99, 0, NA)))))))))]

# Oyesters, mussels, shrimp, sardines: make 99 -> <1/week
table(d.bl$RQ11_OMSS, useNA="always")
sum(is.na(d.bl$RQ11_OMSS))

d.bl[ , RQ11_OMSS := ifelse(RQ11_OMSS == 1, 0,
                            ifelse(RQ11_OMSS == 2, 0.5,
                                   ifelse(RQ11_OMSS == 3, 1,
                                          ifelse(RQ11_OMSS == 4, 2,
                                                 ifelse(RQ11_OMSS == 5, 3.5,
                                                        ifelse(RQ11_OMSS == 6, 5.5,
                                                               ifelse(RQ11_OMSS == 7, 7,
                                                                      ifelse(RQ11_OMSS == 8, 14,
                                                                             ifelse(RQ11_OMSS == 99, 0.5, NA)))))))))]
# Other fish: make 99 -> <1/week
table(d.bl$RQ11_OtrFish, useNA="always")
sum(is.na(d.bl$RQ11_OtrFish))

d.bl[ , RQ11_OtrFish := ifelse(RQ11_OtrFish == 1, 0,
                               ifelse(RQ11_OtrFish == 2, 0.5,
                                      ifelse(RQ11_OtrFish == 3, 1,
                                             ifelse(RQ11_OtrFish == 4, 2,
                                                    ifelse(RQ11_OtrFish == 5, 3.5,
                                                           ifelse(RQ11_OtrFish == 6, 5.5,
                                                                  ifelse(RQ11_OtrFish == 7, 7,
                                                                         ifelse(RQ11_OtrFish == 8, 14,
                                                                                ifelse(RQ11_OtrFish == 99, 0.5, NA)))))))))]
# Eggs: make 99 -> 1/week
table(d.bl$RQ11_Eggs, useNA="always")
sum(is.na(d.bl$RQ11_Eggs))

d.bl[ , RQ11_Eggs := ifelse(RQ11_Eggs == 1, 0,
                            ifelse(RQ11_Eggs == 2, 0.5,
                                   ifelse(RQ11_Eggs == 3, 1,
                                          ifelse(RQ11_Eggs == 4, 2,
                                                 ifelse(RQ11_Eggs == 5, 3.5,
                                                        ifelse(RQ11_Eggs == 6, 5.5,
                                                               ifelse(RQ11_Eggs == 7, 7,
                                                                      ifelse(RQ11_Eggs == 8, 14,
                                                                             ifelse(RQ11_Eggs == 99, 1, NA)))))))))]
# Dairy: make 99 -> 1/day
table(d.bl$RQ11_Dairy, useNA="always")
sum(is.na(d.bl$RQ11_Dairy))

d.bl[ , RQ11_Dairy := ifelse(RQ11_Dairy == 1, 0,
                             ifelse(RQ11_Dairy == 2, 0.5,
                                    ifelse(RQ11_Dairy == 3, 1,
                                           ifelse(RQ11_Dairy == 4, 2,
                                                  ifelse(RQ11_Dairy == 5, 3.5,
                                                         ifelse(RQ11_Dairy == 6, 5.5,
                                                                ifelse(RQ11_Dairy == 7, 7,
                                                                       ifelse(RQ11_Dairy == 8, 14,
                                                                              ifelse(RQ11_Dairy == 99, 7, NA)))))))))]

#Composit iron score
mean_composite_iron <- d.bl[, mean(compositeIronScore, na.rm=TRUE)]
table(is.na(d.bl$compositeIronScore))
d.bl[, compositeIronScore := ifelse(is.na(compositeIronScore), mean_composite_iron, compositeIronScore)]

#Multivitamin & Iron supplements
table(d.bl$RQ12A_MultiVitamins_YN, useNA="always")
d.bl[, RQ12A_MultiVitamins_YN := fifelse(RQ12A_MultiVitamins_YN==1, 1, 0, na=0)] #Recode 'don't know' and blank to no
table(d.bl$RQ12A_MultiVitamins_YN)

table(d.bl[RQ12A_MultiVitamins_YN == 0, RQ12B_MultiVitamins_How_Often], useNA="always")
table(d.bl[RQ12A_MultiVitamins_YN == 1, RQ12B_MultiVitamins_How_Often], useNA="always")
d.bl[ , multivitamins_per_week := ifelse(RQ12A_MultiVitamins_YN == 0, 0, 
                                                ifelse(RQ12B_MultiVitamins_How_Often==1, 7,
                                                       ifelse(RQ12B_MultiVitamins_How_Often==2, 5,
                                                              ifelse(RQ12B_MultiVitamins_How_Often==3, 2,
                                                                     #If yes to multivitamins but missing howOften, 
                                                                     #set to 4-6x per week
                                                                     5))))]


table(d.bl$multivitamins_per_week, useNA="always")

#If answered "don't know" to whether iron supplement has iron, set to no.
d.bl[ , RQ12C_MV_WithIron_YN := fifelse(RQ12C_MV_WithIron_YN==1, 1, 0, na=0)]
d.bl[ , RQ13A_Iron_Supplements_YN := fifelse(RQ13A_Iron_Supplements_YN == 1, 1, 0, na=0)]

d.bl[ , ironsupp_per_week := fifelse(RQ13A_Iron_Supplements_YN == 0, 0, 
                                         fifelse(RQ13B_Iron_Supplements_HowOften==1, 7,
                                                fifelse(RQ13B_Iron_Supplements_HowOften==2, 5,
                                                       fifelse(RQ13B_Iron_Supplements_HowOften==3, 2,
                                                              #If yes to iron supplements but missing how_often,
                                                              # set to 4-6x per weekmissing, set to daily
                                                              5))),
                                     na=5)]

table(d.bl$ironsupp_per_week, useNA="always")

d.bl[ , supp_iron_pct_of_daily := pmin(1, ((ironsupp_per_week+RQ12C_MV_WithIron_YN*multivitamins_per_week)/7))]
d.bl[ , supp_iron_pct_of_daily := fifelse(is.na(supp_iron_pct_of_daily), 0, supp_iron_pct_of_daily)]
table(d.bl$supp_iron_pct_of_daily, useNA="always")


#Menstration
table(d.bl[DD_Gender == "F"]$RQ15_Menstrual_Status, useNA = "always")
#1 19yo donor has NA. Think she may be pre-menstrual so giving her 0.
table(d.bl[DD_Gender == "F"]$RQ15_Menstrual_Status, 
      d.bl[DD_Gender == "F"]$RQ17_NumberOfPeriods, useNA = "always")
table(d.bl[DD_Gender == "F"]$RQ15_Menstrual_Status, 
      d.bl[DD_Gender == "F"]$RQ18_Menstrual_Flow, useNA = "always")

d.bl[ , RQ17_NumberOfPeriods := fifelse(RQ17_NumberOfPeriods==99, 12, RQ17_NumberOfPeriods, na=0)]
d.bl[ , RQ18_Menstrual_Flow := fifelse(RQ18_Menstrual_Flow==9, 4, RQ18_Menstrual_Flow, na=0)]
d.bl[ , menstrual_flow_times_freq := (RQ17_NumberOfPeriods/12)*(RQ18_Menstrual_Flow/6)]
table(d.bl$menstrual_flow_times_freq, d.bl$DD_Gender, useNA="always")
#Add field for male, female<50, and female>=50
d.bl[ , gender_menstrating_cohorts := ifelse(DD_Gender == "M", "M",
                                             ifelse(RQ17_NumberOfPeriods == 0, "F_menstrating", "F_no_menstr"))]
#Pregnancy
table(d.bl$RQ19_Ever_Pregnant, d.bl$DD_Gender, useNA="always")
d.bl[, RQ19_Ever_Pregnant:= fifelse(RQ19_Ever_Pregnant==1, 1, 0, na=0)]
table(d.bl$RQ20_NumberOfPregnancies, d.bl$DD_Gender, useNA="always")
d.bl[, RQ20_NumberOfPregnancies:= fifelse(RQ20_NumberOfPregnancies>0, RQ20_NumberOfPregnancies, 0, na=0)]
table(d.bl$RQ21_NumberOfLiveBirths, d.bl$DD_Gender, useNA="always")
d.bl[, RQ21_NumberOfLiveBirths:= fifelse(RQ21_NumberOfLiveBirths>0, RQ21_NumberOfLiveBirths, 0, na=0)]



#For fields in baseline but not followup table, merge by randID

#fields in BL table but not FU table
setdiff(colnames(d.bl), colnames(d.fu))
#fields in FU table but not BL table
setdiff(colnames(d.fu), colnames(d.bl))
#Remove all fields from d.fu that aren't in d.bl
d.fu[ , setdiff(colnames(d.fu), colnames(d.bl)) := NULL ]
#Merge in all fields in d.bl that aren't in d.fu
d.fu <- d.bl[, .SD, .SDcols = c("RandID", setdiff(colnames(d.bl), colnames(d.fu))),][d.fu, , on="RandID"]
#Combine into one table
d.all <- rbind(d.bl, d.fu)



# Total lifetime
#sort by RandID then VisitNum
d.all<-d.all[ order(RandID, VisitNum)]
d.all[ , donationMade := ifelse(DER_RBCLoss_Units > 0, 1, 0)]
d.all[ , cumStudyDonations := cumsum(donationMade), by = RandID]
d.all[ , cumLifetimeDonations := RQ2_Total_Lifetime_Donations + cumStudyDonations] #includes current visit

#head(d.all[, c("RandID", "VisitNum", "donationMade", "cumStudyDonations", "DER_RBCLoss_Units", "cumLifetimeDonations", "RQ2_Total_Lifetime_Donations")],20)

#Replace DD_Gender with Gender_F==1 if female, 0 otherwise
d.all[ , Gender_F := fifelse(DD_Gender=="M", 0, 1, na=NA)]



#Define features to use; remove other fields
features_all <- c('DER_RBC_Last12months', 'DER_RBC_Last24months', 'DER_RBCLoss_Units', 'DER_RBCLoss_mL', 
                  'DER_DaysRBCLoss', 'DER_DaysDRLoss', 'RQ1_Ever_Donated', 'cumLifetimeDonations', 
                  'FingerstickHGB_equiv', 'DD_ABO_RH', 'DER_AdjVenousHgb', 'DER_Weight', 'DER_Height', 
                  'BMI', 'DER_EBV', 'DER_RedCellVolume', 'DER_PercentRBCLoss', 'DD_Country', 'DER_Age', 
                  'Gender_F', 'DD_Raceth', 'RQ7_Ever_Smoked', 'RQ8_Smoked_Past_90Days', 'RQ11_Liver', 
                  'RQ11_Beef', 'RQ11_LPCT', 'RQ11_Clams', 'RQ11_OMSS', 'RQ11_OtrFish', 'RQ11_Eggs', 
                  'RQ11_Dairy', 'compositeIronScore', 'supp_iron_pct_of_daily', 'multivitamins_per_week', 
                  'RQ17_NumberOfPeriods', 'RQ18_Menstrual_Flow', 'menstrual_flow_times_freq', 
                  'RQ19_Ever_Pregnant', 'RQ20_NumberOfPregnancies', 'RQ21_NumberOfLiveBirths', 
                  'gender_menstrating_cohorts')
features_fe_as_predictor <- c('ARUP_Ferritin', 'ARUP_STR', 'DER_ARUP_log_Ferr', 
                              'DER_ARUP_log_STfR_Ferr', 'DER_BodyIron')

identifiers <- c("RandID", "VisitDate", "VisitNum", "DER_VisitResult", "DV_Donproc")

#setdiff(features_all, colnames(d.all))


d.all <- d.all[ , .SD, .SDcols = c(features_all, 
                           features_fe_as_predictor,
                           identifiers)]



#CREATE SUMMARY
library(summarytools)
view(dfSummary(d.all), file="./3_intermediate/data_summary_prelabelling.html")



# Build model development dataset ----------------------
#Generate outcome of visit

thresh_absent_f = 12
thresh_absent_m = 12
thresh_low_f = 20
thresh_low_m = 30



d.all[, outcome := fifelse(DER_VisitResult == 3, 
                                  1,#1=HGB deferral
                                  fifelse(is.na(ARUP_Ferritin),
                                          -1,
                                          fifelse((Gender_F==1 & ARUP_Ferritin < thresh_absent_f) |
                                                    (Gender_F==0 & ARUP_Ferritin < thresh_absent_m),
                                                  3, #3=absent iron donation
                                                  fifelse((Gender_F==1 & ARUP_Ferritin < thresh_low_f) |
                                                            (Gender_F==0 & ARUP_Ferritin < thresh_low_m),
                                                          2,#2=low iron donation
                                                          0#1=no adverse event
                                                          ))))]

d.all[, iron_loss_visit := ifelse(DER_RBCLoss_mL > 55, 1, 0)]
d.all[, index_donation := ifelse(DER_RBCLoss_mL > 150 & DER_RBCLoss_Units < 2, 1, 0)]


d.labeled <- cbind(d.all[is.na(RandID)],
                   "time_to_fu" = numeric(),
                   "fu_outcome"= character())


for (row_idx in 1:nrow(d.all)){
  if(d.all[row_idx, index_donation]==1){
    #if qualifies as index visit, get all subsequent visits from donor
    d.index_visit <- d.all[row_idx]
    d.fu_visits <- d.all[RandID==d.index_visit$RandID & VisitNum > d.index_visit$VisitNum]
    reached_iron_loss_visit <- FALSE
    row_fu <- 1
    while (reached_iron_loss_visit == FALSE & row_fu <= nrow(d.fu_visits)){
      d.labeled <- rbind(
        d.labeled,
        cbind(d.index_visit,
              "time_to_fu" = d.fu_visits[row_fu, VisitDate] - d.index_visit$VisitDate,
              "fu_outcome" = d.fu_visits[row_fu, outcome]
              )
      )
      
      reached_iron_loss_visit <- fifelse(d.fu_visits[row_fu]$iron_loss_visit == 1, TRUE, FALSE)
      row_fu = row_fu+1
    }

  }
}

#Distribution of outcome including undetermined
table(d.labeled$fu_outcome)
d.labeled[, .N/nrow(d.labeled), by=fu_outcome]
#Delete undetermined outcomes
d.labeled <- rbind(d.labeled[fu_outcome != -1])
#Distribution of outcomes
d.labeled[, .N/nrow(d.labeled), by=fu_outcome]
#Number of rows per donor
table(d.labeled[, .N, by=RandID]$N)
#Number of rows per unique index donation
table(d.labeled[, .N, by=paste0(RandID, VisitNum)]$N)

#Remove excessive columns 
d.labeled <- d.labeled[, !c("outcome", "index_donation", "iron_loss_visit")]

#CREATE SUMMARY
view(dfSummary(d.labeled), file="3_intermediate/data_summary_labeled.html")


fwrite(d.labeled, "./1_data/ml_training_data.csv")

# Build calibration/simulation dataset ----------------------
d.firstreturn <- cbind(d.all[is.na(RandID)],
                       "time_to_fu" = numeric(),
                       "fu_outcome"= character())

for (row_idx in 1:nrow(d.all)){
  if(d.all[row_idx, index_donation]==1){
    #if qualifies as index visit, get all subsequent visits from donor
    d.index_visit <- d.all[row_idx]
    d.fu_visits <- d.all[RandID==d.index_visit$RandID & VisitNum > d.index_visit$VisitNum]
    fu_visit_added <- FALSE
    row_fu <- 1
    while (fu_visit_added == FALSE & row_fu <= nrow(d.fu_visits)){
      if(
        d.fu_visits[row_fu, DER_VisitResult == 3 | DER_RBCLoss_mL >= 55]
      ){
        d.firstreturn <- rbind(
          d.firstreturn,
          cbind(d.index_visit,
                "time_to_fu" = d.fu_visits[row_fu, VisitDate] - d.index_visit$VisitDate,
                "fu_outcome" = d.fu_visits[row_fu, outcome]
          )
        )
        fu_visit_added <- TRUE
        
      }
      
      row_fu = row_fu+1
    }
    
  }
}

d.firstreturn <- d.firstreturn[, !c("outcome", "index_donation", "iron_loss_visit")]


#CREATE SUMMARY
library(summarytools)
view(dfSummary(d.firstreturn), file="./3_intermediate/data_summary_firstreturn.html")


fwrite(d.firstreturn, "./1_data/first_return_dataset.csv")
