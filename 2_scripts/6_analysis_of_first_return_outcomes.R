dt.fr <- fread("./1_data/first_return_dataset.csv")
dt.fr <- dt.fr[!is.na(FingerstickHGB_equiv)]
dt.fr<-dt.fr[time_to_fu >=56]
dt.frXB <- dt.fr[!is.na(ARUP_Ferritin)]

##Plot actual returns + outcomes
dt.plt <- dt.frXB[, c("time_to_fu", "fu_outcome")]

ggplot(dt.plt)+
  geom_boxplot(aes(x = time_to_fu, y = factor(fu_outcome)))

#Average IDI by outcomes
dt.frXB[, outcome := ifelse(fu_outcome==-1,
                            "Unknown iron status donation",
                            ifelse(fu_outcome==0,
                                   "No adverse event",
                                   ifelse(fu_outcome==1,
                                          "HGB deferral",
                                          ifelse(fu_outcome==2,
                                                 "Low iron donation",
                                                 "Absent iron donation"))))]
dt.frXB[,outcome := factor(outcome,
                           levels = c("HGB deferral",
                                      "Absent iron donation",
                                      "Low iron donation",
                                      "No adverse event",
                                      "Unknown iron status donation"))]

#Calculate expected distribution
sim_outcome_dist <- table(dt.frXB$fu_outcome)
frac_of_completed <- sim_outcome_dist[c("0", "2", "3")]/sum(sim_outcome_dist[c("0", "2", "3")])

target_dist <- c(sim_outcome_dist["0"] + frac_of_completed["0"]*sim_outcome_dist["-1"],
                 sim_outcome_dist["1"],
                 sim_outcome_dist["2"] + frac_of_completed["2"]*sim_outcome_dist["-1"],
                 sim_outcome_dist["3"] + frac_of_completed["3"]*sim_outcome_dist["-1"] )

#Create data.table
dt.target<-data.table(cbind("target_dist" = c(0, target_dist),
                   "fu_outcome" = -1:3))
#Calculate percent
dt.target[, "Number (%) after proportionally allocating unknown iron donations" := 
            paste0(round(target_dist,digits=0)," (",percent(target_dist/sum(target_dist),accuracy=0.1),")")]

#calculate outcomes by outcome
dt.byoutcome <- dt.frXB[,list("Number (%) in first return dataset" = paste0(.N," (",percent(.N/nrow(dt.frXB),accuracy=0.1),")"),
              "Median (mean) time to follow-up visit" = paste0(quantile(time_to_fu,.5)," (", round(mean(time_to_fu),digits=1),")")
              ),
        by=c("fu_outcome", "outcome")]
#Bring in target dist
dt.byoutcome<-dt.byoutcome[dt.target[,c(2,3)], on="fu_outcome"]
#Order the table by outcome
dt.byoutcome<-dt.byoutcome[order(outcome)]
dt.byoutcome<-dt.byoutcome[, c(2,3,5,4)]
setnames(dt.byoutcome, "outcome", "Follow-up visit outcome")

fwrite(dt.byoutcome, "./4_output/table_by_outcome.csv")



#Distributions for labeled vs. unlabeled ferritin

dt.plt <- dt.plt[, .N, by=c("time_to_fu", "fu_outcome")]
#truncate at 250 day IDI
dt.plt[,time_to_fu:=ifelse(time_to_fu >250,250,time_to_fu)]
dt.plt <- dt.plt[order(time_to_fu)]
dt.plt[, csum := cumsum(N), by=fu_outcome]

#library(ggbeeswarm)
ggplot(dt.plt, aes(x = time_to_fu, y = csum, color = factor(fu_outcome)))+
  geom_line()
