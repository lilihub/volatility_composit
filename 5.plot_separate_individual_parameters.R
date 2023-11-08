library(rstan)
library(tidyverse)
library(cowplot)
library(stringr)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(smplot2)
#### control group clean data
control_baseline_schedule1_stable = read.csv('clean_data/control_baseline_schedule1_stable_matched.csv')
control_baseline_schedule2_stable = read.csv('clean_data/control_baseline_schedule2_stable_matched.csv')
subjID1 = as.character(unique(control_baseline_schedule1_stable$subjID))
subjID2 = as.character(unique(control_baseline_schedule2_stable$subjID))

chris_model2_control_baseline_individual_par = read.csv("fitted_data/chris_model2/control_baseline_individual_par.csv")
chris_model2_ev_control_baseline_individual_par = read.csv("fitted_data/chris_model2_ev/control_baseline_individual_par.csv")
mike_baseline_control_basleine_individual_par = read.csv("fitted_data/mike_baseline/control_baseline_individual_par.csv")
mike_baseline_updated_control_baseline_individual_par = read.csv("fitted_data/mike_baseline_updated/control_baseline_individual_par.csv")
mike_baseline_updated_ev_control_baseline_individual_par = read.csv("fitted_data/mike_baseline_updated_ev/control_baseline_individual_par.csv")
mike_MLE_control_baseline_individual_par = read.csv("fitted_data/MLE/mike_MLE_control_baseline_matched.csv")

colnames(chris_model2_control_baseline_individual_par) = c("subjID", "chris_model2_stable_A", "chris_model2_volatile_A",
                                                           "chris_model2_stable_beta", "chris_model2_volatile_beta",
                                                           "chris_model2_stable_gamma","chris_model2_volatile_gamma", "schedule")
colnames(chris_model2_ev_control_baseline_individual_par) = c("subjID", "chris_model2_ev_stable_A", "chris_model2_ev_volatile_A",
                                                              "chris_model2_ev_stable_beta", "chris_model2_ev_volatile_beta",
                                                              "chris_model2_ev_stable_gamma","chris_model2_ev_volatile_gamma", "schedule")

colnames(mike_baseline_control_basleine_individual_par) = c("subjID", "mike_baseline_stable_A", "mike_baseline_volatile_A",
                                                            "mike_baseilne_stable_beta", "mike_baseline_volatile_beta",
                                                            "mike_baseline_stable_gamma","mike_baseline_volatile_gamma", "schedule")
colnames(mike_baseline_updated_control_baseline_individual_par) = c("subjID", "mike_baseline_updated_stable_A", "mike_baseline_updated_volatile_A",
                                                                    "mike_baseline_updated_stable_beta", "mike_baseline_updated_volatile_beta",
                                                                    "mike_baseline_updated_stable_gamma","mike_baseline_updated_volatile_gamma", "schedule")
colnames(mike_baseline_updated_ev_control_baseline_individual_par) = c("subjID", "mike_baseline_updated_ev_stable_A", "mike_baseline_updated_ev_volatile_A",
                                                                       "mike_baseline_updated_ev_stable_beta", "mike_baseline_updated_ev_volatile_beta",
                                                                       "mike_baseline_updated_ev_stable_gamma","mike_baseline_updated_ev_volatile_gamma", "schedule")

colnames(mike_MLE_control_baseline_individual_par) = c("subjID", "mike_MLE_stable_A", "mike_MLE_volatile_A",
                                                       "mike_MLE_stable_gamma", "mike_MLE_volatile_gamma",
                                                       "mike_MLE_stable_beta","mike_MLE_volatile_beta", "schedule", "diff", "paired")
#put all data frames into list
df_list <- list(chris_model2_control_baseline_individual_par,
                chris_model2_ev_control_baseline_individual_par,
                mike_baseline_control_basleine_individual_par,
                mike_baseline_updated_control_baseline_individual_par,
                mike_baseline_updated_ev_control_baseline_individual_par,
                mike_MLE_control_baseline_individual_par)

#merge all data frames in list
merged_df <- df_list %>% reduce(full_join, by=c("subjID", "schedule"))
merged_df$schedule = as.factor(merged_df$schedule)

####################### plot individual parameter correlations between different models ###########################
###### mike_MLE versus mike_baseline_updated ########
ggplot(merged_df, aes(x=mike_MLE_stable_A, y=mike_baseline_updated_stable_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  xlim(0,0.8) + 
  ylim(0,0.8) +
  ggtitle("mike_MLE vs. mike_baseline_updated") +
  xlab("mike_MLE_control_baseline_stable_A") +
  ylab("mike_baseline_updated_control_baseline_stable_A") +
  sm_statCorr() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_stable_A, y=mike_baseline_updated_ev_stable_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  xlim(0,0.8) + 
  ylim(0,0.8) +
  ggtitle("mike_MLE vs. mike_baseline_updated_ev") +
  xlab("mike_MLE_control_baseline_stable_A") +
  ylab("mike_baseline_updated_ev_control_baseline_stable_A") +
  sm_statCorr() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")

ggplot(merged_df, aes(x=mike_MLE_volatile_A, y=mike_baseline_updated_volatile_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  xlim(0,0.8) + 
  ylim(0,0.8) +
  ggtitle("mike_MLE vs. mike_baseline_updated") +
  xlab("mike_MLE_control_baseline_volatile_A") +
  ylab("mike_baseline_updated_control_baseline_volatile_A") +
  sm_statCorr() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_volatile_A, y=mike_baseline_updated_ev_volatile_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  xlim(0,0.8) + 
  ylim(0,0.8) +
  ggtitle("mike_MLE vs. mike_baseline_updated_ev") +
  xlab("mike_MLE_control_baseline_volatile_A") +
  ylab("mike_baseline_updated_ev_control_baseline_volatile_A") +
  sm_statCorr()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")


ggplot(merged_df, aes(x=mike_MLE_stable_gamma, y=mike_baseline_updated_stable_gamma,))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,6) +
  ylim(0,6) +
  ggtitle("mike_MLE vs. mike_baseline_updated") +
  xlab("mike_MLE_control_baseline_stable_gamma") +
  ylab("mike_baseline_updated_control_baseline_stable_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_stable_gamma, y=mike_baseline_updated_ev_stable_gamma))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,6) + 
  ylim(0,6) +
  ggtitle("mike_MLE vs. mike_baseline_updated_ev") +
  xlab("mike_MLE_control_baseline_stable_gamma") +
  ylab("mike_baseline_updated_ev_control_baseline_stable_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")

ggplot(merged_df, aes(x=mike_MLE_volatile_gamma, y=mike_baseline_updated_volatile_gamma,))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,8) +
  ylim(0,8) +
  ggtitle("mike_MLE vs. mike_baseline_updated") +
  xlab("mike_MLE_control_baseline_volatile_gamma") +
  ylab("mike_baseline_updated_control_baseline_volatile_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_volatile_gamma, y=mike_baseline_updated_ev_volatile_gamma))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,8) + 
  ylim(0,8) +
  ggtitle("mike_MLE vs. mike_baseline_updated_ev") +
  xlab("mike_MLE_control_baseline_volatile_gamma") +
  ylab("mike_baseline_updated_ev_control_baseline_volatile_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")

ggplot(merged_df, aes(x=mike_MLE_stable_beta, y=mike_baseline_updated_ev_stable_beta))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("mike_MLE vs. mike_baseline_updated_ev") +
  xlab("mike_MLE_control_baseline_stable_beta") +
  ylab("mike_baseline_updated_ev_control_baseline_stable_beta") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_volatile_beta, y=mike_baseline_updated_ev_volatile_beta))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("mike_MLE vs. mike_baseline_updated_ev") +
  xlab("mike_MLE_control_baseline_volatile_beta") +
  ylab("mike_baseline_updated_ev_control_baseline_volatile_beta") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")

###### chris_model2 versus mike_baseline_updated ########
ggplot(merged_df, aes(x=chris_model2_stable_A, y=mike_baseline_updated_stable_A))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,0.6) + 
  ylim(0,0.6) +
  ggtitle("chris_model2 vs. mike_baseline_updated") +
  xlab("chris_model2_control_baseline_stable_A") +
  ylab("mike_baseline_updated_control_baseline_stable_A") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=chris_model2_volatile_A, y=mike_baseline_updated_volatile_A))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,0.6) + 
  ylim(0,0.6) +
  ggtitle("chris_model2 vs. mike_baseline_updated") +
  xlab("chris_model2_control_baseline_volatile_A") +
  ylab("mike_baseline_updated_control_baseline_volatile_A") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=chris_model2_stable_gamma, y=mike_baseline_updated_stable_gamma,))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("chris_model2 vs. mike_baseline_updated") +
  xlab("chris_model2_control_baseline_stable_gamma") +
  ylab("mike_baseline_updated_control_baseline_stable_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")

ggplot(merged_df, aes(x=chris_model2_volatile_gamma, y=mike_baseline_updated_volatile_gamma))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("chris_model2 vs. mike_baseline_updated") +
  xlab("chris_model2_control_baseline_volatile_gamma") +
  ylab("mike_baseline_updated_control_baseline_volatile_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=chris_model2_stable_beta, y=mike_baseline_updated_stable_beta))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,10) + 
  ylim(0,10) +
  ggtitle("chris_model2 vs. mike_baseline_updated") +
  xlab("chris_model2_control_baseline_stable_beta") +
  ylab("mike_baseline_updated_control_baseline_stable_beta") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=chris_model2_volatile_beta, y=mike_baseline_updated_volatile_beta))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,10) + 
  ylim(0,10) +
  ggtitle("chris_model2 vs. mike_baseline_updated") +
  xlab("chris_model2_control_baseline_volatile_beta") +
  ylab("mike_baseline_updated_control_baseline_volatile_beta") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")

###### chris_model2 versus chris_model2_ev ########
ggplot(merged_df, aes(x=chris_model2_stable_A, y=chris_model2_ev_stable_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,0.6) + 
  ylim(0,0.6) +
  ggtitle("chris_model2 vs. chris_model2_ev") +
  xlab("chris_model2_control_baseline_stable_A") +
  ylab("chris_model2_ev_control_baseline_stable_A") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=chris_model2_volatile_A, y=chris_model2_ev_volatile_A))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,0.6) + 
  ylim(0,0.6) +
  ggtitle("chris_model2 vs. chris_model2_ev") +
  xlab("chris_model2_control_baseline_volatile_A") +
  ylab("chris_model2_ev_control_baseline_volatile_A") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=chris_model2_stable_gamma, y=chris_model2_ev_stable_gamma,))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("chris_model2 vs. chris_model2_ev") +
  xlab("chris_model2_control_baseline_stable_gamma") +
  ylab("chris_model2_ev_control_baseline_stable_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")

ggplot(merged_df, aes(x=chris_model2_volatile_gamma, y=chris_model2_ev_volatile_gamma))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("chris_model2 vs. mike_baseline_updated") +
  xlab("chris_model2_control_baseline_volatile_gamma") +
  ylab("chris_model2_ev_control_baseline_volatile_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=chris_model2_stable_beta, y=chris_model2_ev_stable_beta))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,10) + 
  ylim(0,10) +
  ggtitle("chris_model2 vs. chris_model2_ev") +
  xlab("chris_model2_control_baseline_stable_beta") +
  ylab("chris_model2_ev_control_baseline_stable_beta") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=chris_model2_volatile_beta, y=chris_model2_ev_volatile_beta))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,10) + 
  ylim(0,10) +
  ggtitle("chris_model2 vs. chris_model2_ev") +
  xlab("chris_model2_control_baseline_volatile_beta") +
  ylab("chrism_model2_ev_control_baseline_volatile_beta") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")


###### mike_MLE versus chris_model2 ########
ggplot(merged_df, aes(x=mike_MLE_stable_A, y=chris_model2_stable_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,0.8) + 
  ylim(0,0.8) +
  ggtitle("mike_MLE vs. chris_model2") +
  xlab("mike_MLE_control_baseline_stable_A") +
  ylab("chris_model2_control_baseline_stable_A") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_stable_A, y=chris_model2_ev_stable_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,0.8) + 
  ylim(0,0.8) +
  ggtitle("mike_MLE vs. chris_model2_ev") +
  xlab("mike_MLE_control_baseline_stable_A") +
  ylab("chris_model2_ev_control_baseline_stable_A") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")


ggplot(merged_df, aes(x=mike_MLE_volatile_A, y=chris_model2_volatile_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,0.6) + 
  ylim(0,0.6) +
  ggtitle("mike_MLE vs. chris_model2") +
  xlab("mike_MLE_control_baseline_volatile_A") +
  ylab("chris_model2_control_baseline_volatile_A") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_volatile_A, y=chris_model2_ev_volatile_A))+
  geom_point(aes(color = schedule))+
  # geom_abline(intercept = 0, slope = 1) +
  sm_statCorr()+
  xlim(0,0.6) + 
  ylim(0,0.6) +
  ggtitle("mike_MLE vs. chris_model2_ev") +
  xlab("mike_MLE_control_baseline_volatile_A") +
  ylab("chris_model2_ev_control_baseline_volatile_A") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")


ggplot(merged_df, aes(x=mike_MLE_stable_gamma, y=chris_model2_ev_stable_gamma))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("mike_MLE vs. chris_model2_ev") +
  xlab("mike_MLE_control_baseline_stable_gamma") +
  ylab("chris_model2_ev_control_baseline_stable_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_volatile_gamma, y=chris_model2_ev_volatile_gamma))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("mike_MLE vs. chris_model2_ev") +
  xlab("mike_MLE_control_baseline_volatile_gamma") +
  ylab("chris_model2_ev_control_baseline_volatile_gamma") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_stable_beta, y=chris_model2_ev_stable_beta))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("mike_MLE vs. chris_model2_ev") +
  xlab("mike_MLE_control_baseline_stable_beta") +
  ylab("chris_model2_ev_control_baseline_stable_beta") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")
ggplot(merged_df, aes(x=mike_MLE_volatile_beta, y=chris_model2_ev_volatile_beta))+
  geom_point(aes(color = schedule))+
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("mike_MLE vs. chris_model2_ev") +
  xlab("mike_MLE_control_baseline_volatile_beta") +
  ylab("chris_model2_ev_control_baseline_volatile_beta") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="top")


############################### plot individual parameter change #####################################
##### mike_baseline_updated_ev
### constract formatted dataframe
mike_baseline_updated_ev_control_baseline_df = data.frame(subjID = rep(merged_df$subjID, time = 2),
                                                          A = c(merged_df$mike_baseline_updated_ev_stable_A, merged_df$mike_baseline_updated_ev_volatile_A),
                                                          beta = c(merged_df$mike_baseline_updated_ev_stable_beta, merged_df$mike_baseline_updated_ev_volatile_beta),
                                                          gamma = c(merged_df$mike_baseline_updated_ev_stable_gamma, merged_df$mike_baseline_updated_ev_volatile_gamma),
                                                          condition = c(rep("stable", time = length(subjID1) + length(subjID2)),
                                                                        rep("volatile", time = length(subjID1) + length(subjID2))),
                                                          schedule = rep(merged_df$schedule, time = 2),
                                                          paired = rep(1:(length(subjID1) + length(subjID2)), time = 2))
mike_baseline_updated_ev_control_baseline_df['combine'] = paste0(mike_baseline_updated_ev_control_baseline_df$schedule, 
                                                                 mike_baseline_updated_ev_control_baseline_df$condition) 
mike_baseline_updated_ev_control_baseline_df$combine = factor(mike_baseline_updated_ev_control_baseline_df$combine, 
                                                              levels = c('1stable', '1volatile', '2volatile', '2stable'))
ggplot(mike_baseline_updated_ev_control_baseline_df,aes(x=combine,y=A,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(A))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+
  scale_y_log10()+labs(title = "mike_baseline_updated_ev_control_baseline_A")

ggplot(mike_baseline_updated_ev_control_baseline_df,aes(x=combine,y=gamma,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(gamma))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "mike_baseline_updated_ev_control_baseline_gamma")

ggplot(mike_baseline_updated_ev_control_baseline_df,aes(x=combine,y=beta,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(beta))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "mike_baseline_updated_ev_control_baseline_beta")

##### mike_baseline_updated
### constract formatted dataframe
mike_baseline_updated_control_baseline_df = data.frame(subjID = rep(merged_df$subjID, time = 2),
                                                       A = c(merged_df$mike_baseline_updated_stable_A, merged_df$mike_baseline_updated_volatile_A),
                                                       beta = c(merged_df$mike_baseline_updated_stable_beta, merged_df$mike_baseline_updated_volatile_beta),
                                                       gamma = c(merged_df$mike_baseline_updated_stable_gamma, merged_df$mike_baseline_updated_volatile_gamma),
                                                       condition = c(rep("stable", time = length(subjID1) + length(subjID2)),
                                                                     rep("volatile", time = length(subjID1) + length(subjID2))),
                                                       schedule = rep(merged_df$schedule, time = 2),
                                                       paired = rep(1:(length(subjID1) + length(subjID2)), time = 2))
mike_baseline_updated_control_baseline_df['combine'] = paste0(mike_baseline_updated_control_baseline_df$schedule, 
                                                              mike_baseline_updated_control_baseline_df$condition) 
mike_baseline_updated_control_baseline_df$combine = factor(mike_baseline_updated_control_baseline_df$combine, 
                                                           levels = c('1stable', '1volatile', '2volatile', '2stable'))
ggplot(mike_baseline_updated_control_baseline_df,aes(x=combine,y=A,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(A))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+
  scale_y_log10()+labs(title = "mike_baseline_updated_control_baseline_A")

ggplot(mike_baseline_updated_control_baseline_df,aes(x=combine,y=gamma,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(gamma))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "mike_baseline_updated_control_baseline_gamma")

ggplot(mike_baseline_updated_control_baseline_df,aes(x=combine,y=beta,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(beta))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "mike_baseline_updated_control_baseline_beta")

##### chris model2
chris_model2_control_baseline_df = data.frame(subjID = rep(merged_df$subjID, time = 2),
                                              A = c(merged_df$chris_model2_stable_A, merged_df$chris_model2_volatile_A),
                                              beta = c(merged_df$chris_model2_stable_beta, merged_df$chris_model2_volatile_beta),
                                              gamma = c(merged_df$chris_model2_stable_gamma, merged_df$chris_model2_volatile_gamma),
                                              condition = c(rep("stable", time = length(subjID1) + length(subjID2)),
                                                            rep("volatile", time = length(subjID1) + length(subjID2))),
                                              schedule = rep(merged_df$schedule, time = 2),
                                              paired = rep(1:(length(subjID1) + length(subjID2)), time = 2))
chris_model2_control_baseline_df['combine'] = paste0(chris_model2_control_baseline_df$schedule, 
                                                     chris_model2_control_baseline_df$condition) 
chris_model2_control_baseline_df$combine = factor(chris_model2_control_baseline_df$combine, 
                                                  levels = c('1stable', '1volatile', '2volatile', '2stable'))
ggplot(chris_model2_control_baseline_df,aes(x=combine,y=A,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(A))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+
  scale_y_log10()+labs(title = "chris_model2_control_baseline_A")
ggplot(chris_model2_control_baseline_df,aes(x=combine,y=gamma,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(gamma))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "chris_model2_control_baseline_gamma")
ggplot(chris_model2_control_baseline_df,aes(x=combine,y=beta,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(beta))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "chris_model2_control_baseline_beta")

##### chris model2_ev
chris_model2_ev_control_baseline_df = data.frame(subjID = rep(merged_df$subjID, time = 2),
                                                 A = c(merged_df$chris_model2_ev_stable_A, merged_df$chris_model2_ev_volatile_A),
                                                 beta = c(merged_df$chris_model2_ev_stable_beta, merged_df$chris_model2_ev_volatile_beta),
                                                 gamma = c(merged_df$chris_model2_ev_stable_gamma, merged_df$chris_model2_ev_volatile_gamma),
                                                 condition = c(rep("stable", time = length(subjID1) + length(subjID2)),
                                                               rep("volatile", time = length(subjID1) + length(subjID2))),
                                                 schedule = rep(merged_df$schedule, time = 2),
                                                 paired = rep(1:(length(subjID1) + length(subjID2)), time = 2))
chris_model2_ev_control_baseline_df['combine'] = paste0(chris_model2_ev_control_baseline_df$schedule, 
                                                        chris_model2_ev_control_baseline_df$condition) 
chris_model2_ev_control_baseline_df$combine = factor(chris_model2_ev_control_baseline_df$combine, 
                                                     levels = c('1stable', '1volatile', '2volatile', '2stable'))
ggplot(chris_model2_ev_control_baseline_df,aes(x=combine,y=A,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(A))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+
  scale_y_log10()+labs(title = "chris_model2_ev_control_baseline_A")
ggplot(chris_model2_ev_control_baseline_df,aes(x=combine,y=gamma,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(gamma))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "chris_model2_ev_control_baseline_gamma")
ggplot(chris_model2_ev_control_baseline_df,aes(x=combine,y=beta,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(beta))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "chris_model2_ev_control_baseline_beta")


##### mike MLE
mike_MLE_control_baseline_df = data.frame(subjID = rep(merged_df$subjID, time = 2),
                                          A = c(merged_df$mike_MLE_stable_A, merged_df$mike_MLE_volatile_A),
                                          beta = c(merged_df$mike_MLE_stable_beta, merged_df$mike_MLE_volatile_beta),
                                          gamma = c(merged_df$mike_MLE_stable_gamma, merged_df$mike_MLE_volatile_gamma),
                                          condition = c(rep("stable", time = length(subjID1) + length(subjID2)),
                                                        rep("volatile", time = length(subjID1) + length(subjID2))),
                                          schedule = rep(merged_df$schedule, time = 2),
                                          paired = rep(1:(length(subjID1) + length(subjID2)), time = 2))
mike_MLE_control_baseline_df['combine'] = paste0(mike_MLE_control_baseline_df$schedule, 
                                                 mike_MLE_control_baseline_df$condition) 
mike_MLE_control_baseline_df$combine = factor(mike_MLE_control_baseline_df$combine, 
                                              levels = c('1stable', '1volatile', '2volatile', '2stable'))
ggplot(mike_MLE_control_baseline_df,aes(x=combine,y=A,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(A))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+
  scale_y_log10()+labs(title = "mike_MLE_control_baseline_A")
ggplot(mike_MLE_control_baseline_df,aes(x=combine,y=gamma,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(gamma))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "mike_MLE_control_baseline_gamma")
ggplot(mike_MLE_control_baseline_df,aes(x=combine,y=beta,fill=condition,col=condition))+
  geom_point(aes(color=condition),size=2) +
  geom_line(aes(group = paired),color="grey") +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(fill = FALSE, col = FALSE)+
  ylab(expression(beta))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+labs(title = "mike_MLE_control_baseline_beta")
































######################### CBT convergence check ####################################
CBT_baseline_composit <- readRDS("fitted_data/CBT_baseline_composit2_fit.rda")
CBT_baseline_summary <- rstan::summary(CBT_baseline_composit)
CBT_baseline_rhat <- data.frame(Rhat = CBT_baseline_summary[["summary"]][, "Rhat"])
CBT_baseline_unnormal_rhat <- subset(CBT_baseline_rhat, CBT_baseline_rhat$Rhat>1.1)
hist(CBT_baseline_rhat$Rhat)

CBT_baseline_composit_pars <- rstan::extract(CBT_baseline_composit)
mu = as.data.frame(CBT_baseline_composit_pars[1])
colnames(mu) = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma')
mcmc_trace(mu,  pars=c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma'),
           facet_args = list(nrow = 2, labeller = label_parsed))



CBT_followup_composit <- readRDS("fitted_data/CBT_followup_composit2_fit.rda")
CBT_followup_summary <- rstan::summary(CBT_followup_composit)
CBT_followup_rhat <- data.frame(Rhat = CBT_followup_summary[["summary"]][, "Rhat"])
CBT_followup_unnormal_rhat <- subset(CBT_followup_rhat, CBT_followup_rhat$Rhat>1.1)
hist(CBT_followup_rhat$Rhat)

CBT_followup_composit_pars <- rstan::extract(CBT_followup_composit)
mu = as.data.frame(CBT_followup_composit_pars[1])
colnames(mu) = c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma')
mcmc_trace(mu,  pars=c('A_base', 'A_vs', 'A_gb', 'A_vs_gb', 'beta', 'gamma'),
           facet_args = list(nrow = 2, labeller = label_parsed))


######################### SSRI convergence check ####################################
SSRI_baseline_composit <- readRDS("fitted_data/SSRI_baseline_composit2_fit.rda")
SSRI_baseline_summary <- rstan::summary(SSRI_baseline_composit)
SSRI_baseline_rhat <- data.frame(Rhat = SSRI_baseline_summary[["summary"]][, "Rhat"])
SSRI_baseline_unnormal_rhat <- subset(SSRI_baseline_rhat, SSRI_baseline_rhat$Rhat>1.1)
hist(SSRI_baseline_rhat$Rhat)

SSRI_followup_composit <- readRDS("fitted_data/SSRI_followup_composit2_fit.rda")
SSRI_followup_summary <- rstan::summary(SSRI_followup_composit)
SSRI_followup_rhat <- data.frame(Rhat = SSRI_followup_summary[["summary"]][, "Rhat"])
SSRI_followup_unnormal_rhat <- subset(SSRI_followup_rhat, SSRI_followup_rhat$Rhat>1.1)

######################### Healthy control convergence check ####################################
control_baseline_composit <- readRDS("fitted_data/control_baseline_composit2_fit.rda")
control_baseline_summary <- rstan::summary(control_baseline_composit)
control_baseline_rhat <- data.frame(Rhat = control_baseline_summary[["summary"]][, "Rhat"])
control_baseline_unnormal_rhat <- subset(control_baseline_rhat, control_baseline_rhat$Rhat>1.1)
hist(control_baseline_rhat$Rhat)

control_followup_composit <- readRDS("fitted_data/control_followup_composit2_fit.rda")
control_followup_summary <- rstan::summary(control_followup_composit)
control_followup_rhat <- data.frame(Rhat = control_followup_summary[["summary"]][, "Rhat"])
control_followup_unnormal_rhat <- subset(control_followup_rhat, control_followup_rhat$Rhat>1.1)
hist(control_followup_rhat$Rhat)


