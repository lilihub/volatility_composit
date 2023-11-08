library(cowplot)
library(ggplot2)
library(ggpubr)
CBT_par = read.csv("fitted_data/CBT_par.csv")
SSRI_par = read.csv("fitted_data/SSRI_par.csv")
control_par = read.csv("fitted_data/control_par.csv")

clinical_CBT = read.csv("clean_data/clinical_CBT_baseline.csv")
clinical_SSRI = read.csv("clean_data/clinical_SSRI_baseline.csv")
clinical_control = read.csv("clean_data/clinical_control_baseline.csv")


CBT_merged = merge(CBT_par, clinical_CBT, by="subjID")
CBT_merged['baseline_A_diff'] = CBT_merged['baseline_A_volatile'] - CBT_merged['baseline_A_stable']

t.test(CBT_merged$baseline_A_stable, CBT_merged$baseline_A_volatile, paired = TRUE, alternative = "two.sided")
t.test(CBT_merged$followup_A_stable, CBT_merged$followup_A_volatile, paired = TRUE, alternative = "two.sided")
t.test(CBT_merged$baseline_A_stable, CBT_merged$followup_A_stable, paired = TRUE, alternative = "two.sided")
t.test(CBT_merged$baseline_A_volatile, CBT_merged$followup_A_volatile, paired = TRUE, alternative = "two.sided")


model <- lm(baseline_A_diff ~ AD, data = CBT_merged)
summary(model)

ggscatter(CBT_merged, x = "baseline_A_diff", y = "AD", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = expression(diff_A), 
          ylab = expression(AD))
