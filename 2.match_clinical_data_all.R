## import clinical data
clinical_data = read.csv("all_data/srData_cc.csv")
## import IDs that need to be removed
incomplete_cases = read.csv("all_data/incomplete_cases.csv")

############################# CBT_baseline 682 participants####################
clinical_temp = subset(clinical_data, clinical_data$treatment_group == 'cbt' &
                         !is.na(clinical_data$AD) & !is.na(clinical_data$AD_fu) &
                         !(clinical_data$subjID %in% incomplete_cases$ncPpts))
CBT_baseline = read.csv("clean_data/CBT_baseline.csv")

# format ID_short in clinical data
CBT_baseline['subjID'] <- sub("C1", "C", CBT_baseline$subjID)
intersectIDs = intersect(clinical_temp$subjID,unique(CBT_baseline$subjID))
# subset CBT_baseline (subjects with clinical match)
CBT_baseline_matched = subset(CBT_baseline,
                              CBT_baseline$subjID %in% intersectIDs)
write.csv(CBT_baseline_matched,
          file = 'clean_data/CBT_baseline_matched.csv',
          row.names = FALSE)
clinical_CBT_baseline = subset(clinical_temp, clinical_temp$subjID %in% intersectIDs)
write.csv(clinical_CBT_baseline,
          file = 'clean_data/clinical_CBT_baseline.csv',
          row.names = FALSE)
# clinical_CBT_baseline_scale <- clinical_CBT_baseline %>%
#   dplyr::mutate_at(c('AES_total', 'AUDIT_total', 'BIS_total', 'EAT_total',
#                      'LSAS_total', 'OCI_total', 'SCZ_total','SDS_total',
#                      'STAI_total', 'QIDS_total', 'WSAS_total'), ~(scale(.) %>% as.vector))
# write.csv(clinical_CBT_baseline_scale,
#           file = 'clean_data/clinical_CBT_baseline_scale.csv',
#           row.names = FALSE)

### Extract stable and volatile data ####
setwd("~/Documents/Rstudio/volatility_composit")
CBT_baseline_matched = read.csv("clean_data/CBT_baseline_matched.csv")
CBT_baseline_stable = subset(CBT_baseline_matched, CBT_baseline_matched$status == -1)
CBT_baseline_volatile = subset(CBT_baseline_matched, CBT_baseline_matched$status == 1)
write.csv(CBT_baseline_stable,
          file = 'clean_data/CBT_baseline_matched_stable.csv',
          row.names = FALSE)
write.csv(CBT_baseline_volatile,
          file = 'clean_data/CBT_baseline_matched_volatile.csv',
          row.names = FALSE)

### Join choice data and clinical data ####
CBT_baseline_matched = read.csv("clean_data/CBT_baseline_matched.csv")
clinical_CBT_baseline = read.csv("clean_data/clinical_CBT_baseline.csv")
CBT_baseline_merged = merge(CBT_baseline_matched, clinical_CBT_baseline)
write.csv(CBT_baseline_merged,
          file = 'clean_data/CBT_baseline_merged.csv',
          row.names = FALSE)


############################# CBT_followup 682 participants####################
CBT_followup = read.csv("clean_data/CBT_followup.csv")

# format ID_short in clinical data
CBT_followup['subjID'] <- sub("C4", "C", CBT_followup$subjID)
intersectIDs = intersect(clinical_temp$subjID,unique(CBT_followup$subjID))
# subset CBT_followup (subjects with clinical match)
CBT_followup_matched = subset(CBT_followup,
                              CBT_followup$subjID %in% intersectIDs)
write.csv(CBT_followup_matched,
          file = 'clean_data/CBT_followup_matched.csv',
          row.names = FALSE)
clinical_CBT_followup = subset(clinical_temp, clinical_temp$subjID %in% intersectIDs)
write.csv(clinical_CBT_followup,
          file = 'clean_data/clinical_CBT_followup.csv',
          row.names = FALSE)
# clinical_CBT_followup_scale <- clinical_CBT_followup %>%
#   dplyr::mutate_at(c('AES_total', 'AUDIT_total', 'BIS_total', 'EAT_total',
#                      'LSAS_total', 'OCI_total', 'SCZ_total','SDS_total',
#                      'STAI_total', 'QIDS_total', 'WSAS_total'), ~(scale(.) %>% as.vector))
# write.csv(clinical_CBT_followup_scale,
#           file = 'clean_data/clinical_CBT_followup_scale.csv',
#           row.names = FALSE)

### Extract stable and volatile data ####
CBT_followup_matched = read.csv("clean_data/CBT_followup_matched.csv")
CBT_followup_stable = subset(CBT_followup_matched, CBT_followup_matched$status == -1)
CBT_followup_volatile = subset(CBT_followup_matched, CBT_followup_matched$status == 1)
write.csv(CBT_followup_stable,
          file = 'clean_data/CBT_followup_matched_stable.csv',
          row.names = FALSE)
write.csv(CBT_followup_volatile,
          file = 'clean_data/CBT_followup_matched_volatile.csv',
          row.names = FALSE)

### Join choice data and clinical data ######
setwd("~/Documents/Rstudio/volatility_composit")
CBT_followup_matched = read.csv("clean_data/CBT_followup_matched.csv")
clinical_CBT_followup = read.csv("clean_data/clinical_CBT_followup.csv")
CBT_followup_merged = merge(CBT_followup_matched, clinical_CBT_followup)
write.csv(CBT_followup_merged,
          file = 'clean_data/CBT_followup_merged.csv',
          row.names = FALSE)


############################# SSRI_baseline 95 participants####################
clinical_temp = subset(clinical_data, clinical_data$treatment == 'ssri' &
                         !is.na(clinical_data$AD) & !is.na(clinical_data$AD_fu) &
                         !(clinical_data$subjID %in% incomplete_cases$ncPpts))
SSRI_baseline = read.csv("clean_data/SSRI_baseline.csv")

# format ID_short in clinical data
SSRI_baseline['subjID'] <- sub(".", "S", SSRI_baseline$subjID)
intersectIDs = intersect(clinical_temp$subjID, unique(SSRI_baseline$subjID))
# subset CBT_baseline (subjects with clinical match)
SSRI_baseline_matched = subset(SSRI_baseline,
                               SSRI_baseline$subjID %in% intersectIDs)
write.csv(SSRI_baseline_matched,
          file = 'clean_data/SSRI_baseline_matched.csv',
          row.names = FALSE)
clinical_SSRI_baseline = subset(clinical_temp, clinical_temp$subjID %in% intersectIDs)
write.csv(clinical_SSRI_baseline,
          file = 'clean_data/clinical_SSRI_baseline.csv',
          row.names = FALSE)

### Extract stable and volatile data ####
SSRI_baseline_matched = read.csv("clean_data/SSRI_baseline_matched.csv")
SSRI_baseline_stable = subset(SSRI_baseline_matched, SSRI_baseline_matched$status == -1)
SSRI_baseline_volatile = subset(SSRI_baseline_matched, SSRI_baseline_matched$status == 1)
write.csv(SSRI_baseline_stable,
          file = 'clean_data/SSRI_baseline_matched_stable.csv',
          row.names = FALSE)
write.csv(SSRI_baseline_volatile,
          file = 'clean_data/SSRI_baseline_matched_volatile.csv',
          row.names = FALSE)


### Join choice data and clinical data ######
SSRI_baseline_matched = read.csv("clean_data/SSRI_baseline_matched.csv")
clinical_SSRI_baseline = read.csv("clean_data/clinical_SSRI_baseline.csv")
SSRI_baseline_merged = merge(SSRI_baseline_matched, clinical_SSRI_baseline)
write.csv(SSRI_baseline_merged,
          file = 'clean_data/SSRI_baseline_merged.csv',
          row.names = FALSE)


############################# SSRI_followup 95 participants####################
SSRI_followup = read.csv("clean_data/SSRI_followup.csv")

# format ID_short in clinical data
SSRI_followup['subjID'] <- sub(".", "S", SSRI_followup$subjID)
intersectIDs = intersect(clinical_temp$subjID,unique(SSRI_followup$subjID))
# subset CBT_baseline (subjects with clinical match)
SSRI_followup_matched = subset(SSRI_followup,
                               SSRI_followup$subjID %in% intersectIDs)
write.csv(SSRI_followup_matched,
          file = 'clean_data/SSRI_followup_matched.csv',
          row.names = FALSE)
clinical_SSRI_followup = subset(clinical_temp, clinical_temp$subjID %in% intersectIDs)
write.csv(clinical_SSRI_followup,
          file = 'clean_data/clinical_SSRI_followup.csv',
          row.names = FALSE)

### Extract stable and volatile data ####
SSRI_followup_matched = read.csv("clean_data/SSRI_followup_matched.csv")
SSRI_followup_stable = subset(SSRI_followup_matched, SSRI_followup_matched$status == -1)
SSRI_followup_volatile = subset(SSRI_followup_matched, SSRI_followup_matched$status == 1)
write.csv(SSRI_followup_stable,
          file = 'clean_data/SSRI_followup_matched_stable.csv',
          row.names = FALSE)
write.csv(SSRI_followup_volatile,
          file = 'clean_data/SSRI_followup_matched_volatile.csv',
          row.names = FALSE)

### Join choice data and clinical data ######
SSRI_followup_matched = read.csv("clean_data/SSRI_followup_matched.csv")
clinical_SSRI_followup = read.csv("clean_data/clinical_SSRI_followup.csv")
SSRI_followup_merged = merge(SSRI_followup_matched, clinical_SSRI_followup)
write.csv(SSRI_followup_merged,
          file = 'clean_data/SSRI_followup_merged.csv',
          row.names = FALSE)



############################# Control_baseline 95 participants####################
clinical_temp = subset(clinical_data, clinical_data$treatment == 'control' &
                         !is.na(clinical_data$AD) & !is.na(clinical_data$AD_fu) &
                         !(clinical_data$subjID %in% incomplete_cases$ncPpts))
control_baseline = read.csv("clean_data/control_baseline.csv")

# format ID_short in clinical data
control_baseline['subjID'] <- sub("H1", "H", control_baseline$subjID)
intersectIDs = intersect(clinical_temp$subjID, unique(control_baseline$subjID))
# subset CBT_baseline (subjects with clinical match)
control_baseline_matched = subset(control_baseline,
                                  control_baseline$subjID %in% intersectIDs)
write.csv(control_baseline_matched,
          file = 'clean_data/control_baseline_matched.csv',
          row.names = FALSE)
clinical_control_baseline = subset(clinical_temp, clinical_temp$subjID %in% intersectIDs)
write.csv(clinical_control_baseline,
          file = 'clean_data/clinical_control_baseline.csv',
          row.names = FALSE)

### Extract stable and volatile data ####
control_baseline_matched = read.csv("clean_data/control_baseline_matched.csv")
control_baseline_stable = subset(control_baseline_matched, control_baseline_matched$status == -1)
control_baseline_volatile = subset(control_baseline_matched, control_baseline_matched$status == 1)
write.csv(control_baseline_stable,
          file = 'clean_data/control_baseline_matched_stable.csv',
          row.names = FALSE)
write.csv(control_baseline_volatile,
          file = 'clean_data/control_baseline_matched_volatile.csv',
          row.names = FALSE)

### Join choice data and clinical data ######
control_baseline_matched = read.csv("clean_data/control_baseline_matched.csv")
clinical_control_baseline = read.csv("clean_data/clinical_control_baseline.csv")
control_baseline_merged = merge(control_baseline_matched, clinical_control_baseline)
write.csv(control_baseline_merged,
          file = 'clean_data/control_baseline_merged.csv',
          row.names = FALSE)


############################# Control_followup 95 participants####################
control_followup = read.csv("clean_data/control_followup.csv")

# format ID_short in clinical data
control_followup['subjID'] <- sub("H4", "H", control_followup$subjID)
intersectIDs = intersect(clinical_temp$subjID,unique(control_followup$subjID))
# subset CBT_baseline (subjects with clinical match)
control_followup_matched = subset(control_followup,
                                  control_followup$subjID %in% intersectIDs)
write.csv(control_followup_matched,
          file = 'clean_data/control_followup_matched.csv',
          row.names = FALSE)
clinical_control_followup = subset(clinical_temp, clinical_temp$subjID %in% intersectIDs)
write.csv(clinical_control_followup,
          file = 'clean_data/clinical_control_followup.csv',
          row.names = FALSE)

### Extract stable and volatile data ####
control_followup_matched = read.csv("clean_data/control_followup_matched.csv")
control_followup_stable = subset(control_followup_matched, control_followup_matched$status == -1)
control_followup_volatile = subset(control_followup_matched, control_followup_matched$status == 1)
write.csv(control_followup_stable,
          file = 'clean_data/control_followup_matched_stable.csv',
          row.names = FALSE)
write.csv(control_followup_volatile,
          file = 'clean_data/control_followup_matched_volatile.csv',
          row.names = FALSE)


### Join choice data and clinical data ######
control_followup_matched = read.csv("clean_data/control_followup_matched.csv")
clinical_Control_followup = read.csv("clean_data/clinical_control_followup.csv")
control_followup_merged = merge(control_followup_matched, clinical_control_followup)
write.csv(control_followup_merged,
          file = 'clean_data/control_followup_merged.csv',
          row.names = FALSE)















