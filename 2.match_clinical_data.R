## import clinical data
clinical_data = read.csv("original_data/qnData_prepost.csv")

############################# CBT_baseline ####################
clinical_temp = subset(clinical_data, clinical_data$treatment == 'cbt' 
                       & clinical_data$session == 'baseline')
CBT_baseline = read.csv("clean_data/CBT_baseline.csv")

# format ID_short in clinical data
clinical_temp['subjID'] <- sub("C", "C1", clinical_temp$ID_short)
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
clinical_CBT_baseline_scale <- clinical_CBT_baseline %>%
                  dplyr::mutate_at(c('AES_total', 'AUDIT_total', 'BIS_total', 'EAT_total',
                  'LSAS_total', 'OCI_total', 'SCZ_total','SDS_total', 
                  'STAI_total', 'QIDS_total', 'WSAS_total'), ~(scale(.) %>% as.vector))
write.csv(clinical_CBT_baseline_scale, 
          file = 'clean_data/clinical_CBT_baseline_scale.csv',
          row.names = FALSE)


############################# CBT_followup ####################
clinical_temp = subset(clinical_data, clinical_data$treatment == 'cbt' 
                       & clinical_data$session == 'followup')
CBT_followup = read.csv("clean_data/CBT_followup.csv")

# format ID_short in clinical data
clinical_temp['subjID'] <- sub("C", "C4", clinical_temp$ID_short)
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
clinical_CBT_followup_scale <- clinical_CBT_followup %>%
  dplyr::mutate_at(c('AES_total', 'AUDIT_total', 'BIS_total', 'EAT_total',
                     'LSAS_total', 'OCI_total', 'SCZ_total','SDS_total', 
                     'STAI_total', 'QIDS_total', 'WSAS_total'), ~(scale(.) %>% as.vector))
write.csv(clinical_CBT_followup_scale, 
          file = 'clean_data/clinical_CBT_followup_scale.csv',
          row.names = FALSE)


############################# SSRI_baseline ####################
clinical_temp = subset(clinical_data, clinical_data$treatment == 'ssri' 
                       & clinical_data$session == 'baseline')
CBT_baseline = read.csv("clean_data/CBT_baseline.csv")

# format ID_short in clinical data
clinical_temp['subjID'] <- sub("C", "C1", clinical_temp$ID_short)
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
clinical_CBT_baseline_scale <- clinical_CBT_baseline %>%
  dplyr::mutate_at(c('AES_total', 'AUDIT_total', 'BIS_total', 'EAT_total',
                     'LSAS_total', 'OCI_total', 'SCZ_total','SDS_total', 
                     'STAI_total', 'QIDS_total', 'WSAS_total'), ~(scale(.) %>% as.vector))
write.csv(clinical_CBT_baseline_scale, 
          file = 'clean_data/clinical_CBT_baseline_scale.csv',
          row.names = FALSE)















# SSRI_baseline_clinical = subset(clinical_data, clinical_data$treatment == 'ssri' 
#                                & clinical_data$session == 'baseline')

# SSRI_baseline_clinical$ID_short <- paste0("1", SSRI_baseline_clinical$ID_short)
# subset CBT_baseline1_stable and CBT_baseline1_volatile (subjects with clinical match)
CBT_baseline1_stable_match_clinical = subset(CBT_baseline1_stable, 
                                   CBT_baseline1_stable$subjID 
                                   %in% CBT_baseline_clinical$ID_short)
CBT_baseline1_volatile_match_clinical = subset(CBT_baseline1_volatile,
                                     CBT_baseline1_volatile$subjID 
                                     %in% CBT_baseline_clinical$ID_short)

write.csv(CBT_baseline1_stable_match_clinical, file = 'clean_data/CBT_baseline1_stable_match_clinical.csv',
          row.names = FALSE)
write.csv(CBT_baseline1_volatile_match_clinical, file = 'clean_data/CBT_baseline1_volatile_match_clinical.csv',
          row.names = FALSE)

# subset CBT_baseline2_stable and CBT_baseline2_volatile (subjects with clinical match)
CBT_baseline2_stable_match_clinical = subset(CBT_baseline2_stable, 
                                             CBT_baseline2_stable$subjID 
                                             %in% CBT_baseline_clinical$ID_short)
CBT_baseline2_volatile_match_clinical = subset(CBT_baseline2_volatile,
                                               CBT_baseline2_volatile$subjID 
                                               %in% CBT_baseline_clinical$ID_short)

write.csv(CBT_baseline2_stable_match_clinical, file = 'clean_data/CBT_baseline2_stable_match_clinical.csv',
          row.names = FALSE)
write.csv(CBT_baseline2_volatile_match_clinical, file = 'clean_data/CBT_baseline2_volatile_match_clinical.csv',
          row.names = FALSE)


SSRI_baseline1_stable_match_clinical = subset(SSRI_baseline1_stable, 
                                             SSRI_baseline1_stable$subjID 
                                             %in% SSRI_baseline_clinical$ID_short)
SSRI_baseline1_volatile_match_clinical = subset(SSRI_baseline1_volatile,
                                               SSRI_baseline1_volatile$subjID 
                                               %in% SSRI_baseline_clinical$ID_short)

write.csv(SSRI_baseline1_stable_match_clinical, file = 'clean_data/SSRI_baseline1_stable_match_clinical.csv',
          row.names = FALSE)
write.csv(SSRI_baseline1_volatile_match_clinical, file = 'clean_data/SSRI_baseline1_volatile_match_clinical.csv',
          row.names = FALSE)
SSRI_baseline2_stable_match_clinical = subset(SSRI_baseline2_stable, 
                                              SSRI_baseline2_stable$subjID 
                                              %in% SSRI_baseline_clinical$ID_short)
SSRI_baseline2_volatile_match_clinical = subset(SSRI_baseline2_volatile,
                                                SSRI_baseline2_volatile$subjID 
                                                %in% SSRI_baseline_clinical$ID_short)

write.csv(SSRI_baseline2_stable_match_clinical, file = 'clean_data/SSRI_baseline2_stable_match_clinical.csv',
          row.names = FALSE)
write.csv(SSRI_baseline2_volatile_match_clinical, file = 'clean_data/SSRI_baseline2_volatile_match_clinical.csv',
          row.names = FALSE)


######################################################################################
# subset CBT_baseline_clinical (CBT_baseline subjects who did schedule1)  
CBT_baseline1_clinical = subset(CBT_baseline_clinical, CBT_baseline_clinical$ID_short
                               %in% unique(CBT_baseline1_stable_match_clinical$subjID))
write.csv(CBT_baseline1_clinical, file = 'clean_data/CBT_baseline1_clinical.csv', row.names = FALSE)
# subset CBT_baseline_clinical (CBT_baseline subjects who did schedule2)  
CBT_baseline2_clinical = subset(CBT_baseline_clinical, CBT_baseline_clinical$ID_short
                                %in% unique(CBT_baseline2_match_clinical$subjID))
write.csv(CBT_baseline2_clinical, file = 'clean_data/CBT_baseline2_clinical.csv', row.names = FALSE)


# subset CBT_baseline1 and CBT_baseline2 (subjects with clinical match)
CBT_baseline1_match_clinical = subset(CBT_baseline1, 
                                      CBT_baseline1$subjID 
                                      %in% CBT_baseline_clinical$ID_short)
CBT_baseline2_match_clinical = subset(CBT_baseline2,
                                      CBT_baseline2$subjID 
                                      %in% CBT_baseline_clinical$ID_short)

write.csv(CBT_baseline1_match_clinical, file = 'clean_data/CBT_baseline1_match_clinical.csv',
          row.names = FALSE)
write.csv(CBT_baseline2_match_clinical, file = 'clean_data/CBT_baseline2_match_clinical.csv',
          row.names = FALSE)


