# This script is used to clean and wrap up the original dataset
# Created two folders "schedule1" and "schedule2" in CBT/baseline, 
# CBT/followup, SSRI/baseline, SSRI/followup to obtain the cleaned files
# The outputs of this script are the csv files in the clean_data folder, which 
# wrapped all subjects together
library(readr)
library(tibble)
library(dplyr)
library(gtools)
source("utils/processer_utils.R")
original_data_path = '~/Documents/Rstudio/volatility_composit/all_data/'
####### CBT Baseline (csv files in this folder are in the same format) ######
#################### Don't need to run this block every time ############
setwd("~/Documents/Rstudio/volatility_composit/all_data/CBT/Baseline")
files = list.files(pattern="*.csv", full.names = FALSE)
file_lists = sapply(files, read.csv, simplify = FALSE, skip=1)
## cluster schedule1 and schedule2
cluster_schedule(files, file_lists)

#########################################################################
## extract data from all csv files and put them into one (schedule1 and schedule2 separately)
setwd("~/Documents/Rstudio/volatility_composit/all_data/CBT/Baseline/schedule1")
wrap_up(schedule_path="schedule1", 
        save_path="~/Documents/Rstudio/volatility_composit/clean_data/CBT_baseline_schedule1", 
        ID_length = 6,
        original_data_path)

setwd("~/Documents/Rstudio/volatility_composit/all_data/CBT/Baseline/schedule2")
wrap_up(schedule_path="schedule2", 
        save_path="~/Documents/Rstudio/volatility_composit/clean_data/CBT_baseline_schedule2", 
        ID_length = 6,
        original_data_path)

## combine schedule1 data and schedule2 data
setwd("~/Documents/Rstudio/volatility_composit")
combine_schedule("clean_data/CBT_baseline_schedule1", 
                 "clean_data/CBT_baseline_schedule2", 
                 "clean_data/CBT_baseline")
CBT_baseline = read.csv("clean_data/CBT_baseline.csv")


####### CBT Followup (an extra column in some of the csv files) ######
#################### Don't need to run this block every time ############
setwd("~/Documents/Rstudio/volatility_composit/all_data/CBT/FollowUp")
files = list.files(pattern="*.csv", full.names = FALSE)
NAMES = read.table(files[1], nrow = 1, skip = 1, sep = ",")
file_lists = list()
for(i in 1:length(files)){
  if(ncol(read.table(files[i], skip = 2, sep = ","))==15){
    file_lists[[i]] <- read.table(files[i], skip = 2,  sep = ",")[,2:15]
    names(file_lists[[i]]) <- NAMES
  }else{
    file_lists[[i]] <- read.table(files[i], skip = 2,  sep = ",")
    names(file_lists[[i]]) <- NAMES
  }
}

## cluster schedule1 and schedule2
cluster_schedule(files, file_lists)
## wrap up stable and volatile in schedule1 and schedule2
setwd("~/Documents/Rstudio/volatility_composit/all_data/CBT/FollowUp/schedule1")
wrap_up(schedule_path="schedule1", 
        save_path="~/Documents/Rstudio/volatility_composit/clean_data/CBT_followup_schedule1", 
        ID_length = 6,
        original_data_path)
setwd("~/Documents/Rstudio/volatility_composit/all_data/CBT/FollowUp/schedule2")
wrap_up(schedule_path="schedule2", 
        save_path="~/Documents/Rstudio/volatility_composit/clean_data/CBT_followup_schedule2", 
        ID_length = 6,
        original_data_path)

## combine schedule1 data and schedule2 data
setwd("~/Documents/Rstudio/volatility_composit")
combine_schedule("clean_data/CBT_followup_schedule1", 
                 "clean_data/CBT_followup_schedule2", 
                 "clean_data/CBT_followup")
CBT_followup = read.csv("clean_data/CBT_followup.csv")



####### SSRI Baseline (some subjects have two extra rows in the beginning) ######
setwd("~/Documents/Rstudio/volatility_composit/all_data/SSRI/Baseline")
# files = exclude_subjects('SSRI/excluded_subs_SSRI_BL', 'SSRI', original_data_path) # 115->107
files = list.files(pattern="*.csv", full.names = FALSE)
file_lists = list()
# decide to read skip 1 or 2 rows
for(i in 1:length(files)){
  skip_count = 0
  while(read.table(files[i], nrow = 1, skip = skip_count, sep = ",")[[1]] != 'num_trial'){
    skip_count = skip_count + 1
  }
  file_lists[[i]] <- read.table(files[i], skip = skip_count, sep = ",", header = TRUE)
}

cluster_schedule(files, file_lists)
## wrap up stable and volatile in schedule1 and schedule2
setwd("~/Documents/Rstudio/volatility_composit/all_data/SSRI/Baseline/schedule1")
wrap_up(schedule_path="schedule1", 
        save_path="~/Documents/Rstudio/volatility_composit/clean_data/SSRI_baseline_schedule1", 
        ID_length = 5,
        original_data_path)
setwd("~/Documents/Rstudio/volatility_composit/all_data/SSRI/Baseline/schedule2")
wrap_up(schedule_path="schedule2", 
        save_path="~/Documents/Rstudio/volatility_composit/clean_data/SSRI_baseline_schedule2", 
        ID_length = 5,
        original_data_path)

## combine schedule1 data and schedule2 data
setwd("~/Documents/Rstudio/volatility_composit")
combine_schedule("clean_data/SSRI_baseline_schedule1", 
                 "clean_data/SSRI_baseline_schedule2", 
                 "clean_data/SSRI_baseline")
SSRI_baseline = read.csv("clean_data/SSRI_baseline.csv")


####### SSRI FollowUp (an extra column in some of the csv files) #####
setwd("~/Documents/Rstudio/volatility_composit/all_data/SSRI/FollowUp")
# files = exclude_subjects('SSRI/excluded_subs_SSRI_FU', 'SSRI', original_data_path) #96->90
files = list.files(pattern="*.csv", full.names = FALSE)
NAMES = read.table(files[1], nrow = 1, skip = 1, sep = ",")
file_lists = list()
for(i in 1:length(files)){
  if(ncol(read.table(files[i], skip = 2, sep = ","))==15){
    file_lists[[i]] <- read.table(files[i], skip = 2,  sep = ",")[,2:15]
    names(file_lists[[i]]) <- NAMES
  }else{
    file_lists[[i]] <- read.table(files[i], skip = 2,  sep = ",")
    names(file_lists[[i]]) <- NAMES
  }
}
cluster_schedule(files, file_lists)
## wrap up stable and volatile in schedule1 and schedule2
setwd("~/Documents/Rstudio/volatility_composit/all_data/SSRI/FollowUp/schedule1")
wrap_up(schedule_path="schedule1", 
        save_path="~/Documents/Rstudio/volatility_composit/clean_data/SSRI_followup_schedule1", 
        ID_length = 5,
        original_data_path)
setwd("~/Documents/Rstudio/volatility_composit/all_data/SSRI/FollowUp/schedule2")
wrap_up(schedule_path="schedule2", 
        save_path="~/Documents/Rstudio/volatility_composit/clean_data/SSRI_followup_schedule2",  
        ID_length = 5,
        original_data_path)

## combine schedule1 and schedule2
setwd("~/Documents/Rstudio/volatility_composit")
combine_schedule("clean_data/SSRI_followup_schedule1", 
                 "clean_data/SSRI_followup_schedule2", 
                 "clean_data/SSRI_followup")
SSRI_followup = read.csv("clean_data/SSRI_followup.csv")


