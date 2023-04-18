## function used to exclude low-effort subjects
exclude_subjects <- function(exclude_file, group, original_data_path){
  exclude = read.csv(paste0(original_data_path, exclude_file, '.csv'))
  names(exclude) = c('num', 'subj')
  files = list.files(pattern="*.csv", full.names = FALSE)
  files_df = data.frame(files)
  # exclude low-effort subjects
  if(group == 'CBT'){
    files = subset(files, !(substr(files_df$files, 1, 6) %in% exclude$subj)) 
  }else{
    files = subset(files, !(substr(files_df$files, 1, 5) %in% exclude$subj)) 
  }
  return(files)
}
## function used to cluster schedule1 and schedule2 files into different folders
cluster_schedule <- function(files, file_lists){
  # create directories for the new folders
  schedule1 <- "schedule1"
  schedule2 <- "schedule2"
  # dir.create(schedule2, recursive = TRUE)
  # dir.create(schedule1, recursive = TRUE)
  if(!file.exists(schedule1)) {  # If the folder does not exist, create a new one
    # make.dir(dirname(schedule1))
    dir.create(schedule1)
  } else {   # If it existed, delete and replace with a new one  
    unlink(schedule1, recursive = TRUE)
    dir.create(schedule1)
  }
  if(!file.exists(schedule2)) {  # If the folder does not exist, create a new one
    # make.dir(dirname(schedule2))
    dir.create(schedule2)
  } else {   # If it existed, delete and replace with a new one  
    unlink(schedule2, recursive = TRUE)
    dir.create(schedule2)
  }
  # Group the files according to schedules
  for (i in 1:length(file_lists)) {
    if (file_lists[[i]][1,'amount_blue'] == 10){  # found the stealing amount for the two schedules are fixed and different 
      #i.e. if the 'amount-blue' column of one subject begins with 10, the subject was given schedule2
      # save the clean data directly
      write.csv(file_lists[[i]], file = paste0("schedule2/", files[i]), row.names = FALSE)
    }else{
      # file.copy(from=files[i], to=schedule1)   # otherwise they were given schedule1
      write.csv(file_lists[[i]], file = paste0("schedule1/", files[i]), row.names = FALSE)
    }
  }
}

## function used to put all data files together (schedule1 and schedule2 separately)
wrap_up <- function(schedule_path, save_path, ID_length, original_data_path){
  files = list.files(pattern="*.csv", full.names = FALSE)
  schedulefiles = sapply(files, read.csv, simplify = FALSE) 
  schedule = read.csv(paste0(original_data_path, "schedule/", schedule_path, ".csv"))
  # the first 70 trials
  dat_list <- list()
  for (i in 1:length(schedulefiles)) {
    dat1 <- data.frame(subjID = rep(substr(files[i], start=1, stop=ID_length), 70), 
                       choice = schedulefiles[[i]][1:70, 'chosen_color'],
                       stringsAsFactors=FALSE,
                       trial = c(1:70),
                       outcome_blue = schedule[1:70, 'outcome'], #The schedule outcome where 1 represents blue stealed 0 represents blue didn't steal
                       outcome = schedulefiles[[i]][1:70, 'outcome'], # The real outcome where 1 represents loss pearls 0 represents no loss
                       amount_blue = schedulefiles[[i]][1:70, 'amount_blue'],
                       amount_orange = schedulefiles[[i]][1:70, 'amount_orange'])
    
    dat2 <- data.frame(subjID = rep(substr(files[i], start=1, stop=ID_length), 70), 
                       choice = schedulefiles[[i]][71:140, 'chosen_color'],
                       stringsAsFactors=FALSE,
                       trial = c(71:140),
                       outcome_blue = schedule[71:140, 'outcome'], #The schedule outcome where 1 represents blue stealed 0 represents blue didn't steal
                       outcome = schedulefiles[[i]][71:140, 'outcome'], # The real outcome where 1 represents loss pearls 0 represents no loss
                       amount_blue = schedulefiles[[i]][71:140, 'amount_blue'],
                       amount_orange = schedulefiles[[i]][71:140, 'amount_orange'])
    
    if(schedule_path == "schedule1"){
      dat1['status'] = -1 ## stable or volatile block
      dat2['status'] = 1
    }else{
      dat1['status'] = 1
      dat2['status'] = -1
    }
    dat = rbind(dat1, dat2)
    dat$choice <- as.numeric(factor(dat$choice, levels = c("blue", "orange")))
    dat_list[[i]] <- dat
  }
  dat_df <- dplyr::bind_rows(dat_list)
  write.csv(dat_df, paste0("~/Documents/rstudio/volatility1/clean_data/", save_path, ".csv"), row.names = FALSE)
}

## function used to combine schedule1 and schedule2
combine_schedule <- function(schedule1_file, schedule2_file, save_name){
  schedule1 = read.csv(paste0(schedule1_file, ".csv"))
  schedule2 = read.csv(paste0(schedule2_file, ".csv"))
  combine = rbind(schedule1, schedule2)
  combine <- combine %>% mutate(magLoss = dplyr::case_when(outcome == 0 ~ 0,
                                                          (outcome == 1) & (choice == 1) ~ as.numeric(amount_blue),
                                                          (outcome == 1) & (choice == 2) ~ as.numeric(amount_orange)),
                                choiceB = dplyr::if_else(choice == 1, 1, 0),
                                out = dplyr::if_else(outcome == 0, 1, -1),
                                outcome_blue1 = dplyr::if_else(outcome_blue == 1, 0, 1))
  write.csv(combine, file = paste0(save_name, ".csv"), row.names = FALSE)
}








wrap_up_mike <- function(schedule, stable_path, volatile_path){
  files = list.files(pattern="*.csv", full.names = FALSE)
  files = mixedsort(sort(files)) # order the files
  schedulefiles = sapply(files, read.csv, simplify = FALSE) 
  # the first 90 trials
  schedule_list1 <- list()
  # green stands for circle, which is the reference choice;
  # choice_color.1.green: subject's choice 1 green, 0 blue
  # information.1.green: whether the green choice generates shock 1yes 0no
  # outcome: whether the suject receives shock 1yes 0no
  # green_shock: the magnitude of green shock
  # blue_shock: the magnitude of blue shock
  for (i in 1:length(schedulefiles)) {
    dat1 <- data.frame(subjID = rep(substr(files[i], start=1, stop=11), 90), 
                       choice = schedulefiles[[i]][1:90, 'choice_colour.1.green.'],
                       trial = c(1:90),
                       outcome_blue = schedulefiles[[i]][1:90, 'information.1.green.'], 
                       outcome = schedulefiles[[i]][1:90, 'outcome'], 
                       amount_blue = schedulefiles[[i]][1:90, 'green_shock'],
                       amount_orange = schedulefiles[[i]][1:90, 'blue_shock'])
    dat1$choice <- factor(dat1$choice, levels = c(0,1), labels = c(2,1))
    schedule_list1[[i]] <- dat1                
  }
  # the last 90 trials
  schedule_list2 <- list()
  for (i in 1:length(schedulefiles)) {
    dat2 <- data.frame(subjID = rep(substr(files[i], start=1, stop=11), 90), 
                       choice = schedulefiles[[i]][91:180, 'choice_colour.1.green.'],
                       trial = c(91:180),
                       outcome_blue = schedulefiles[[i]][91:180, 'information.1.green.'], #The schedule outcome where 1 represents blue stealed 0 represents blue didn't steal
                       outcome = schedulefiles[[i]][91:180, 'outcome'], # The real outcome where 1 represents loss pearls 0 represents no loss
                       amount_blue = schedulefiles[[i]][91:180, 'green_shock'],
                       amount_orange = schedulefiles[[i]][91:180, 'blue_shock'])
    dat2$choice <- factor(dat2$choice, levels = c(0,1), labels = c(2,1))
    schedule_list2[[i]] <- dat2                
  }
  if(schedule == "schedule1"){
    schedule1_stable <- dplyr::bind_rows(schedule_list1)
    write.csv(schedule1_stable, paste0("~/Documents/rstudio/volatility/clean_data/", stable_path, ".csv"), row.names = FALSE)
    schedule1_volatile <- dplyr::bind_rows(schedule_list2)
    write.csv(schedule1_volatile, paste0("~/Documents/rstudio/volatility/clean_data/", volatile_path, ".csv"), row.names = FALSE)
  }else{
    schedule2_stable <- dplyr::bind_rows(schedule_list2)
    write.csv(schedule2_stable, paste0("~/Documents/rstudio/volatility/clean_data/", stable_path, ".csv"), row.names = FALSE)
    schedule2_volatile <- dplyr::bind_rows(schedule_list1)
    write.csv(schedule2_volatile, paste0("~/Documents/rstudio/volatility/clean_data/", volatile_path, ".csv"), row.names = FALSE)
  }
}
