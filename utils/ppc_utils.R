## posterior predictive check on trial, individual, and group levels in stable and volatile blocks
ppc_stable <- function(fit_extract, list_data, schedule, file_name){
  y_pred = fit_extract$y_pred   # y_pred --> 12000 (MCMC samples) x 33 (subjects) x 70 (trials)
  true_y = list_data$choiceB     # true_y --> 33(subjects) x 70(trials)

  if(schedule == 1){
    y_pred_mean_trial = apply(y_pred == 0, c(1,3), mean)  # average of all subjects
    y_pred_mean_sub = apply(y_pred == 0, c(1,2), mean)  # average of all trials
    true_mean_trial = colMeans(true_y == 0) # mean correct choice in each trial
    true_mean_sub = rowMeans(true_y == 0) # mean correct choice of each subject
  } else{
    y_pred_mean_trial = apply(y_pred == 1, c(1,3), mean)  # average of all subjects
    y_pred_mean_sub = apply(y_pred == 1, c(1,2), mean)  # average of all trials
    true_mean_trial = colMeans(true_y == 1) # mean correct choice in each trial
    true_mean_sub = rowMeans(true_y == 1) # mean correct choice of each subject
  }

  color_scheme_set("pink")
  p_trial = ppc_ribbon(true_mean_trial, y_pred_mean_trial, 
                       prob = 0.5,
                       prob_outer = 0.95,
                       alpha = 0.5,
                       size = 0.5) + ggplot2::xlab("Trial") + 
                                     ggplot2::ylab("Correct choices (%)") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p_sub = ppc_intervals(true_mean_sub, y_pred_mean_sub, x = true_mean_sub) + 
          abline_01() + 
          ggplot2::xlab("Data: correct choice(%)") + 
          ggplot2::ylab("Model: correct choice(%)") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p_all = ppc_stat(true_mean_sub, y_pred_mean_sub) + 
    ggplot2::xlab("Data: correct choice(%)") + 
    ggplot2::ylab("Model: correct choice(%)") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p_combine = ggarrange(p_trial, p_sub, p_all, ncol = 3, nrow = 1)
  p_combine = annotate_figure(p_combine, top = text_grob(file_name, face = "bold", size = 14))
  ggsave(paste0("plots/ppc/", file_name, ".pdf"), plot = p_combine, width = 15, height = 5)
  # for plotting in markdown file
  # return_list = list(p_trial = p_trial, p_sub=p_sub, p_all=p_all)
  # return(return_list)
}

ppc_volatile <- function(fit_extract, list_data, schedule, file_name, choice_code){
  y_pred = fit_extract$y_pred   # y_pred --> 12000 (MCMC samples) x 33 (subjects) x 70 (trials)
  true_y = list_data$choiceB     # true_y --> 33(subjects) x 70(trials)
  
  if(schedule == 1){
    y_pred_mean_trial1 = apply(y_pred[,, 1:20] == 1, c(1,3), mean)  # average of all subjects in trial 1:20
    y_pred_mean_trial2 = apply(y_pred[,, 21:40] == choice_code, c(1,3), mean)  # average of all subjects in trial 21:40
    y_pred_mean_trial3 = apply(y_pred[,, 41:60] == 1, c(1,3), mean)  # average of all subjects in trial 41:60
    y_pred_mean_trial4 = apply(y_pred[,, 61:70] == choice_code, c(1,3), mean)  # average of all subjects in trial 61:70
    
    y_pred_mean_trial = cbind(y_pred_mean_trial1, y_pred_mean_trial2, y_pred_mean_trial3, y_pred_mean_trial4)
    y_pred_sub1 = y_pred[,,1:20] == 1
    y_pred_sub2 = y_pred[,,21:40] == choice_code
    y_pred_sub3 = y_pred[,,41:60] == 1
    y_pred_sub4 = y_pred[,,61:70] == choice_code
    y_pred_sub = abind(y_pred_sub1, y_pred_sub2, y_pred_sub3, y_pred_sub4, along = 3)
    y_pred_mean_sub = apply(y_pred_sub, c(1,2), mean)
    
    true_mean_trial1 = colMeans(true_y[, 1:20] == 1) # mean correct choice in each trial
    true_mean_trial2 = colMeans(true_y[, 21:40] == 0) # mean correct choice in each trial
    true_mean_trial3 = colMeans(true_y[, 41:60] == 1) # mean correct choice in each trial
    true_mean_trial4 = colMeans(true_y[, 61:70] == 0) # mean correct choice in each trial
    true_mean_trial = c(true_mean_trial1, true_mean_trial2, true_mean_trial3, true_mean_trial4)
    true_sub1 = true_y[, 1:20] == 1
    true_sub2 = true_y[, 21:40] == 0
    true_sub3 = true_y[, 41:60] == 1
    true_sub4 = true_y[, 61:70] == 0
    true_sub = cbind(true_sub1, true_sub2, true_sub3, true_sub4)
    true_mean_sub = rowMeans(true_sub)
    
  }else{
    y_pred_mean_trial1 = apply(y_pred[,, 1:20] == choice_code, c(1,3), mean)  # average of all subjects in trial 21:40
    y_pred_mean_trial2 = apply(y_pred[,, 21:40] == 1, c(1,3), mean)  # average of all subjects in trial 41:60
    y_pred_mean_trial3 = apply(y_pred[,, 41:60] == choice_code, c(1,3), mean)  # average of all subjects in trial 61:70
    y_pred_mean_trial4 = apply(y_pred[,, 61:70] == 1, c(1,3), mean)  # average of all subjects in trial 61:70
    y_pred_mean_trial = cbind(y_pred_mean_trial1, y_pred_mean_trial2, y_pred_mean_trial3, y_pred_mean_trial4)
    y_pred_sub1 = y_pred[,,1:20]==choice_code
    y_pred_sub2 = y_pred[,,21:40]==1
    y_pred_sub3 = y_pred[,,41:60]==choice_code
    y_pred_sub4 = y_pred[,,61:70]==1
    y_pred_sub = abind(y_pred_sub1, y_pred_sub2, y_pred_sub3, y_pred_sub4, along = 3)
    y_pred_mean_sub = apply(y_pred_sub, c(1,2), mean)
    
    true_mean_trial1 = colMeans(true_y[, 1:20] == 0) # mean correct choice in each trial
    true_mean_trial2 = colMeans(true_y[, 21:40] == 1) # mean correct choice in each trial
    true_mean_trial3 = colMeans(true_y[, 41:60] == 0) # mean correct choice in each trial
    true_mean_trial4 = colMeans(true_y[, 61:70] == 1) # mean correct choice in each trial
    true_mean_trial = c(true_mean_trial1, true_mean_trial2, true_mean_trial3, true_mean_trial4)
    true_sub1 = true_y[, 1:20] == 0
    true_sub2 = true_y[, 21:40] == 1
    true_sub3 = true_y[, 41:60] == 0
    true_sub4 = true_y[, 61:70] == 1
    true_sub = cbind(true_sub1, true_sub2, true_sub3, true_sub4)
    true_mean_sub = rowMeans(true_sub)
  }
    
  color_scheme_set("pink")
  p_trial = ppc_ribbon(true_mean_trial, y_pred_mean_trial ,prob = 0.5,
                       prob_outer = 0.95,
                       alpha = 0.5,
                       size = 0.5) + ggplot2::xlab("Trial") + 
                                     ggplot2::ylab("Correct choices (%)") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p_sub = ppc_intervals(true_mean_sub, y_pred_mean_sub, x = true_mean_sub) + 
          abline_01() + 
          ggplot2::xlab("Data: correct choice(%)") + 
          ggplot2::ylab("Model: correct choice(%)") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p_all = ppc_stat(true_mean_sub, y_pred_mean_sub) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p_combine = ggarrange(p_trial, p_sub, p_all, ncol = 3, nrow = 1)
  p_combine = annotate_figure(p_combine, top = text_grob(file_name, face = "bold", size = 14))
  ggsave(paste0("plots/ppc/", file_name, ".pdf"), plot = p_combine, width = 15, height = 5)
  # return_list = list(p_trial = p_trial, p_sub=p_sub, p_all=p_all)
  # return(return_list)
 }


## posterior predictive check on trial, individual, and group levels in stable and volatile blocks
ppc_composit_correct <- function(fit_extract, list_data, file_name){
  y_pred = fit_extract$y_pred   # y_pred --> 12000 (MCMC samples) x 33 (subjects) x 70 (trials)
  y_pred = replace(y_pred, y_pred == 2, 0)
  true_y = list_data$choiceB     # true_y --> 33(subjects) x 70(trials)
  
  y_pred_mean_trial1 = apply(y_pred[,,1:60] == 1, c(1,3), mean)  # average of all subjects
  y_pred_mean_trial2 = apply(y_pred[,,61:80] == 0, c(1,3), mean)  # average of all subjects
  y_pred_mean_trial3 = apply(y_pred[,,81:100] == 1, c(1,3), mean)  # average of all subjects
  y_pred_mean_trial4 = apply(y_pred[,,101:120] == 0, c(1,3), mean)  # average of all subjects
  y_pred_mean_trial = cbind(y_pred_mean_trial1, y_pred_mean_trial2, y_pred_mean_trial3, y_pred_mean_trial4)
  
  y_pred_mean_sub1 = apply(y_pred[,,1:60] == 1, c(1,2), mean)  # average of all trials
  y_pred_mean_sub2 = apply(y_pred[,,61:80] == 0, c(1,2), mean)  # average of all trials
  y_pred_mean_sub3 = apply(y_pred[,,81:100] == 1, c(1,2), mean)  # average of all trials
  y_pred_mean_sub4 = apply(y_pred[,,101:120] == 0, c(1,2), mean)  # average of all trials
  y_pred_sub = abind(y_pred_mean_sub1, y_pred_mean_sub2, y_pred_mean_sub3, y_pred_mean_sub4, along = 3)
  y_pred_mean_sub = apply(y_pred_sub, c(1,2), mean)
  
  true_mean_trial1 = colMeans(true_y[, 1:60] == 1) # mean correct choice in each trial
  true_mean_trial2 = colMeans(true_y[, 61:80] == 0) # mean correct choice in each trial
  true_mean_trial3 = colMeans(true_y[, 81:100] == 1) # mean correct choice in each trial
  true_mean_trial4 = colMeans(true_y[, 101:120] == 0) # mean correct choice in each trial
  true_mean_trial = c(true_mean_trial1, true_mean_trial2, true_mean_trial3, true_mean_trial4)
  true_sub1 = true_y[, 1:60] == 1
  true_sub2 = true_y[, 61:80] == 0
  true_sub3 = true_y[, 81:100] == 1
  true_sub4 = true_y[, 101:120] == 0
  true_sub = cbind(true_sub1, true_sub2, true_sub3, true_sub4)
  true_mean_sub = rowMeans(true_sub)
  
  color_scheme_set("pink")
  p_trial = ppc_ribbon(true_mean_trial, y_pred_mean_trial, 
                       prob = 0.5,
                       prob_outer = 0.95,
                       alpha = 0.5,
                       size = 0.5) + ggplot2::xlab("Trial") + ggplot2::ylab("Correct choices (%)")
  p_sub = ppc_intervals(true_mean_sub, y_pred_mean_sub, x = true_mean_sub) + 
    abline_01() + 
    ggplot2::xlab("Data: correct choice(%)") + 
    ggplot2::ylab("Model: correct choice(%)")
  y_pred_mean1 = y_pred[,,1:60] == 1
  y_pred_mean2 = y_pred[,,61:80] == 0
  y_pred_mean3 = y_pred[,,81:100] == 1
  y_pred_mean4 = y_pred[,,101:120] == 0
  y_pred_mean = as.matrix(apply(abind(y_pred_mean1, y_pred_mean2, y_pred_mean3, y_pred_mean4), c(1), mean))
  true_mean1 = true_y[, 1:60] == 1
  true_mean2 = true_y[, 61:80] == 0
  true_mean3 = true_y[, 81:100] == 1
  true_mean4 = true_y[, 101:120] == 0
  true_mean = mean(cbind(true_mean1, true_mean2, true_mean3, true_mean4))
  p_all = ppc_stat(true_mean, y_pred_mean)
  p_combine = ggarrange(p_trial, p_sub, p_all, ncol = 3, nrow = 1, labels = c("(a)", "(b)", "(c)"))
  p_combine = annotate_figure(p_combine, top = text_grob(file_name, face = "bold", size = 14))
  ggsave(paste0("plots/ppc/", file_name, ".png"), plot = p_combine, width = 18, height = 5)
  # for plotting in markdown file
  # return_list = list(p_trial = p_trial, p_sub=p_sub, p_all=p_all)
  # return(return_list)
}


ppc_composit_loss_shift <- function(fit_extract, list_data, file_name){
  y_pred = fit_extract$y_pred   # y_pred --> 12000 (MCMC samples) x 33 (subjects) x 70 (trials)
  
  pred_last_choice1 = y_pred[,,1:dim(y_pred)[3]-1]
  pred_last_choice2 = array(NA, c(dim(y_pred)[1], dim(y_pred)[2], 1))
  pred_last_choice = abind(pred_last_choice2, pred_last_choice1, along = 3)
  pred_shift = pred_last_choice != y_pred
  
  y_pred1 = replace(y_pred, y_pred == 2, 0)
  outcome_blue = apply(list_data$outcome_blue, 2, rep, dim(y_pred)[1]) 
  outcome_blue = array(outcome_blue, c(dim(y_pred)[1], dim(y_pred)[2], dim(y_pred)[3]))
  outcome = y_pred1 == outcome_blue
  pred_loss1 = outcome[,,1:dim(y_pred)[3]-1]
  pred_loss2 = array(NA, c(dim(y_pred)[1], dim(y_pred)[2], 1))
  pred_loss = abind(pred_loss2, pred_loss1, along = 3)
  
  pred_loss_shift_trial = apply((pred_shift==1 & pred_loss==1)[,,2:dim(y_pred)[3]], c(1,3), sum)
  pred_loss_stay_trial = apply((pred_shift==0 & pred_loss==1)[,,2:dim(y_pred)[3]], c(1,3), sum)
  pred_loss_shift_percen_trial = pred_loss_shift_trial/(pred_loss_shift_trial+pred_loss_stay_trial)
  pred_loss_shift_percen_trial[is.na(pred_loss_shift_percen_trial)]=0
  
  pred_loss_shift_subj = apply((pred_shift==1 & pred_loss==1)[,,2:dim(y_pred)[3]], c(1,2), sum)
  pred_loss_stay_subj = apply((pred_shift==0 & pred_loss==1)[,,2:dim(y_pred)[3]], c(1,2), sum)
  pred_loss_shift_percen_subj = pred_loss_shift_subj/(pred_loss_shift_subj+pred_loss_stay_subj)
  pred_loss_shift_percen_subj[is.na(pred_loss_shift_percen_subj)]=0
  
  
  true_choice = list_data$choiceB
  true_outcome = list_data$outcome
  true_last_choice = cbind(rep(NA, nrow(true_choice)), true_choice[, 1:ncol(true_choice)-1])
  true_shift = true_last_choice != true_choice
  true_loss = cbind(rep(NA, nrow(true_choice)), true_outcome[, 1:ncol(true_outcome)-1])
  true_loss_shift_trial = colSums((true_shift==1 & true_loss==1)[,2:ncol(true_choice)])
  true_loss_stay_trial = colSums((true_shift==0 & true_loss==1)[,2:ncol(true_choice)])
  true_loss_shift_percen_trial = true_loss_shift_trial/(true_loss_shift_trial+true_loss_stay_trial)
  
  true_loss_shift_subj = rowSums((true_shift==1 & true_loss==1)[,2:ncol(true_choice)])
  true_loss_stay_subj = rowSums((true_shift==0 & true_loss==1)[,2:ncol(true_choice)])
  true_loss_shift_percen_subj = true_loss_shift_subj/(true_loss_shift_subj+true_loss_stay_subj)

  color_scheme_set("pink")
  p_trial = ppc_ribbon(true_loss_shift_percen_trial,  pred_loss_shift_percen_trial, 
                       prob = 0.5,
                       prob_outer = 0.95,
                       alpha = 0.5,
                       size = 0.5) + ggplot2::xlab("Trial") + ggplot2::ylab("Correct choices (%)")
  p_sub = ppc_intervals(true_loss_shift_percen_subj,  pred_loss_shift_percen_subj, x = true_loss_shift_percen_subj) + 
    abline_01() + 
    ggplot2::xlab("Data: loss-shift(%)") + 
    ggplot2::ylab("Model: loss-shift(%)")
  
  pred_loss_shift = apply((pred_shift==1 & pred_loss==1)[,,2:dim(y_pred)[3]], c(1), sum)
  pred_loss_stay = apply((pred_shift==0 & pred_loss==1)[,,2:dim(y_pred)[3]], c(1), sum)
  pred_loss_shift_percen = as.matrix(pred_loss_shift/(pred_loss_shift+pred_loss_stay))
  pred_loss_shift_percen[is.na(pred_loss_shift_percen)]=0
  
  true_loss_shift = sum((true_shift==1 & true_loss==1)[,2:ncol(true_choice)])
  true_loss_stay = sum((true_shift==0 & true_loss==1)[,2:ncol(true_choice)])
  true_loss_shift_percen = true_loss_shift/(true_loss_shift+true_loss_stay)
  p_all = ppc_stat(true_loss_shift_percen, pred_loss_shift_percen)
  # p_all = ppc_stat(true_loss_shift_percen_subj, pred_loss_shift_percen_subj)
  p_combine = ggarrange(p_trial, p_sub, p_all, ncol = 3, nrow = 1)
  p_combine = annotate_figure(p_combine, top = text_grob(file_name, face = "bold", size = 14))
  ggsave(paste0("plots/ppc/", file_name, ".pdf"), plot = p_combine, width = 15, height = 5)
  # for plotting in markdown file
  # return_list = list(p_trial = p_trial, p_sub=p_sub, p_all=p_all)
  # return(return_list)
}


ppc_correct_correlation <- function(fit_extract, list_data){
  y_pred = fit_extract$y_pred   # y_pred --> 12000 (MCMC samples) x 33 (subjects) x 70 (trials)
  y_pred = replace(y_pred, y_pred == 2, 0)
  true_y = list_data$choiceB     # true_y --> 33(subjects) x 70(trials)
  
  y_pred_mean_trial1 = apply(y_pred[,,1:60] == 1, c(1,3), mean)  # average of all subjects
  y_pred_mean_trial2 = apply(y_pred[,,61:80] == 0, c(1,3), mean)  # average of all subjects
  y_pred_mean_trial3 = apply(y_pred[,,81:100] == 1, c(1,3), mean)  # average of all subjects
  y_pred_mean_trial4 = apply(y_pred[,,101:120] == 0, c(1,3), mean)  # average of all subjects
  y_pred_mean_trial = cbind(y_pred_mean_trial1, y_pred_mean_trial2, y_pred_mean_trial3, y_pred_mean_trial4)
  
  y_pred_mean_sub1 = apply(y_pred[,,1:60] == 1, c(1,2), mean)  # average of all trials
  y_pred_mean_sub2 = apply(y_pred[,,61:80] == 0, c(1,2), mean)  # average of all trials
  y_pred_mean_sub3 = apply(y_pred[,,81:100] == 1, c(1,2), mean)  # average of all trials
  y_pred_mean_sub4 = apply(y_pred[,,101:120] == 0, c(1,2), mean)  # average of all trials
  y_pred_sub = abind(y_pred_mean_sub1, y_pred_mean_sub2, y_pred_mean_sub3, y_pred_mean_sub4, along = 3)
  y_pred_mean_sub = apply(y_pred_sub, c(1,2), mean)
  
  true_mean_trial1 = colMeans(true_y[, 1:60] == 1) # mean correct choice in each trial
  true_mean_trial2 = colMeans(true_y[, 61:80] == 0) # mean correct choice in each trial
  true_mean_trial3 = colMeans(true_y[, 81:100] == 1) # mean correct choice in each trial
  true_mean_trial4 = colMeans(true_y[, 101:120] == 0) # mean correct choice in each trial
  true_mean_trial = c(true_mean_trial1, true_mean_trial2, true_mean_trial3, true_mean_trial4)
  true_sub1 = true_y[, 1:60] == 1
  true_sub2 = true_y[, 61:80] == 0
  true_sub3 = true_y[, 81:100] == 1
  true_sub4 = true_y[, 101:120] == 0
  true_sub = cbind(true_sub1, true_sub2, true_sub3, true_sub4)
  true_mean_sub = rowMeans(true_sub)
  
  y_pred_mean1 = y_pred[,,1:60] == 1
  y_pred_mean2 = y_pred[,,61:80] == 0
  y_pred_mean3 = y_pred[,,81:100] == 1
  y_pred_mean4 = y_pred[,,101:120] == 0
  y_pred_mean = as.matrix(apply(abind(y_pred_mean1, y_pred_mean2, y_pred_mean3, y_pred_mean4, along = 3), c(1), mean))
  true_mean1 = true_y[, 1:60] == 1
  true_mean2 = true_y[, 61:80] == 0
  true_mean3 = true_y[, 81:100] == 1
  true_mean4 = true_y[, 101:120] == 0
  true_mean = mean(cbind(true_mean1, true_mean2, true_mean3, true_mean4))

  cor_trial = cor.test(true_mean_trial, colMeans(y_pred_mean_trial))
  cor_sub = cor.test(true_mean_sub, colMeans(y_pred_mean_sub))
  cor_all = mean(y_pred_mean > true_mean)
  
  return(list(cor_trial, cor_sub, cor_all))
}

