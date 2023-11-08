library(cowplot)
library(ggplot2)
control_baseline_indpar = read.csv("fitted_data/chris_model2_composit/control_baseline_indpar.csv")




plot_individual_change <- function(df, name){
  
  A = c(df$baseline_A_stable, df$followup_A_stable,
        df$baseline_A_volatile, df$followup_A_volatile)
  beta = c(df$baseline_beta_stable, df$followup_beta_stable,
           df$baseline_beta_volatile, df$followup_beta_volatile)
  gamma = c(df$baseline_gamma_stable, df$followup_gamma_stable,
            df$baseline_gamma_volatile, df$followup_gamma_volatile)
  df = data.frame(A = A,
                  beta = beta,
                  gamma = gamma,
                  condition = rep(c('stable', 'volatile'), 
                                  each = length(df$baseline_A_stable)+length(df$followup_A_stable)),
                  combine = rep(c('Bstable', 'Fstable', 'Bvolatile', 'Fvolatile'), 
                                time = c(length(df$baseline_A_stable),
                                         length(df$followup_A_stable),
                                         length(df$baseline_A_volatile),
                                         length(df$followup_A_volatile))),
                  paired = rep(1:(length(df$baseline_A_stable)+length(df$followup_A_stable)), time = 2))
  df$combine = factor(df$combine, levels = c('Bstable', 'Bvolatile', 'Fvolatile', 'Fstable'))
  
  p1 <- ggplot(df,aes(x=combine,y=A,fill=condition,col=condition))+
    geom_point(aes(color=condition),size=2) +
    geom_line(aes(group = paired),color="grey") +
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    ylab(expression(A))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())+
    scale_y_log10()
  p2 <- ggplot(df,aes(x=combine,y=beta,fill=condition,col=condition))+
    geom_point(aes(color=condition),size=2) +
    geom_line(aes(group = paired),color="grey") +
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    ylab(expression(beta))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  p3 <- ggplot(df,aes(x=combine,y=gamma,fill=condition,col=condition))+
    geom_point(aes(color=condition),size=2) +
    geom_line(aes(group = paired),color="grey") +
    scale_fill_brewer(palette = "Dark2")+
    scale_colour_brewer(palette = "Dark2")+
    guides(fill = FALSE, col = FALSE)+
    ylab(expression(gamma))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  all_plot <- plot_grid(p1, p2, p3, labels="AUTO")
  title <- ggdraw() + draw_label(name, fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  h_all = plot_grid(title, all_plot, ncol = 1, rel_heights = c(0.5, 5)) # rel_heights values control title margins
  # ggsave(filename=paste0("plots/individual/", name, "_change", ".pdf"), plot = h_all, width = 6, height = 5)
  return(h_all)
}

CBT_all = plot_individual_change(CBT_par, "CBT")
CBT_all
SSRI_all = plot_individual_change(SSRI_par, "SSRI")
SSRI_all
control_all = plot_individual_change(control_par, "control")
control_all

