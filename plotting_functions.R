

plot_OR <- function(file,p_val=TRUE){
  
  shift_vars <- rev(c(
   "No shift work"= "Day workers (referent)",
    "Never/rarely"="Shift work, but never or\nrarely night shifts",
    "Sometimes"="Irregular shift work\nincluding nights",
    "Always"="Permanent night shift\nwork"
  ))
  cbPalette <- c("red","black","red")
  pd_width <- 0.6
  y_lim <- c(.6,1.5)
  
  readxl::read_xlsx(file) -> plot_data
  
  clean_data <- plot_data %>%
    filter(vars=="formatted") %>%
    dplyr::select(-vars,-sex_interaction) %>%
    pivot_longer(cols = -c(model,Sex),
                 names_to="Shift work",
                 values_to="values") %>%
    separate(values,
             into=c("OR","LCI","UCI"),
             sep="[()\\– ]+",
             convert=TRUE) %>%
    mutate(OR=ifelse(`Shift work`=="No shift work",1,OR)) %>%
    mutate(`Shift work`=factor(recode(`Shift work`,!!!shift_vars),levels=unname(shift_vars),ordered=TRUE))
  
  
  plots<-list()
  
  for(i in unique(clean_data$model)){
    
    p_mod<- clean_data %>% 
      filter(model==i) %>%
      ggplot(aes(y=OR,x=`Shift work`,colour=Sex)) + 
      geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
      geom_errorbar(aes(ymax = UCI, ymin = LCI),
                    size = .5, width = .4,
                    position = position_dodge(width = pd_width)) +
      geom_point(position = position_dodge(width = pd_width)) + 
      theme_bw() +
      theme(axis.title.y = element_blank(),
            legend.position=c(0.85, 0.85),
            legend.background = element_blank())+
      ylab("Odds Ratio") +
      ylim(y_lim) +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
      scale_colour_manual(
        values = c(cbPalette[c(2,3)],"black"),
        name = element_blank(),
        guide = guide_legend(reverse=TRUE)
        )
    
    if (p_val) {
      pval <- plot_data %>%
        filter(model == i, !is.na(sex_interaction)) %>%
        pull(sex_interaction) %>%
        unique()
      
      pval_text <- ifelse(pval < 0.001, "p<0.001", paste0("p=", pval))
      y_pos <- (y_lim[1]+1)/2
      x_pos <- length(unique(clean_data$`Shift work`)) +.25
      
      p_mod <- p_mod +
        annotate("text",
                 x = x_pos,
                 y = y_pos,
                 label = pval_text) }
    
    plots[[as.character(i)]] <- p_mod
    
  }
  return(plots)
}
