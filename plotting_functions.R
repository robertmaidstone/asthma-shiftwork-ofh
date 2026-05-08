

plot_OR <- function(file,p_val=TRUE){
  
  shift_vars <- rev(c(
   "No shift work"= "Day workers (referent)",
    "Never/rarely"="Shift work, but never or\nrarely night shifts",
    "Sometimes"="Irregular shift work\nincluding nights",
    "Always"="Permanent night shift\nwork"
  ))
  cbPalette <- c("red","black")
  pd_width <- 0.6
  
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
             convert=TRUE,
             extra="drop",
             fill="right") %>%
    mutate(OR=ifelse(`Shift work`=="No shift work",1,OR)) %>%
    mutate(`Shift work`=factor(recode(`Shift work`,!!!shift_vars),levels=unname(shift_vars),ordered=TRUE))
  
  y_lim <- c(0.975*min(clean_data$LCI,na.rm=T),max(clean_data$UCI,na.rm=T)*1.025)
  
  plots<-list()
  
  for(i in unique(clean_data$model)){
    
    p_mod<- clean_data %>% 
      filter(model==i) %>%
      ggplot(aes(y=OR,x=`Shift work`,colour=Sex)) + 
      geom_hline(aes(yintercept = 1), linewidth = .25, linetype = "dashed") +
      geom_errorbar(aes(ymax = UCI, ymin = LCI),
                    linewidth = .5, width = .4,
                    position = position_dodge(width = pd_width)) +
      geom_point(position = position_dodge(width = pd_width)) + 
      theme_classic() +
      theme(axis.title.y = element_blank(),
            legend.position=c(0.85, 0.925),
            legend.background = element_blank())+
      ylab("Adjusted odds ratio\nof medicated asthma") +
      ylim(y_lim) +
      coord_flip() +
      scale_colour_manual(
        values = cbPalette,
        name = "",
        guide = guide_legend(reverse=TRUE)
      )
    
    if (p_val) {
      pval <- plot_data %>%
        filter(model == i, !is.na(sex_interaction)) %>%
        pull(sex_interaction) %>%
        unique()
      
      pval_text <- ifelse(pval < 0.0001, "p<0.0001", paste0("p=", pval))
      y_pos <- (1-y_lim[1])/4 +y_lim[1]
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



plot_OR_UKB_CT <- function(file,p_val=TRUE){
  
  shift_vars <- rev(c(
    "No shift work"= "Day workers (referent)",
    "Never/rarely"="Shift work, but never or\nrarely night shifts",
    "Sometimes"="Irregular shift work\nincluding nights",
    "Always"="Permanent night shift\nwork"
  ))
  cbPalette <- c("red","black","blue")
  pd_width <- 0.6
  
  readxl::read_xlsx(file) -> plot_data
  
  clean_data <- plot_data %>%
    dplyr::select(-vars,-sex_interaction) %>%
    pivot_longer(cols = -c(model,Sex),
                 names_to="Shift work",
                 values_to="values") %>%
    separate(values,
             into=c("OR","LCI","UCI"),
             sep="[()\\–\\- ]+",
             convert=TRUE,
             extra="drop",
             fill="right") %>%
    mutate(OR=ifelse(`Shift work`=="No shift work",1,OR)) %>%
    mutate(`Shift work`=factor(recode(`Shift work`,!!!shift_vars),levels=unname(shift_vars),ordered=TRUE))
  
  y_lim <- c(0.975*min(clean_data$LCI,na.rm=T),max(clean_data$UCI,na.rm=T)*1.025)
  
  plots<-list()
  
  for(i in unique(clean_data$Sex)){
    
    p_mod<- clean_data %>% 
      filter(Sex==i) %>%
      ggplot(aes(y=OR,x=`Shift work`)) + 
      geom_hline(aes(yintercept = 1), linewidth = .25, linetype = "dashed") +
      geom_errorbar(aes(ymax = UCI, ymin = LCI),
                    linewidth = .5, width = .4,
                    position = position_dodge(width = pd_width)) +
      geom_point(position = position_dodge(width = pd_width)) + 
      theme_classic() +
      theme(axis.title.y = element_blank(),
            legend.position=c(0.85, 0.925),
            legend.background = element_blank())+
      ylab("Adjusted odds ratio\nof moderate-severe asthma") +
      ylim(y_lim) +
      coord_flip() +
      scale_colour_manual(
        values = cbPalette,
        name = "",
        guide = guide_legend(reverse=TRUE)
      )
    
    plots[[as.character(i)]] <- p_mod
    
  }
  return(plots)
}

