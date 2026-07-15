

plot_OR <- function(file,p_val=TRUE,y_lim=NULL,p_val_loc=NULL,y_lab_text=NULL){
  
  shift_vars <- rev(c(
   "No shift work"= "Day workers (referent)",
    "Never/rarely"="Shift work, but never or\nrarely night shifts",
    "Sometimes"="Irregular shift work\nincluding nights",
    "Always"="Permanent night shift\nwork",
   "Shift work"="Shift workers"
  ))
  cbPalette <- c("black","red")
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
  
  if(is.null(y_lim)){
    y_lim <- c(0.975*min(clean_data$LCI,na.rm=T),max(clean_data$UCI,na.rm=T)*1.025)
  }
  
  if(is.null(y_lab_text)){
  y_lab_text<-"Adjusted odds ratio\nof medicated asthma"
  }
  
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
      ylab(y_lab_text) +
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
      
      pval_text <- ifelse(pval < 0.01, "p<0.01", paste0("p=", round(pval,2)))
      if(is.null(p_val_loc)){
        y_pos <- (1-y_lim[1])/4 +y_lim[1]
        x_pos <- length(unique(clean_data$`Shift work`)) +.25
      }else{
        y_pos <- p_val_loc[2]
        x_pos <- p_val_loc[1]
      }
      
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

plot_ORb <- function(file,p_val=TRUE){
  
  shift_vars <- rev(c(
    "No shift work"= "Day workers (referent)",
    "Never/rarely"="Shift work, but never or\nrarely night shifts",
    "Sometimes"="Irregular shift work\nincluding nights",
    
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


make_shiftwork_table <- function(filename,
                                 model_labels = c(
                                   "1" = "Model 1: Age and\nethnicity-adjusted",
                                   "2" = "Model 2: Multivariable-\nadjusted",
                                   "3" = "Model 3: Model 2\ncovariates +\npotential\nmoderators"
                                 ),
                                 SWb=FALSE) {
  
  SW_cats_in <- c(
    "no_shift_work",
    "never_rarely",
    "sometimes",
    "always"
  )
  
  SW_cats_out <-  c(
    "Day workers",
    "Shift work, but never/rarely nights",
    "Irregular shift work including nights",
    "Permanent night shift work"
  )
  if(SWb==TRUE){
    SW_cats_in <- c(
      "no_shift_work",
      "shift_work"
    )
    
    SW_cats_out <-  c(
      "Day workers",
      "Shift workers"
    )
  }
  
  
  # -----------------------------
  # 1. Read + clean
  # -----------------------------
  df <- read_excel(filename, sheet = 1)
  df <- janitor::clean_names(df)
  
  df
  # -----------------------------
  # 2. Long → tidy
  # -----------------------------
  tidy <- df %>%
    pivot_longer(
      cols = all_of(SW_cats_in),
      names_to = "shift_category",
      values_to = "value"
    )
  
  # -----------------------------
  # 3. Wide table by model/sex
  # -----------------------------
  names(SW_cats_out) <- SW_cats_in
  
  supp_table <- tidy %>%
    mutate(
      shift_category = recode(shift_category, !!!SW_cats_out)
    ) %>%
    pivot_wider(
      id_cols = c(model, sex, vars),
      names_from = shift_category,
      values_from = value
    ) %>%
    arrange(model, sex, vars)
  
  # -----------------------------
  # 4. Combine Female + Male rows
  # -----------------------------
  model_int <- supp_table$model[1]
  
  supp_table <- supp_table %>%
    mutate(
      `Day workers` = ifelse(
        vars %in% c("cases_display", "N_total"),
        `Day workers`,
        paste(sex, "referent")
      )
    ) %>%
    group_by(model, vars) %>%
    summarise(
      across(
        all_of(SW_cats_out),
        ~ paste(.x, collapse = "\n")
      ),
      .groups = "drop"
    ) %>%
    filter(model == model_int | vars == "formatted")
  
  # -----------------------------
  # 5. Add sex‑interaction column
  # -----------------------------
  sex_int <- tidy %>%
    filter(!is.na(sex_interaction)) %>%
    select(model, sex_interaction) %>%
    distinct()
  
  supp_table_combined <- cbind(
    supp_table,
    "Sex-shift work interaction" = c("", "", sprintf("%.2f", sex_int$sex_interaction))
  ) %>%
    mutate(`Sex-shift work interaction`=ifelse(`Sex-shift work interaction`>=0.01| `Sex-shift work interaction`=="",`Sex-shift work interaction`,"p<0.01"))
  
  # -----------------------------
  # 6. Relabel rows + reorder
  # -----------------------------
  
  names(model_labels) <- paste0("formatted_",names(model_labels))
  
  supp_table_combined <- supp_table_combined %>%
    mutate(
      vars = case_when(
        vars == "N_total" ~ "Total sample size",
        vars == "cases_display" ~ "Total cases (% of\ntotal sample size)",
        vars == "formatted" ~ model_labels[paste0(vars, "_", model)]
      )
    ) %>%
    slice(2, 1, 3:n()) %>%
    select(-model)
  
  # -----------------------------
  # 7. Build flextable
  # -----------------------------
  ft <- flextable(supp_table_combined)
  
  # -----------------------------
  # 8. Apply two‑line colouring
  # -----------------------------
  for (j in 2:(length(SW_cats_out)+1)) {  # columns 2–5
    for (i in 1:nrow(ft$body$dataset)) {
      
      vals <- supp_table_combined[i, j] %>%
        str_split("\n") %>%
        unlist()
      
      ft <- mk_par(
        ft,
        i = i,
        j = j,
        value = as_paragraph(
          as_chunk(paste0(vals[1], "\n"), props = fp_text(color = "black")),
          as_chunk(vals[2], props = fp_text(color = "red"))
        )
      )
    }
  }
  
  ft <- align(
    ft,
    i = 1:nrow(ft$body$dataset),   # body rows only
    j = 2:(length(SW_cats_out)+1),                       # columns 2–5
    align = "center",
    part = "body"
  )
  n_tab <- tidy %>% filter(vars=="N_total",model==model_int) %>% dplyr::select(value) %>% unlist %>% as.numeric() %>% sum()
  ft <- add_header_lines(ft, values = paste(filename,"### n=",n_tab))
  ft
}
