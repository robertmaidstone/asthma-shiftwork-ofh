SD_table_function <- function(data, vars, by, rd = 1, missing = c("exclude", "include", "only")
) {
  missing <- match.arg(missing)
  # Tidy-eval for grouping variable
  by_var <- rlang::sym(by)
  
  # Filter out unwanted values only for the grouping variable
  data <- data %>%
    filter(!(!!by_var %in% c(NA, "Do not know", "Prefer not to answer")))
  
  n_table <- data %>%
    group_by(!!by_var) %>%
    summarise(n = n()) %>%
    mutate(variable = "n", level = "") %>%
    select(variable, level, everything()) %>%
    pivot_wider(names_from = !!by_var, values_from = n)
  
  # Helper: numeric summary
  summarise_numeric <- function(x) {
    tibble(
      level = "mean_sd",
      value = paste0(
        round(mean(x, na.rm = TRUE), rd),
        " (",
        round(sd(x, na.rm = TRUE), rd),
        ")"
      )
    )
  }
  
  # Helper: categorical summary
  summarise_categorical <- function(x) {
    if (is.logical(x)) {
      pct_true <- round(100 * sum(x, na.rm = TRUE) / sum(!is.na(x)), rd)
      return(tibble(level = "TRUE", value = as.character(pct_true)))
    }
    
    tab <- table(x, useNA = "no")
    pct <- round(100 * tab / sum(tab), rd)
    
    tibble(
      level = names(pct),
      value = as.character(pct)
    )
  }
  
  # Helper: missingness summary
  summarise_missing <- function(x) {
    tibble(
      level = "Missing (%)",
      value = as.character(round(100 * mean(is.na(x)), rd))
    )
  }
  
  # Loop over variables
  summaries <- lapply(vars, function(v) {
    x <- data[[v]]
    
    # Base summary (numeric or categorical)
    base_summary <- data %>%
      group_by(!!by_var) %>%
      summarise(tmp = list(
        if (is.numeric(x)) {
          summarise_numeric(.data[[v]])
        } else {
          summarise_categorical(.data[[v]])
        }
      )) %>%
      tidyr::unnest(tmp)
    
    # Add missingness if requested
    if (missing%in%c("include","only")) {
      miss_df <- data %>%
        group_by(!!by_var) %>%
        summarise(tmp = list(summarise_missing(.data[[v]]))) %>%
        tidyr::unnest(tmp)
      
      base_summary <- bind_rows(base_summary, miss_df)
    }
    
    # Combine counts + summaries
    out <- base_summary %>%
      mutate(variable = v) %>%
      select(variable, level, everything()) %>%
      pivot_wider(names_from = !!by_var, values_from = value)
    
    if (missing==c("only")) {
      out <- out %>% filter(level=="Missing (%)")
    }
    out
  })
  
  return(rbind(n_table,bind_rows(summaries)))
}


# sex SD function wrapper -------------------------------------------------

SD_table_by_sex <- function(data, vars, by, rd = 1, missing = "exclude",filter_PNTA_DNK=FALSE) {
  # Split dataset
  data_F <- data %>% filter(Sex == "Female")
  data_M <- data %>% filter(Sex == "Male")
  
  # Run your existing function
  tab_F <- SD_table_function(data_F, vars=vars, by=by, rd = rd, missing = missing)
  tab_M <- SD_table_function(data_M, vars=vars, by=by, rd = rd, missing = missing)
  
  # Add sex labels
  tab_F <- tab_F %>% mutate(Sex = "Female")
  tab_M <- tab_M %>% mutate(Sex = "Male")
  
  # Build keys
  keys_F <- interaction(tab_F$variable, tab_F$level, drop = TRUE)
  keys_M <- interaction(tab_M$variable, tab_M$level, drop = TRUE)
  
  # Split
  split_F <- split(tab_F, keys_F)[as.character(keys_F)]
  split_M <- split(tab_M, keys_M)[as.character(keys_M)]
  
  # Interleave using Female's key order
  interleaved <- lapply(names(split_F), function(k) {
    bind_rows(split_F[[k]], split_M[[k]])
  })
  
  # Bind in the correct order
  final <- bind_rows(interleaved) %>%
    relocate(Sex, .after = level)
  if(filter_PNTA_DNK){final<-final %>% filter(!(level%in%c("Prefer not to answer","Do not know")))}
  final #%>% arrange(variable,level,Sex)
}


# or table function -----------------------------------------------------

or_table_df <- function(data, formula, subset = NULL, digits = 2) {
  # Apply subset if provided
  if (!is.null(subset)) {
    data <- data[subset, ]
  }
  # Fit logistic regression
  mod <- glm(formula = formula,
             data = data,
             family = binomial(link = "logit"))
  # Extract coefficients and Wald CIs
  coefs <- coef(mod)
  ci    <- confint.default(mod)
  # Exponentiate
  OR  <- exp(coefs)
  LCL <- exp(ci[,1])
  UCL <- exp(ci[,2])
  # Build data frame
  out <- data.frame(
    term = names(coefs),
    OR   = round(OR, digits),
    LCL  = round(LCL, digits),
    UCL  = round(UCL, digits),
    stringsAsFactors = FALSE
  )
  # Add formatted column
  out$formatted <- paste0(out$OR, " (", out$LCL, "–", out$UCL, ")")
  return(out)
}



# plot OR_table_df output -------------------------------------------------

plot_or <- function(or_table_1,or_table_2,names){
  
  plot_data_sex %>%
    ggplot(aes(y=OR,x=`Shift work`,colour=Sex)) + 
    geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                  position = position_dodge(width = pd_width)) +
    geom_point(position = position_dodge(width = pd_width)) + 
    theme_bw() +
    theme(axis.title.y = element_blank(),
          legend.position=c(0.8, 0.25),
          legend.background = element_blank())+
    ylab("Odds Ratio") +
    ylim(c(.6,1.5)) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(plot_data$`Shift work`)))+
    scale_colour_manual(#values = cbPalette[2:4],
      #values = c("transparent","transparent","black"),
      values = c(cbPalette[c(2,3)],"black"),
      name = element_blank(),guide = guide_legend(reverse=TRUE))-> p_sex
}
