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


# model missing data ------------------------------------------------------

model_missing <- function(data, formula, subset = NULL) {
  if (!is.null(subset)) {
    data <- data[subset, ]
  }
  var_vec <- strsplit(formula,
                      split = "\\+|~")[[1]] |> trimws()
  
  if("Surv(followuptime, status)"%in% var_vec){
    var_vec[var_vec=="Surv(followuptime, status)"]<-"status"
    var_vec <- c("followuptime",var_vec)
  }
  # per-variable missing counts
  missing_table <- data %>%
    summarise(across(all_of(var_vec), ~ sum(is.na(.)))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "missing"
    )
  
  # total rows with any missing among var_vec
  total_missing <- data %>%
    filter(if_any(all_of(var_vec), is.na)) %>%
    nrow()
  
  # add total row
  missing_table <- missing_table %>%
    bind_rows(tibble(variable = "TOTAL_ANY_MISSING", missing = total_missing))
  
  missing_table
  
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

# or table out function -----------------------------------------------------

or_table_out <- function(data, mod_male, mod_female,shift_vars,pval) {
  if(length(shift_vars)==1){
    data<-data %>% mutate(Shift_work=Shift_work_b)
  }
  
  sample_sizes <- data %>%
    count(Sex, Shift_work, name = "N_total")
  
  case_counts <- data %>%
    group_by(Sex, Shift_work) %>%
    summarise(
      N_cases = sum(Asthma_med2 == 1, na.rm = TRUE),
      .groups = "drop"
    )
  
  group_summary <- sample_sizes %>%
    left_join(case_counts, by = c("Sex", "Shift_work")) %>%
    mutate(
      case_percent = N_cases / N_total * 100
    )
  
  mod_male2 <- mod_male %>%
    filter(term %in% shift_vars) %>%
    mutate(
      term_pretty = names(shift_vars)[match(term, shift_vars)],
      Sex="Male"
    )
  
  mod_female2 <- mod_female %>%
    filter(term %in% shift_vars) %>%
    mutate(
      term_pretty = names(shift_vars)[match(term, shift_vars)],
      Sex="Female"
    )
  
  or_all <- bind_rows(mod_male2, mod_female2)
  
  final_table <- group_summary %>%
    left_join(or_all, by = c("Sex", "Shift_work" = "term_pretty"))
  
  desired_row_order <- c("cases_display", "N_total", "formatted")
  desired_col_order <- names(shift_vars)
  
  final_table %>%
    arrange(Sex, Shift_work) %>%
    mutate(
      cases_display = paste0(N_cases, " (", sprintf("%.2f", case_percent), "%)")
    ) %>%
    select(Sex, Shift_work, cases_display, N_total, formatted) %>%
    mutate(N_total=as.character(N_total)) %>%
    pivot_longer(
      !c(Sex,Shift_work),
      names_to = "vars",
      values_to = "vals"
    ) %>%
    mutate(
      sex_interaction = ifelse(vars == "formatted", round(pval,4), "")
    ) %>%
    mutate(vars = factor(vars, levels = desired_row_order)) %>%
    pivot_wider(
      names_from="Shift_work",
      values_from="vals"
    ) %>%
    select(Sex,vars, "No shift work",all_of(desired_col_order),sex_interaction) %>%
    arrange(vars) 
}


# sex interation calc -----------------------------------------------------

OR_sex_int<-function(data,model,var="Shift_work"){
  
  modela<-paste(model,"+ Sex")
  modelb<-paste(model,"+ Sex + ",var,"*Sex")
  
  glm(data=data,formula=modela,family = binomial(link="logit")) -> mod2a
  glm(data=data,formula=modelb,family = binomial(link="logit")) -> mod2b
  
  
  logLik(mod2a) -> a
  logLik(mod2b) -> b
  
  # Likelihood ratio test statistic
  LR_stat <- 2 * (as.numeric(b) - as.numeric(a))
  
  # Degrees of freedom = difference in number of parameters
  df <- attr(b, "df") - attr(a, "df")
  
  # p-value
  p_value <- pchisq(LR_stat, df = df, lower.tail = FALSE)
  p_value
}

# plot OR_table_df output -------------------------------------------------

plot_or <- function(or_table_1, or_table_2, var_names,
                    group_names = c("Group 1", "Group 2"),
                    referent_label = "Day workers (referent)",
                    pd_width = 0.5) {
  
  # vars_named is a named vector: pretty_label = variable_name
  pretty_labels <- names(var_names)
  var_codes     <- unname(var_names)
  
  # Add group labels
  or_table_1$Group <- group_names[1]
  or_table_2$Group <- group_names[2]
  
  # Combine
  df <- rbind(or_table_1, or_table_2)
  
  # Filter to selected variables
  df <- df[df$term %in% var_codes, ]
  
  # Add pretty labels
  df$label <- pretty_labels[match(df$term, var_codes)]
  
  # Force ordering by var_names
  df$term  <- factor(df$term,  levels = var_codes)
  df$label <- factor(df$label, levels = pretty_labels)
  
  # Add referent row (no CI, OR = 1)
  ref_row <- data.frame(
    term = "referent",
    OR   = 1,
    LCL  = NA,
    UCL  = NA,
    formatted = NA,
    Group = NA,     # no colour
    label = referent_label,
    stringsAsFactors = FALSE
  )
  
  # Plot
  ggplot(df, aes(x = term, y = OR, colour = Group)) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 0.3) +
    geom_errorbar(data = subset(df, !is.na(Group)),
                  aes(ymin = LCL, ymax = UCL),
                  width = 0.3,
                  position = position_dodge(width = pd_width)) +
    # Points (referent has no colour)
    geom_point(
      data = subset(df, is.na(Group)),
      colour = "black",
      size = 2
    ) +
    geom_point(
      data = subset(df, !is.na(Group)),
      position = position_dodge(width = pd_width),
      size = 2
    ) +
    coord_flip() +
    theme_bw() +
    ylab("Odds Ratio") +
    xlab("") +
    scale_colour_manual(values = c("red", "black")) +
    scale_x_discrete(breaks = df$term,labels = df$label) +
    theme(legend.position = c(0.9, 0.9),
          legend.background = element_blank())
}

# hr table function -----------------------------------------------------

hr_table_df <- function(data, formula, subset = NULL, digits = 2) {
  # Apply subset if provided
  if (!is.null(subset)) {
    data <- data[subset, ]
  }
  
  #filter out data prior to baseline
  
  data <- data %>% dplyr::filter(is.na(asthma_beforebaseline)|!asthma_beforebaseline)
  # Convert character to formula if needed
  if (is.character(formula)) {
    formula <- as.formula(formula)
  }
  # Fit hazard ratio model
  mod <- coxph(formula, data = data)
  
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


# HR sex interation calc -----------------------------------------------------

HR_sex_int<-function(data,model,var="Shift_work"){
  #filter out data prior to baseline
  
  data <- data %>% dplyr::filter(is.na(asthma_beforebaseline)|!asthma_beforebaseline)
  # Convert character to formula if needed

  
  
  modela<-as.formula(paste(model,"+ Sex"))
  modelb<-as.formula(paste(model,"+ Sex + ",var,"*Sex"))
  
  coxph(modela, data = data) -> mod2a
  coxph(modelb, data = data)-> mod2b
  
  
  logLik(mod2a) -> a
  logLik(mod2b) -> b
  
  # Likelihood ratio test statistic
  LR_stat <- 2 * (as.numeric(b) - as.numeric(a))
  
  # Degrees of freedom = difference in number of parameters
  df <- attr(b, "df") - attr(a, "df")
  
  # p-value
  p_value <- pchisq(LR_stat, df = df, lower.tail = FALSE)
  p_value
}

# hr table out function -----------------------------------------------------

hr_table_out <- function(data, mod_male, mod_female,shift_vars,pval) {
  
  if(length(shift_vars)==1){
    data<-data %>% mutate(Shift_work=Shift_work_b)
  }
  
  data <- data %>% dplyr::filter(is.na(asthma_beforebaseline)|!asthma_beforebaseline)
  
  sample_sizes <- data %>%
    count(Sex, Shift_work, name = "N_total")
  
  case_counts <- data %>%
    group_by(Sex, Shift_work) %>%
    summarise(
      N_cases = sum(status == 1, na.rm = TRUE),
      .groups = "drop"
    )
  
  group_summary <- sample_sizes %>%
    left_join(case_counts, by = c("Sex", "Shift_work")) %>%
    mutate(
      case_percent = N_cases / N_total * 100
    )
  
  mod_male2 <- mod_male %>%
    filter(term %in% shift_vars) %>%
    mutate(
      term_pretty = names(shift_vars)[match(term, shift_vars)],
      Sex="Male"
    )
  
  mod_female2 <- mod_female %>%
    filter(term %in% shift_vars) %>%
    mutate(
      term_pretty = names(shift_vars)[match(term, shift_vars)],
      Sex="Female"
    )
  
  or_all <- bind_rows(mod_male2, mod_female2)
  
  final_table <- group_summary %>%
    left_join(or_all, by = c("Sex", "Shift_work" = "term_pretty"))
  
  
  
  
  desired_row_order <- c("cases_display", "N_total", "formatted")
  desired_col_order <- names(shift_vars)
  
  final_table %>%
    arrange(Sex, Shift_work) %>%
    mutate(
      cases_display = paste0(N_cases, " (", sprintf("%.2f", case_percent), "%)")
    ) %>%
    select(Sex, Shift_work, cases_display, N_total, formatted) %>%
    mutate(N_total=as.character(N_total)) %>%
    pivot_longer(
      !c(Sex,Shift_work),
      names_to = "vars",
      values_to = "vals"
    ) %>%
    mutate(
      sex_interaction = ifelse(vars == "formatted", round(pval,4), "")
    ) %>%
    mutate(vars = factor(vars, levels = desired_row_order)) %>%
    pivot_wider(
      names_from="Shift_work",
      values_from="vals"
    ) %>%
    select(Sex,vars, "No shift work",all_of(desired_col_order),sex_interaction) %>%
    arrange(vars)
}

plot_or2 <- function(or_tables, 
                     var_names,
                     group_names = NULL,
                     referent_label = "Day workers (referent)",
                     pd_width = 0.5) {
  
  # vars_named is a named vector: pretty_label = variable_name
  pretty_labels <- names(var_names)
  var_codes     <- unname(var_names)
  
  # If no group names provided, auto-generate
  if (is.null(group_names)) {
    group_names <- paste("Group", seq_along(or_tables))
  }
  
  # Add group labels to each table
  for (i in seq_along(or_tables)) {
    or_tables[[i]]$Group <- group_names[i]
  }
  
  # Combine all tables
  df <- do.call(rbind, or_tables)
  df$Group <- factor(df$Group, levels = group_names)
  
  # Filter to selected variables
  df <- df[df$term %in% var_codes, ]
  
  # Add pretty labels
  df$label <- pretty_labels[match(df$term, var_codes)]
  
  # Force ordering by var_names
  df$term  <- factor(df$term,  levels = var_codes)
  df$label <- factor(df$label, levels = pretty_labels)
  
  # Referent row (optional)
  ref_row <- data.frame(
    term = "referent",
    OR   = 1,
    LCL  = NA,
    UCL  = NA,
    formatted = NA,
    Group = NA,
    label = referent_label,
    stringsAsFactors = FALSE
  )
  
  # Plot
  ggplot(df, aes(x = term, y = OR, colour = Group)) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 0.3) +
    geom_errorbar(aes(ymin = LCL, ymax = UCL),
                  width = 0.3,
                  position = position_dodge(width = pd_width)) +
    geom_point(position = position_dodge(width = pd_width), size = 2) +
    coord_flip() +
    theme_bw() +
    ylab("Odds Ratio") +
    xlab("") +
    scale_x_discrete(breaks = df$term, labels = df$label) +
    theme(legend.position = c(0.9, 0.9),
          legend.background = element_blank())
}









