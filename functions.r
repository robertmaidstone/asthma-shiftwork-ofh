SD_table_function <- function(data, vars, by, rd = 1) {
  # Convert grouping variable to tidy-eval symbol
  by_var <- rlang::ensym(by)
  #Filter out unwanted values *only for the grouping variable*
  data <- data %>%
    filter(!(!!by_var %in% c(NA, "Do not know", "Prefer not to answer")))
  # Helper functions
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
  # Loop over variables and summarise
  summaries <- lapply(vars, function(v) {
    x <- data[[v]]
    out <- data %>%
      group_by(!!by_var) %>%
      summarise(tmp = list(
        if (is.numeric(x)) {
          summarise_numeric(.data[[v]])
        } else {
          summarise_categorical(.data[[v]])
        }
      )) %>%
      tidyr::unnest(tmp) %>%
      mutate(variable = v) %>%
      select(variable, level, everything()) %>%
      pivot_wider(names_from = !!by_var, values_from = value)
    
    out
  })
  bind_rows(summaries)
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


