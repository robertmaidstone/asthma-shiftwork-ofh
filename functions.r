SD_table_function <- function(data, vars, rd = 1) {
  
  # Filter out unwanted JiNS_o values
  data <- data %>% 
    filter(!(Shift_work %in% c(NA, "Do not know", "Prefer not to answer")))
  
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
    
    # Logical: only TRUE percentage
    if (is.logical(x)) {
      pct_true <- round(100 * sum(x, na.rm = TRUE) / sum(!is.na(x)), rd)
      return(
        tibble(
          level = "TRUE",
          value = as.character(pct_true)
        )
      )
    }
    
    # Multi-level categorical
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
      group_by(Shift_work) %>%
      summarise(tmp = list(
        if (is.numeric(x)) {
          summarise_numeric(.data[[v]])
        } else {
          summarise_categorical(.data[[v]])
        }
      )) %>%
      tidyr::unnest(tmp) %>%
      mutate(variable = v) %>%
      dplyr::select(variable,level,everything()) %>%
      pivot_wider(names_from = "Shift_work",values_from = "value")
    out
  })
  
  
  return(bind_rows(summaries))
  
}
