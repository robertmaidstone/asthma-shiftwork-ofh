load_and_merge <- function(..., id = "pid") {
  
  file_paths <- c(...)
  
  # Check files exist
  missing_files <- file_paths[!file.exists(file_paths)]
  if (length(missing_files) > 0) {
    stop("These files do not exist: ", paste(missing_files, collapse = ", "))
  }
  
  # Read all files
  data_list <- lapply(file_paths, read.csv)
  
  # Check id column exists in all datasets
  missing_id <- which(!sapply(data_list, function(df) id %in% names(df)))
  if (length(missing_id) > 0) {
    stop("The ID column '", id, "' is missing in dataset(s): ",
         paste(file_paths[missing_id], collapse = ", "))
  }
  
  # Merge all datasets
  reduce(data_list, left_join, by = id)
}

# define asthma variables -------------------------------------------------

derive_asthma_vars <- function(data) {
  
  asthma_patterns <- c(
    Ast_Reliever        = "Asthma reliever inhaler",
    Ast_Preventer       = "Asthma preventer inhaler",
    Ast_Combination     = "Asthma combination inhaler",
    Ast_Anticholinergic = "Anticholinergic inhaler",
    Ast_Leukotriene     = "Leukotriene receptor antagonist",
    Ast_Bronchodilator  = "Tablet bronchodilator",
    Ast_Corticosteroids = "Corticosteroids",
    Ast_Other           = "Other not listed",
    Ast_Unknown         = "Do not know|Prefer not to answer",
    Ast_None            = "None of the above"
  )
  
  for (nm in names(asthma_patterns)) {
    data[[nm]] <- str_detect(data$medicat_resp_1_m, asthma_patterns[[nm]])
  }
  
  data <- data %>%
    mutate(
      Lung_med      = str_detect(medicat_1_m, "Lung or breathing problems"),
      Asthma        = str_detect(diag_resp_1_m, "Asthma"),
      Asthma2       = Asthma & Lung_med,
      Asthma_med    = Asthma & (Ast_Preventer | Ast_Combination),
      Asthma_med2   = if_else(Asthma & !(Ast_Preventer | Ast_Combination),
                              NA, Asthma_med)
    )
  
  data
}