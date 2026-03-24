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

# smoking variables -------------------------------------------------------

derive_smoking_vars <- function(data) {
data <- data %>%
  mutate(
    AnySmoking = !str_detect(smoke_tobacco_type_1_m, "I have not used"),
    Cigarettes = str_detect(smoke_tobacco_type_1_m, "manufactured|hand-rolled"),
    SmokeAmount = case_when(
      !AnySmoking ~ "None",
      Cigarettes ~ smoke_100_times_2_1,
      TRUE ~ NA
    )
  )
data
}

# shift work variables ----------------------------------------------------

derive_shiftwork_vars <- function(data) {
  data <- data %>%
    mutate(
      Shift_work = case_when(
        work_shifts_1_1 == "Never/rarely" ~ "No shift work",
        work_nights_1_1 == "Usually" ~ "Sometimes",
        work_nights_1_1 != "" ~ work_nights_1_1,
        TRUE ~ NA
      ),
      Shift_work = factor(
        Shift_work,
        levels = c("No shift work","Never/rarely","Sometimes","Always","Do not know","Prefer not to answer")
      ),
      Shift_work_b=case_when(
        Shift_work =="No shift work" ~ "No shift work",
        Shift_work %in% c("Never/rarely","Sometimes","Always") ~ "Shift work"
        )
    )
  
  data
}

# contraception variables -------------------------------------------------

derive_contraception_vars <- function(data) {
  data$gyn_contracept_methods_1_m <- ifelse(data$gyn_contracept_methods_1_m=="",NA,data$gyn_contracept_methods_1_m)
  
  contraception_patterns <- c(
    Cont_ComPill = "Combined Pill",
    Cont_Injection = "Injection",
    Cont_IUD = "IUD",
    Cont_IUS = "IUS",
    Cont_ProgPill = "Progesterone only pill",
    Cont_Patch = "Patch",
    Cont_Ring = "Vaginal ring",
    Cont_Other = "Other not listed"
  )
  
  for (nm in names(contraception_patterns)) {
    data[[nm]] <- str_detect(data$gyn_contracept_methods_1_m, contraception_patterns[[nm]])
  }
  
  data
}

# clinical variables -------------------------------------------------

derive_clinical_vars <- function(data) {
  data <- data %>%
    mutate(BMI = weight / (height/100)^2,
            BMI_class = cut(BMI, breaks=c(-Inf,18.5,25,30,35,40,Inf), closed = "left", boundary = 10),
            BRI = 364.2-365.5*sqrt(1-((waist/(2*pi))^2)/((.5*height)^2))
  )
  data
}

# job variables -------------------------------------------------

derive_job_vars <- function(data) {
  data <- data %>%
    mutate(
      Income = housing_income_1_1,
      Income_cat=factor(Income,levels=c('Less than £18,000','£18,000 to £30,999','£31,000 to £51,999','£52,000 to £100,000','Greater than £100,000'),ordered=T),
      LengthWW = as.numeric(gsub("[^0-9]", "", work_wk_hrs_1_1))
    )
  data
}

# sleep variables -------------------------------------------------

derive_sleep_vars <- function(data) {
  data <- data %>%
    mutate(
      Chronotype=case_when(sleep_chronotype_1_1 %in% c("More a 'morning' than 'evening' person","More an 'evening' than a 'morning' person") ~ "Intermediate",
                           sleep_chronotype_1_1=="Definitely a 'morning' person" ~ "Morning",
                           sleep_chronotype_1_1=="Definitely an 'evening' person" ~ "Evening"),
      sleep_dur=as.numeric(gsub("[^0-9]", "", sleep_hrs_1_1))
    )
  data
}

# alcohol variables -------------------------------------------------

derive_alcohol_vars <- function(data) {
  data <- data %>%
    mutate(Alcohol_status = ifelse(alcohol_curr_1_1=="Never",alcohol_prev_1_1,alcohol_curr_1_1),
           Alcohol_status=factor(Alcohol_status,levels=c('No','Special occasions only','One to three times a month','Once or twice a week','Three or four times a week','Daily or almost daily','Yes'),ordered=T))
    data
}

# exercise variables -------------------------------------------------

derive_exercise_vars <- function(data) {
  data <- data %>% 
    mutate(Days_walk = ifelse(
      activity_walk_days_2_1 == "Unable to walk",
      0,
      as.numeric(gsub("[^0-9]", "", activity_walk_days_2_1))),
      Days_mod = as.numeric(gsub("[^0-9]", "", activity_mod_days_2_1)),
      Days_vig = as.numeric(gsub("[^0-9]", "", activity_vig_days_2_1))
    )
  data
}


# core demographic variables -------------------------------------------------

derive_core_demographic_vars <- function(data) {
  data <- data %>%
    mutate(
      Sex = demog_sex_2_1,
      Submission_year = (submission_date |> strsplit("-") |> unlist())[(1:length(submission_date))*3-2],
      Age = as.numeric(Submission_year)-as.numeric(gsub("[^0-9]", "", birth_year)), #change to be based on months/days as well as year??
      Ethnicity = demog_ethnicity_1_1,
      ethnicity_group = case_when(
        str_detect(demog_ethnicity_1_1, "White.*British") ~ "White British",
        str_detect(demog_ethnicity_1_1, "White.*Irish|Any other white|Polish|Gypsy|Traveller") ~ "White Other",
        str_detect(demog_ethnicity_1_1, "^Mixed|Any other mixed") ~ "Mixed",
        str_detect(demog_ethnicity_1_1, "Indian|Pakistani|Bangladeshi|Asian") ~ "Asian",
        str_detect(demog_ethnicity_1_1, "Black|African|Caribbean") ~ "Black",
        str_detect(demog_ethnicity_1_1, "Chinese") ~ "Chinese",
        TRUE ~ "Other"
      )
    )
  data
}

# environment variables ----------------------------------------------------
derive_environment_vars <- function(data) {
  data <- data %>%
    mutate(
      Hours_Outdoor_Summer = ifelse(
        lifestyle_outdoor_sum_hrs_1_1 == "Less than an hour a day",
        0,
        as.numeric(gsub("[^0-9]", "", lifestyle_outdoor_sum_hrs_1_1))
        ),
        Hours_Outdoor_Winter = ifelse(
          lifestyle_outdoor_win_hrs_1_1 == "Less than an hour a day",
          0,
          as.numeric(gsub("[^0-9]", "", lifestyle_outdoor_win_hrs_1_1))
        )
      )
  data
}


# filter out missing data -------------------------------------------------

filter_missing_data <- function(data){
  data <- data %>% 
    filter(Sex%in%c("Female","Male"),
           !is.na(Asthma_med),
           !is.na(Shift_work),
           !is.na(Age),
           !(Shift_work %in% c("Prefer not to answer","Do not know")),
           !(Ethnicity %in% c("Prefer not to answer","Do not know")),
           !(Alcohol_status %in% c("Prefer not to answer","Do not know")),
           !is.na(Days_walk),
           !is.na(Days_mod),
           !is.na(Days_vig),
           !is.na(LengthWW),
           !(Chronotype %in% c("Prefer not to answer","Do not know")),
           !(Income %in% c("Prefer not to answer","Do not know")),
           !(sleep_hrs_1_1 %in% c("Do not know","Prefer not to answer"))
           )
  data
}
