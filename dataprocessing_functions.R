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


# never/rarely, sometimes, usually, always --------------------------------

factor_nsua <- function(x) {
  allowed_levels <- c(
    "Prefer not to answer",
    "Do not know",
    "Never/rarely",
    "Sometimes",
    "Usually",
    "Always",
    ""
  )
  # find unexpected values
  unexpected <- setdiff(unique(x), allowed_levels)
  # warn if anything is unexpected
  if (length(unexpected) > 0) {
    warning(
      "Unexpected values found: ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }
  # return ordered factor
  factor(
    x,
    levels = allowed_levels,
    ordered = TRUE
  )
}

factor_nsua_cond <- function(x){
  x <- factor_nsua(x)
  
  x <- ifelse(
    x %in% c("Sometimes", "Usually"),
    "Occasionally",
    as.character(x)
  )
  
  x <- factor(x,
              levels=c(
                "Prefer not to answer",
                "Do not know",
                "Never/rarely",
                "Occasionally",
                "Always",
                ""
              ),
              ordered=T)
  x
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

clean_packyears <- function(data,
                            start_var,
                            stop_var,
                            cigs_var,
                            current_age_var,
                            suffix = "") {
  
  data %>%
    mutate(
      # Extract numeric values
      age_start_raw = as.numeric(str_extract({{ start_var }}, "\\d+")),
      age_stop_raw  = as.numeric(str_extract({{ stop_var }}, "\\d+")),
      cigs_day      = as.numeric(str_extract({{ cigs_var }}, "\\d+")),
      current_age   = {{ current_age_var }},
      
      # Identify obvious reversals
      reversal_flag = age_start_raw > age_stop_raw &
        age_start_raw >= 5 &
        age_stop_raw <= current_age &
        (age_start_raw - age_stop_raw) < 70,
      
      # Correct reversals only when unambiguous
      age_start = if_else(reversal_flag, age_stop_raw, age_start_raw),
      age_stop  = if_else(reversal_flag, age_start_raw, age_stop_raw),
      
      # Duration
      duration = age_stop - age_start,
      
      # Pack-years
      packyears = (duration * cigs_day) / 20,
      
      # Implausible flags
      implausible = case_when(
        is.na(age_start) | is.na(age_stop) ~ TRUE,
        age_start < 5 ~ TRUE,
        age_stop > current_age ~ TRUE,
        duration < 0 ~ TRUE,
        #cigs_day > 80 ~ TRUE,          # >4 packs/day
        #packyears > 300 ~ TRUE,        # extreme upper bound
        TRUE ~ FALSE
      ),
      
      # Cleaned pack-years
      packyears_clean = if_else(implausible, NA_real_, packyears)
    ) %>%
    # Add suffix to avoid overwriting when running twice
    rename_with(~ paste0(., suffix),
                c(age_start_raw, age_stop_raw, cigs_day,
                  reversal_flag, age_start, age_stop,
                  duration, packyears, implausible, packyears_clean))
}


derive_smoking_vars <- function(data) {
  data <- data %>%
    ############### smoking status ################
  mutate(
    smoking_status = case_when(
      is.na(smoke_tobacco_type_1_m) |
        str_detect(smoke_tobacco_type_1_m, "Prefer not to answer") ~ NA_character_,
      
      str_detect(smoke_tobacco_type_1_m,
                 "I have not used any of these tobacco") ~ "Never smoker",
      
      is.na(smoke_reg_1_m) |
        str_detect(smoke_reg_1_m, "Prefer not to answer") ~ NA_character_,
      
      str_detect(smoke_reg_1_m,
                 "I have not used any of these tobacco") ~ "Not regular",
      
      !str_detect(smoke_reg_1_m, "Cigarettes") ~ "Regular, but not cigarettes",
      
      smoke_status_2_1 == "Yes, every day" ~ "Current regular smoker",
      
      smoke_status_2_1 == "Yes, some days" &
        smoke_prev_reg_2_1 == "Yes" ~ "Current occasional, previous regular",
      
      smoke_status_2_1 == "Yes, but rarely" &
        smoke_prev_reg_2_1 == "Yes" ~ "Current occasional, previous regular",
      
      smoke_status_2_1 == "No, not at all" &
        smoke_prev_reg_2_1 == "Yes" ~ "Not current, previous regular",
      
      smoke_status_2_1 == "Yes, some days" &
        smoke_prev_reg_2_1 == "No" ~ "Occasional smoker",
      
      smoke_status_2_1 == "Yes, but rarely" &
        smoke_prev_reg_2_1 == "No" ~ "Occasional smoker",
      
      smoke_status_2_1 == "No, not at all" &
        smoke_prev_reg_2_1 == "No" ~ "Occasional smoker"
    ),
    
    smoking_status = factor(
      smoking_status,
      levels = c(
        "Never smoker",
        "Not regular",
        "Regular, but not cigarettes",
        "Occasional smoker",
        "Current occasional, previous regular",
        "Not current, previous regular",
        "Current regular smoker"
      ),
      ordered = TRUE
    ),
    smoking_status_cond = factor(case_when(
      smoking_status == "Never smoker" ~ "Never smoker",
      smoking_status == "Not regular" ~ "Occasional smoker",
      smoking_status == "Regular, but not cigarettes" ~ "Regular, but not cigarettes",
      smoking_status == "Not regular" ~ "Occasional smoker",
      smoking_status == "Occasional smoker" ~ "Occasional smoker",
      smoking_status == "Current occasional, previous regular" ~ "Previous regular smoker",
      smoking_status == "Not current, previous regular" ~ "Previous regular smoker",
      smoking_status == "Current regular smoker" ~ "Current regular smoker"
    ),levels= c(
      "Never smoker",
      "Regular, but not cigarettes",
      "Occasional smoker",
      "Previous regular smoker",
      "Current regular smoker"
    ), ordered=TRUE
    )
  )%>%
    
    ###### packyears ########
  clean_packyears(
    start_var = smoke_reg_first_age_2_1,
    stop_var  = Age,   # current smokers haven't stopped
    cigs_var  = smoke_reg_day_2_1,
    current_age_var = Age,
    suffix = "_current"
  ) %>%
    clean_packyears(
      start_var = smoke_first_age_2_1,
      stop_var  = smoke_prev_age_2_1,
      cigs_var  = smoke_avg_2_1,
      current_age_var = Age,
      suffix = "_previous"
    ) %>%
    mutate(
      # Flag cases where both current and previous pack-years exist
      packyears_both_flag = !is.na(packyears_current) &
        !is.na(packyears_previous),
      # Combine into a single variable
      packyears_clean_combined = case_when(
        packyears_both_flag ~ NA_real_,  # both exist → set to missing
        !is.na(packyears_current) ~ packyears_clean_current,
        !is.na(packyears_previous) ~ packyears_clean_previous,
        TRUE ~ NA_real_
      ),
      packyears_combined = case_when(
        packyears_both_flag ~ NA_real_,  # both exist → set to missing
        !is.na(packyears_current) ~ packyears_current,
        !is.na(packyears_previous) ~ packyears_previous,
        TRUE ~ NA_real_
      )
    ) %>%
    #################### smoke exposure ####################
  mutate(
    smoke_expo_hrs = case_when(
      smoke_exposure_1_1 == "Never" ~ 0,
      is.na(smoke_exposure_hrs_1_1)|(smoke_exposure_hrs_1_1=="Prefer not to answer") ~ NA_real_,
      smoke_exposure_hrs_1_1 == "Less than 1 hour per day" ~ .5,
      smoke_exposure_hrs_1_1 == "1 to 2 hours per days" ~ 1.5,
      smoke_exposure_hrs_1_1 == "3 to 5 hours per day" ~ 4,
      smoke_exposure_hrs_1_1 == "6 to 9 hours per day" ~ 7.5,
      smoke_exposure_hrs_1_1 == "10 to 15 hours per day" ~ 12.5
    ),
    smoke_expo=case_when(
      is.na(smoke_exposure_1_1)|(smoke_exposure_1_1=="Prefer not to answer") ~ NA_real_,
      smoke_exposure_1_1 == "Never" ~ 0,
      smoke_exposure_1_1 == "Every day" ~ 365,
      smoke_exposure_1_1 == "Most days of the week" ~ 5.5*52.1429,
      smoke_exposure_1_1 == "One day per week" ~ 1*52.1429,
      smoke_exposure_1_1 == "One day per month" ~ 1*12,
      smoke_exposure_1_1 == "A few days per year" ~ 5
    ),
    smoke_expo_year_hrs = smoke_expo_hrs*smoke_expo
  ) %>%
    ############# vapes #################################
  mutate(regular_vapers =  str_detect(smoke_reg_1_m, "Electronic delivery devices"),
         age_start_vape_raw = ifelse(regular_vapers,smoke_vape_avg_2_1,NA_real_),
         amount_vaped = case_when(
           is.na(smoke_vape_avg_2_1)|(smoke_vape_avg_2_1%in%c("","Prefer not to answer")) ~ NA_real_,
           smoke_vape_avg_2_1 == "Never" ~ 0,
           smoke_vape_avg_2_1 == "More than 45 times/day" ~ 45,
           smoke_vape_avg_2_1 == "35-44 times/day" ~ 39.5,
           smoke_vape_avg_2_1 == "25-34 times/day" ~ 29.5,
           smoke_vape_avg_2_1 == "15-24 times/day" ~ 19.5,
           smoke_vape_avg_2_1 == "5-14 times/day" ~ 9.5,
           smoke_vape_avg_2_1 == "1-4 times/day" ~ 2.5,
           smoke_vape_avg_2_1 == "3-6 times/week" ~ 4.5/7,
           smoke_vape_avg_2_1 == "1-2 times/week" ~ .5/7,
           smoke_vape_avg_2_1 == "2-3 times/mo" ~ 2.5/30.44,
           smoke_vape_avg_2_1 == "Less than 1 time/mo" ~ .5/30.44
         )
         
  ) %>%
    clean_packyears(
      start_var = age_start_vape_raw,
      stop_var  = Age,
      cigs_var  = amount_vaped,
      current_age_var = Age,
      suffix = "_vape"
    )
  
  vape_patterns <- c(
    Vape_Fruit_NoNic = "Fruit/dessert flavor WITHOUT nicotine",
    Vape_Fruit_Nic = "Fruit/dessert flavor WITH nicotine",
    Vape_Menth_NoNic = "Menthol flavor WITHOUT nicotine",
    Vape_Menth_Nic = "Menthol flavor WITH nicotine",
    Vape_Tobac_NoNic = "Tobacco flavor WITHOUT nicotine",
    Vape_Tobac_Nic = "Tobacco flavor WITH nicotine",
    Vape_Other = "Marijuana|Alcohol|Other"
  )
  
  for (nm in names(vape_patterns)) {
    data[[nm]] <- str_detect(data$smoke_vape_type_2_m, vape_patterns[[nm]])
  }
  
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
      LengthWW = as.numeric(gsub("[^0-9]", "", work_wk_hrs_1_1)),
      Work_Manual_Labour = factor_nsua_cond(work_manual_labour_1_1),
      Years_in_Current_Job = ifelse(
        work_yrs_1_1 == "Less than a year",
        0,
        as.numeric(gsub("[^0-9]", "", work_yrs_1_1))
      ),
      Work_WalkorStand = factor_nsua_cond(work_walk_stand_1_1)
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

unit_map <- list(
  BEER = 2.3,
  SPIRITS = 1.0,
  WINE_RED = 2.3,
  WINE_WHITE = 2.3,
  WINE_FORT = 1.5,
  OTHER = 1.1
)

calc_units <- function(data, prefix, unit_value) {
  mth <- data[[paste0(prefix, "_mth_2_1")]]
  wk  <- data[[paste0(prefix, "_wk_1_1")]]
  
  weekly_from_mth <- if (!is.null(mth)) (mth/4.345) * unit_value else 0
  weekly_from_wk  <- if (!is.null(wk))  wk * unit_value else 0
  
  weekly_from_mth + weekly_from_wk
}

derive_alcohol_vars <- function(data) {
  data <- data %>%
    mutate(Alcohol_status = case_when(
             (alcohol_curr_1_1=="Never") & (alcohol_prev_1_1=="No") ~ "Never drinker",
             (alcohol_curr_1_1=="Never") & (alcohol_prev_1_1=="Yes") ~ "Previous drinker",
             (alcohol_curr_1_1%in%c('Special occasions only','One to three times a month')) ~ "Less than 4 times a month",
             (alcohol_curr_1_1%in%c('Once or twice a week','Three or four times a week')) ~ "1-4 times a week",
             (alcohol_curr_1_1=="Daily or almost daily") ~ "Daily or almost daily"
           ),
           Alcohol_status=factor(Alcohol_status,levels=c('Never drinker','Previous drinker','Less than 4 times a month','1-4 times a week','Daily or almost daily'),ordered=T),
           
           units_beer  = calc_units(.data, "alcohol_beer",  unit_map$BEER),
           units_spir  = calc_units(.data, "alcohol_spirits", unit_map$SPIRITS),
           units_red   = calc_units(.data, "alcohol_wine_red", unit_map$WINE_RED),
           units_white = calc_units(.data, "alcohol_wine_white", unit_map$WINE_WHITE),
           units_fort  = calc_units(.data, "alcohol_wine_fort", unit_map$WINE_FORT),
           units_other = calc_units(.data, "alcohol_other", unit_map$OTHER),
           
           total_units_week = units_beer + units_spir + units_red +
             units_white + units_fort + units_other
           )
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
