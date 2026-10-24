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
    Ast_Reliever        = 1,
    Ast_Preventer       = 2,
    Ast_Combination     = 3,
    Ast_Anticholinergic = 4,
    Ast_Leukotriene     = 5,
    Ast_Bronchodilator  = 6,
    Ast_Corticosteroids = 7,
    Ast_Other           = 8,
    Ast_Unknown         %in% c(-1,-3),
    Ast_None            = -7
  )
  
  for (nm in names(asthma_patterns)) {
    data[[nm]] <- str_detect(data$medicat_resp_1_m, asthma_patterns[[nm]])
  }
  
  data <- data %>%
    mutate(
      Lung_med      = medicat_1_m==8 ,
      Asthma        = diag_resp_1_m ==4,
      Asthma2       = Asthma & Lung_med,
      Asthma_med    = Asthma & (Ast_Preventer | Ast_Combination),
      Asthma_med2   = if_else(Asthma & !(Ast_Preventer | Ast_Combination),
                              NA, Asthma_med)
    )
  
  data
}

# define comorbidity variables -------------------------------------------------

derive_comorbid_vars <- function(data) {
  data <- data %>%
    mutate(
      COPD = str_detect(diag_resp_1_m, "COPD"),
      Lung_fibrosis = str_detect(diag_resp_1_m, "Lung fibrosis"),
      Bronchiectasis = str_detect(diag_resp_1_m, "Bronchiectasis"),
      Hay_fever = str_detect(diag_resp_1_m, "Hay Fever"),
      Other_Resp = str_detect(diag_resp_1_m, "Other"),
      Any_CVD = diag_cvd_1_m != "",
      High_Cholesterol = str_detect(diag_cvd_1_m, "High Cholesterol"),
      High_Blood_Pres = str_detect(diag_cvd_1_m, "High Blood Pressure"),
      Anxiety = str_detect(diag_psych_1_m, "Anxiety"),
      Depression = str_detect(diag_psych_1_m, "Depression"),
      Type_I_Diabetes = str_detect(diag_endocr_1_m, "Type 1 diabetes"),
      Type_II_Diabetes = str_detect(diag_endocr_1_m, "Type 2 diabetes"),
      GO_Acid_Reflux = str_detect(diag_gastro_1_m, "oesophageal")
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
        smoke_tobacco_type_1_m==-3 ~ NA_character_,
      
      smoke_tobacco_type_1_m==6 ~ "Never smoker",
      
      is.na(smoke_reg_1_m) |
        smoke_reg_1_m==-3 ~ NA_character_,
      
      smoke_reg_1_m==6 ~ "Not regular",
      
      smoke_reg_1_m!=0 ~ "Regular, but not cigarettes",
      
      smoke_status_2_1 == 1 ~ "Current regular smoker",
      
      smoke_status_2_1 == 2 &
        smoke_prev_reg_2_1 == 1 ~ "Current occasional, previous regular",
      
      smoke_status_2_1 == 3 &
        smoke_prev_reg_2_1 == 1 ~ "Current occasional, previous regular",
      
      smoke_status_2_1 == 0 &
        smoke_prev_reg_2_1 == 1 ~ "Not current, previous regular",
      
      smoke_status_2_1 == 2 &
        smoke_prev_reg_2_1 == 0 ~ "Occasional smoker",
      
      smoke_status_2_1 == 3 &
        smoke_prev_reg_2_1 == 0 ~ "Occasional smoker",
      
      smoke_status_2_1 == 0 &
        smoke_prev_reg_2_1 == 0 ~ "Occasional smoker"
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
      ),
      packyears_clean_combined_0 = case_when(
        is.na(smoking_status_cond) ~ NA_real_,
        is.na(packyears_clean_combined) ~ 0,
        !is.na(packyears_clean_combined) ~ packyears_clean_combined
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
  mutate(regular_vapers =  str_detect(smoke_reg_1_m, "1"),
         age_start_vape_raw = ifelse(regular_vapers,smoke_vape_avg_2_1,NA_real_),
         amount_vaped = case_when(
           is.na(smoke_vape_avg_2_1)|(smoke_vape_avg_2_1%in%c(NA,-3)) ~ NA_real_,
           smoke_vape_avg_2_1 == 1 ~ 0,
           smoke_vape_avg_2_1 == 11 ~ 45,
           smoke_vape_avg_2_1 == 10 ~ 39.5,
           smoke_vape_avg_2_1 == 9 ~ 29.5,
           smoke_vape_avg_2_1 == 8 ~ 19.5,
           smoke_vape_avg_2_1 == 7 ~ 9.5,
           smoke_vape_avg_2_1 == 6 ~ 2.5,
           smoke_vape_avg_2_1 == 5 ~ 4.5/7,
           smoke_vape_avg_2_1 == 4 ~ .5/7,
           smoke_vape_avg_2_1 == 3 ~ 2.5/30.44,
           smoke_vape_avg_2_1 == 2 ~ .5/30.44
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
    Vape_Fruit_NoNic = 1,
    Vape_Fruit_Nic = 2,
    Vape_Menth_NoNic = 3,
    Vape_Menth_Nic = 4,
    Vape_Tobac_NoNic = 5,
    Vape_Tobac_Nic = 6,
    Vape_Other = "7|8|9"
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
      Shift_work = case_when(
        work_shifts_1_1 == 1 ~ "No shift work",
        work_nights_1_1 ==3 ~ "Sometimes",
        work_nights_1_1 == 2 ~ "Sometimes",
        work_nights_1_1 == 1 ~ "Never/rarely",
        work_nights_1_1 == 4 ~ "Always",
        work_nights_1_1 == -1 ~ "Do not know",
        work_nights_1_1 == -3 ~ "Prefer not to answer"
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
      Income_cat=factor(Income,levels=c(-3,-1,1,2,3,4,5),
                        labels=c('Prefer not to answer','Do not know', 'Less than £18,000','£18,000 to £30,999','£31,000 to £51,999','£52,000 to £100,000','Greater than £100,000'),ordered=T),
      LengthWW = as.numeric(gsub("[^0-9]", "", work_wk_hrs_1_1)),
      #Work_Manual_Labour = factor_nsua_cond(work_manual_labour_1_1),
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
      Chronotype=case_when(sleep_chronotype_1_1 %in% c(2,3) ~ "Intermediate",
                           sleep_chronotype_1_1==1 ~ "Morning",
                           sleep_chronotype_1_1==4 ~ "Evening"),
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
  
  mth <- as.numeric(gsub("[^0-9]", "", mth))
  wk <- as.numeric(gsub("[^0-9]", "", wk))
  
  # If BOTH are NA → return NA
  both_na <- is.na(mth) & is.na(wk)
  
  # Replace NA with 0 for calc
  mth[is.na(mth)] <- 0
  wk[is.na(wk)]   <- 0
  
  # Compute weekly units
  out <- (mth / 4.345) * unit_value + wk * unit_value
  
  # Restore NA where both inputs were NA
  out[both_na] <- NA_real_
  
  out
}

derive_alcohol_vars <- function(data) {
  data <- data %>%
    mutate(Alcohol_status = case_when(
      (alcohol_curr_1_1==6) & (alcohol_prev_1_1==0) ~ "Never drinker",
      (alcohol_curr_1_1==6) & (alcohol_prev_1_1==1) ~ "Previous drinker",
      (alcohol_curr_1_1%in%c(5,4)) ~ "Less than 4 times a month",
      (alcohol_curr_1_1%in%c(3,2)) ~ "1-4 times a week",
      (alcohol_curr_1_1==1) ~ "Daily or almost daily"
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
      activity_walk_days_2_1 == -2,
      0,
      ifelse(activity_walk_days_2_1 %in% c(-3,-1), NA_real_, as.numeric(gsub("[^0-9]", "", activity_walk_days_2_1)))),
      Days_mod = ifelse(activity_walk_days_2_1 %in% c(-3,-1), NA_real_,as.numeric(gsub("[^0-9]", "", activity_mod_days_2_1))),
      Days_vig = ifelse(activity_walk_days_2_1 %in% c(-3,-1), NA_real_,as.numeric(gsub("[^0-9]", "", activity_vig_days_2_1)))
    )
  data
}


# core demographic variables -------------------------------------------------

derive_core_demographic_vars <- function(data) {
  data <- data %>%
    mutate(
      Sex = case_when(
        demog_sex_2_1==1 ~ "Male",
        demog_sex_2_1==2 ~ "Female",
        demog_sex_2_1==3 ~ "Intersex",
        demog_sex_2_1==-3 ~ "Prefer not to answer"
        ),
      Submission_year = lubridate::year(as.Date(submission_date)),
      Age = ifelse(birth_year==-999,NA,as.numeric(Submission_year)-as.numeric(gsub("[^0-9]", "", birth_year))), #change to be based on months/days as well as year??
      Ethnicity = demog_ethnicity_1_1,
      ethnicity_group = case_when(
        demog_ethnicity_1_1==1 ~ "White British",
        demog_ethnicity_1_1 %in% c(2,3,4,5) ~ "White Other",
        demog_ethnicity_1_1 %in% c(6,7,8,9)~ "Mixed",
        demog_ethnicity_1_1 %in% c(10,11,12,14) ~ "Asian or Asian British",
        demog_ethnicity_1_1 %in% c(15,16,17) ~ "Black or Black British",
        demog_ethnicity_1_1 %in% c(13,18,19) ~ "Other"
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

# prospective variables ----------------------------------------------------
derive_prospective_vars <- function(data) {
  data <- data %>%
    mutate(
      submission_date=as.Date(submission_date),
      first_asthma_date=as.Date(first_asthma_date),
      asthma_beforebaseline=(first_asthma_date < submission_date ),
      status = 1-as.numeric(is.na(asthma_beforebaseline)),
      followuptime=ifelse(status==0,endoffollowup-submission_date,first_asthma_date-submission_date)
    )
  data
}


# filter out missing data -------------------------------------------------

filter_missing_data <- function(data, vars) {
  
  # values to remove
  bad_values <- c("Prefer not to answer", "Do not know")
  if("Surv(followuptime, status)"%in% vars){
    vars[vars=="Surv(followuptime, status)"]<-"status"
    vars <- c("followuptime",vars)
  }
  data %>%
    # keep only Male/Female if Sex exists
    { if("Sex" %in% names(.)) filter(., Sex %in% c("Male","Female")) else . } %>%
    
    # remove NA or bad values for all vars
    filter(
      if_all(all_of(vars), ~ !is.na(.) & !(. %in% bad_values))
    )
}
