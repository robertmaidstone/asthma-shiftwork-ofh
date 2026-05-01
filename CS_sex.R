library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(survival)

source("dataprocessing.r")
source("functions.r")

data_m<-load_and_merge("part_data.csv",
                       "asthma_fo.csv",
                       "quest_data.csv",
                       "clinic_data.csv",
                       id="pid")

endoffollowup <- as.Date("2025-07-31")

data <- data_m %>%
  derive_core_demographic_vars() %>%
  derive_asthma_vars() %>%
  derive_smoking_vars() %>%
  derive_shiftwork_vars() %>%
  derive_contraception_vars() %>% 
  derive_exercise_vars() %>%
  derive_clinical_vars() %>%
  derive_job_vars() %>%
  derive_sleep_vars() %>%
  derive_alcohol_vars() %>%
  derive_environment_vars() %>%
  derive_comorbid_vars() %>%
  derive_prospective_vars()

vars_to_clean <- c(
  "Asthma_med2",
  "Shift_work", "Age", "Ethnicity",
  "Alcohol_status", "Days_walk", "Days_mod", "Days_vig","LengthWW", "Chronotype", "Income",
  "BMI","smoking_status_cond","sleep_dur","regular_vapers"
)

data_f<- data %>% filter_missing_data(vars= vars_to_clean)

dim(data)
dim(data_f)

models<-c("Asthma_med2 ~ Shift_work + Age + ethnicity_group",
          "Asthma_med2 ~ Shift_work + Age + ethnicity_group + Alcohol_status + Days_walk + Days_mod + Days_vig + LengthWW + Chronotype + Income_cat",
          "Asthma_med2 ~ Shift_work + Age + ethnicity_group + Alcohol_status + Days_walk + Days_mod + Days_vig + LengthWW + Chronotype + Income_cat + BMI + smoking_status_cond + packyears_clean_combined_0 + sleep_dur + regular_vapers"
)

shift_vars <- c(
  "Never/rarely"="Shift_workNever/rarely",
  "Sometimes"="Shift_workSometimes",
  "Always"="Shift_workAlways"
)

for(i in 1:3){
  
  var_vec <- strsplit(models[i],
                      split = "\\+|~")[[1]] |> trimws()
  #data_f2<- data %>% filter_missing_data(vars= var_vec)
  
  mod_male<- or_table_df(data_f,
                         models[i],
                         data_f$Sex=="Male")
  
  csv_male_out <- csv_table_out(mod_male,models[i],data_f[data_f$Sex=="Male",],"Male","Shift_work")
  
  mod_female<- or_table_df(data_f,
                           models[i],
                           data_f$Sex=="Female")
  
  csv_female_out <- csv_table_out(mod_female,models[i],data_f[data_f$Sex=="Female",],"Female","Shift_work")    
  
  OR_sex_int(data_f,models[i],"Shift_work")-> p_val
  print(p_val)
  t_1 <- or_table_out(data_f,mod_male,mod_female,shift_vars,p_val) 
  
  p_1 <- plot_or(mod_male,mod_female,var_names= rev(shift_vars), group_names=c("Male","Female"))
  
  assign(paste("p_mod_",i,sep=""),p_1)
  assign(paste("t_mod_",i,sep=""),t_1)
  assign(paste("csv_out_",i,sep=""),rbind(csv_male_out,csv_female_out))
  
  rbind(cbind(csv_out_1,analysis="CS_SW_m1"),
        cbind(csv_out_2,analysis="CS_SW_m2"),
        cbind(csv_out_3,analysis="CS_SW_m3")) -> csv_master
  
  ###
  
  source("functions.r")
  
  shift_vars <- c(
    "Shift work"="Shift_work_bShift work"
  )
  
  models<-c("Asthma_med2 ~ Shift_work_b + Age + ethnicity_group",
            "Asthma_med2 ~ Shift_work_b + Age + ethnicity_group + Alcohol_status + Days_walk + Days_mod + Days_vig + LengthWW + Chronotype + Income_cat",
            "Asthma_med2 ~ Shift_work_b + Age + ethnicity_group + Alcohol_status + Days_walk + Days_mod + Days_vig + LengthWW + Chronotype + Income_cat + BMI + smoking_status_cond + packyears_clean_combined_0 + sleep_dur + regular_vapers"
  )
  
  for(i in 1:3){
    
    var_vec <- strsplit(models[i],
                        split = "\\+|~")[[1]] |> trimws()
    data_f2<- data %>% filter_missing_data(vars= var_vec)
    
    mod_male<- or_table_df(data_f2,
                           models[i],
                           data_f2$Sex=="Male")
    
    csv_male_out <- csv_table_out(mod_male,models[i],data_f2[data_f2$Sex=="Male",],"Male","Shift_work_b")
    
    mod_female<- or_table_df(data_f2,
                             models[i],
                             data_f2$Sex=="Female")
    
    csv_female_out <- csv_table_out(mod_female,models[i],data_f2[data_f2$Sex=="Female",],"Female","Shift_work_b")  
    
    OR_sex_int(data_f2,models[i],"Shift_work_b")-> p_val
    
    t_1 <- or_table_out(data_f2,mod_male,mod_female,shift_vars,p_val) 
    
    p_1 <- plot_or(mod_male,mod_female,var_names= rev(shift_vars), group_names=c("Male","Female"))
    
    assign(paste("p_mod_",i,sep=""),p_1)
    assign(paste("t_mod_",i,sep=""),t_1)
    assign(paste("csv_out_",i,sep=""),rbind(csv_male_out,csv_female_out))
  }
}