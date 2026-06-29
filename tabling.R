library(tidyverse)
library(readxl)
library(janitor)
library(flextable)
library(officer)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")

make_shiftwork_table("OurFutureHealth/OFHresults/P_SW.xlsx")
make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW.xlsx")


model2vars_labels <- c(
  "2" = "Model 2",
  "2+Income_cat" = "Model 2 + Income",
  "2+alcoholstatus" = "Model 2 + Alcohol consumption",
  "2+chronotype" = "Model 2 + Chronotype",
  "2+daysexercisedvars" = "Model 2 + Days exercised",
  "2+lengthWW" = "Model 2 + Length of working week"
)
make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_model2vars.xlsx",model_labels = model2vars_labels)

model3vars_labels <- c(
  "3" = "Model 3",
  "3+BMI" = "Model 3 + BMI",
  "3+smokingvars" = "Model 3 + Smoking variables",
  "3+sleepdur" = "Model 3 + Sleep duration",
  "3+vaping" = "Model 3 + Vaping"
)
make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_model3vars.xlsx",model_labels = model3vars_labels)

make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_age_q1.xlsx")

make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_eth_WB.xlsx")

# Export to Word
read_docx() %>% 
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/P_SW.xlsx")) %>%
  body_add_break() %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW.xlsx")) %>%
  body_add_break() %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_model2vars.xlsx",model_labels = model2vars_labels)) %>%
  body_add_break() %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_model3vars.xlsx",model_labels = model3vars_labels)) %>%
  body_add_break() %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_age_q1.xlsx")) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_age_q2.xlsx")) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_age_q3.xlsx")) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_age_q4.xlsx")) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_eth_WB.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_eth_WO.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_eth_Other.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_eth_Mixed.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_eth_A.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_eth_Black.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_eth_Chinese.xlsx",SWb=TRUE)) %>%
  
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_income_LT18.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_income_18-30.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_income_31-52.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_income_53-100.xlsx",SWb=TRUE)) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SWb_income_GT100.xlsx",SWb=TRUE)) %>%
  print(target = "OurFutureHealth/raw_ft_tables.docx")
