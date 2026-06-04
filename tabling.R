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

# Export to Word
read_docx() %>% 
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/P_SW.xlsx")) %>%
  body_add_break() %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW.xlsx")) %>%
  body_add_break() %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW_model2vars.xlsx",model_labels = model2vars_labels)) %>%
  print(target = "OurFutureHealth/raw_ft_tables.docx")
