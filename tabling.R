library(tidyverse)
library(readxl)
library(janitor)
library(flextable)
library(officer)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")

make_shiftwork_table("OurFutureHealth/OFHresults/P_SW.xlsx")
make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW.xlsx")

# Export to Word
read_docx() %>% 
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/P_SW.xlsx")) %>%
  body_add_flextable(make_shiftwork_table("OurFutureHealth/OFHresults/CS_SW.xlsx")) %>%
  print(target = "OurFutureHealth/raw_ft_tables.docx")
