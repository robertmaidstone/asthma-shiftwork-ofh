library(tidyverse)
library(patchwork)
library(writexl)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")

#income binary

LRdata<-read.csv("~/OurFutureHealth/OFHresults/ERJresub/LRresults_SW_asthma.csv")

LRdata %>% separate(analysis,sep = "_",into=c("analysis","SW","model")) %>%filter(SW=="SWb") %>%
  dplyr::select(Sex=Group,term,formatted,model) %>%
  #filter(term!="(Intercept)") %>%
  mutate(formatted=str_replace(formatted,pattern = "\\?",replacement = "-")) %>%
  mutate(model=str_replace(model,pattern = "m",replacement = "")) %>%
  pivot_wider(names_from = term,values_from = formatted) %>%
  mutate(vars="formatted") %>%
  mutate(sex_interaction=0.04) %>% 
  dplyr::select(Sex,vars,`No shift work`=`(Intercept)`,`Shift work`=`Shift_work_bShift work`,sex_interaction,model) %>%
  write_xlsx("~/OurFutureHealth/OFHresults/ERJresub/SWb_csvout.xlsx")


tt <- plot_OR("~/OurFutureHealth/OFHresults/ERJresub/SWb_csvout.xlsx",y_lim = c(0.6,1.5))
tt
#ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_income_LT18.png",width=3.75,height=2)

LRdata %>% separate(analysis,sep = "_",into=c("analysis","SW","model")) %>%filter(SW=="SW") %>%
  dplyr::select(Sex=Group,term,formatted,model) %>%
  #filter(term!="(Intercept)") %>%
  mutate(formatted=str_replace(formatted,pattern = "\\?",replacement = "-")) %>%
  mutate(model=str_replace(model,pattern = "m",replacement = "")) %>%
  pivot_wider(names_from = term,values_from = formatted) %>%
  mutate(vars="formatted") %>%
  mutate(sex_interaction=0.04) %>% 
  dplyr::select(Sex,vars,`No shift work`=`(Intercept)`,
                `Never/rarely`=`Shift_workNever/rarely`,
                `Sometimes`=`Shift_workSometimes`,
                `Always`=`Shift_workAlways`,
                sex_interaction,model) %>%
  write_xlsx("~/OurFutureHealth/OFHresults/ERJresub/SW_csvout.xlsx")


tt <- plot_OR("~/OurFutureHealth/OFHresults/ERJresub/SW_csvout.xlsx",y_lim = c(0.6,1.5))
tt


