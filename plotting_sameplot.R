library(tidyverse)
library(patchwork)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")
# Age binary --------------------------------------------------------------

plot_OR_together(
  files=list("~/OurFutureHealth/OFHresults/CS_SWb_age_q1.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_age_q2.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_age_q3.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_age_q4.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_WB.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_WO.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_Mixed.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_A.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_Black.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_Other.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_LT18.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_18-30.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_31-52.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_53-100.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_GT100.xlsx"
             ),
  labels=c("18-37",
           "38-48",
           "49-57",
           "58-93",
           "White British",
           "White Other",
           "Mixed",
           "Asian",
           "Black",
           "Other",
           "< £18,000",
           "£18,000 - £30,999",
           "£31,000 - £51,999",
           "£52,000 - £100,000",
           "> £100,000"),
  groups=c(rep("Age",4),rep("Ethnicity",6),rep("Household Income",5)),
  y_lim = c(0.2,2.5)
) -> p_together


p_together
ggsave(plot = p_together[[2]], filename="~/OurFutureHealth/ofh-shiftwork/plots/Fig3together_m2.png",width=7,height=10)


####


plot_OR_together(
  files=list("~/OurFutureHealth/OFHresults/CS_SWb_age_q1.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_age_q2.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_age_q3.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_age_q4.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_WB.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_WO.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_Mixed.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_A.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_Black.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_eth_Other.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_LT18.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_18-30.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_31-52.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_53-100.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_income_GT100.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_manlabour_NR.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_manlabour_O.xlsx",
             "~/OurFutureHealth/OFHresults/CS_SWb_manlabour_A.xlsx"
  ),
  labels=c("18-37",
           "38-48",
           "49-57",
           "58-93",
           "White British",
           "White Other",
           "Mixed",
           "Asian",
           "Black",
           "Other",
           "< £18,000",
           "£18,000 - £30,999",
           "£31,000 - £51,999",
           "£52,000 - £100,000",
           "> £100,000",
           "Never/rarely",
           "Occasional",
           "Always"),
  groups=c(rep("Age",4),rep("Ethnicity",6),rep("Household Income",5),rep("Manual Labour",3)),
  y_lim = c(0.2,2.5)
) -> p_together


p_together
ggsave(plot = p_together[[3]], filename="~/OurFutureHealth/ofh-shiftwork/plots/SupFig2together_m3.png",width=7,height=10*(18.5/15.5))
