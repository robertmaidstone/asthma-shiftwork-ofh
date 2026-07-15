library(tidyverse)
library(patchwork)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")

#income binary

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_LT18.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_income_LT18.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_18-30.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_income_18-30.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_31-52.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_income_31-52.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_53-100.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_income_53-100.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_GT100.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_income_GT100.png",width=3.75,height=2)

#man labour binary
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_manlabour_NR.xlsx",y_lim = c(0.8,1.3))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_manlabour_NR.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_manlabour_O.xlsx",y_lim = c(0.8,1.3))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_manlabour_O.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_manlabour_A.xlsx",y_lim = c(0.8,1.3))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_manlabour_A.png",width=3.75,height=2)

# Age --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q1.xlsx",y_lim=c(0.3,1.8))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SW_age_1_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q2.xlsx",y_lim=c(0.3,1.8))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SW_age_2_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q3.xlsx",y_lim=c(0.3,1.8))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SW_age_3_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q4.xlsx",y_lim=c(0.3,1.8))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SW_age_4_m3.png",width=3.75,height=4)

# Age binary --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q1.xlsx")
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_age_1_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q2.xlsx")
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_age_2_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q3.xlsx")
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_age_3_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q4.xlsx")
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_age_4_m2.png",width=3.75,height=2)



# Ethnicity =- binary shift work --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_A.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_eth_A_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Black.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_eth_Black_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Chinese.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_eth_Chinese_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Mixed.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_eth_Mixed_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Other.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_eth_Other_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_WB.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_eth_WB_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_WO.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_m2/CS_SWb_eth_WO_m3.png",width=3.75,height=2)





