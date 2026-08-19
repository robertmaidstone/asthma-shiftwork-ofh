library(tidyverse)
library(patchwork)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW.xlsx")

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SW_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SW_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SW_m3.png",width=5,height=4)

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb.xlsx")

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_m1.png",width=3.75,height=2)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_m2.png",width=3.75,height=2)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_m3.png",width=3.75,height=2)

#prospective

tt <- plot_OR("~/OurFutureHealth/OFHresults/P_SW.xlsx",y_lab_text = "Adjusted hazard ratio\nof asthma",p_val_loc = c(4.3,.8))

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/P_SW_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/P_SW_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/P_SW_m3.png",width=5,height=4)

tt <- plot_OR("~/OurFutureHealth/OFHresults/P_SWb.xlsx",p_val_loc=c(2.3,1.1))

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/P_SWb_m1.png",width=3.75,height=2)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/P_SWb_m2.png",width=3.75,height=2)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/P_SWb_m3.png",width=3.75,height=2)
#income binary

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_LT18.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_income_LT18.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_18-30.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_income_18-30.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_31-52.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_income_31-52.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_53-100.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_income_53-100.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_GT100.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_income_GT100.png",width=3.75,height=2)

#income binary mod 3

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_LT18.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_incomem3_LT18.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_18-30.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_incomem3_18-30.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_31-52.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_incomem3_31-52.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_53-100.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_incomem3_53-100.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_income_GT100.xlsx",y_lim = c(0.6,1.5))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_incomem3_GT100.png",width=3.75,height=2)

#man labour binary
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_manlabour_NR.xlsx",y_lim = c(0.8,1.3))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_manlabour_NR.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_manlabour_O.xlsx",y_lim = c(0.8,1.3))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_manlabour_O.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_manlabour_A.xlsx",y_lim = c(0.8,1.3))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_manlabour_A.png",width=3.75,height=2)


# Age binary --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q1.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_age_1_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q2.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_age_2_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q3.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_age_3_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q4.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_age_4_m3.png",width=3.75,height=2)

# Age binary m2 --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q1.xlsx")
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_age_1_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q2.xlsx")
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_age_2_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q3.xlsx")
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_age_3_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q4.xlsx")
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_age_4_m2.png",width=3.75,height=2)

# Ethnicity =- binary shift work --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_A.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_A_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Black.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_Black_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Chinese.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_Chinese_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Mixed.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_Mixed_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Other.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_Other_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_WB.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_WB_m3.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_WO.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_WO_m3.png",width=3.75,height=2)



# Ethnicity =- binary shift work m2 --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_A.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_A_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Black.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_Black_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Chinese.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_Chinese_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Mixed.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_Mixed_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Other.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_Other_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_WB.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_WB_m2.png",width=3.75,height=2)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_WO.xlsx",y_lim=c(0.25,2))
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots_thorax/CS_SWb_eth_WO_m2.png",width=3.75,height=2)



