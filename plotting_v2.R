library(tidyverse)
library(patchwork)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")


tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW.xlsx")

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_m3.png",width=5,height=4)

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_fullmodels.xlsx")
ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_fulldata_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_fulldata_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_fulldata_m3.png",width=5,height=4)

#prospective

tt <- plot_OR("~/OurFutureHealth/OFHresults/P_SW.xlsx")

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SW_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SW_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SW_m3.png",width=5,height=4)

tt <- plot_OR("~/OurFutureHealth/OFHresults/P_SWb.xlsx")

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SWb_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SWb_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SWb_m3.png",width=5,height=4)
#prospective full data

tt <- plot_OR("~/OurFutureHealth/OFHresults/P_SW_fullmodels.xlsx")

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SW_fulldata_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SW_fulldata_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SW_fulldata_m3.png",width=5,height=4)

tt <- plot_OR("~/OurFutureHealth/OFHresults/P_SWb_fullmodels.xlsx")

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SWb_fulldata_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SWb_fulldata_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/P_SWb_fulldata_m3.png",width=5,height=4)



# model 2 vars ------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_model2vars.xlsx")

p_c <- (tt[[1]]+ ggtitle("Model 1")+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank()) + tt[[2]] + theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.text.y = element_blank(),axis.ticks.y=element_blank()) + ggtitle("Model 1 + alcohol status") + tt[[3]] + theme(axis.text.y = element_blank(),axis.ticks.y =element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+ ggtitle("Model 1 + days exercised"))/ 
  (tt[[4]]+ ggtitle("Model 1 + length of working week")  + tt[[5]] + theme(axis.text.y = element_blank(),axis.ticks=element_blank()) + ggtitle("Model 1 + chronotype") + tt[[6]] + theme(axis.text.y = element_blank(),axis.ticks=element_blank())+ ggtitle("Model 1 + household income"))
ggsave(plot = p_c, filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_model2vars.png",width=12,height=7)


# model 3 vars ------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_model3vars.xlsx")

p_c <- (tt[[1]]+ ggtitle("Model 2")+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank()) + tt[[2]] + theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.text.y = element_blank(),axis.ticks.y=element_blank()) + ggtitle("Model 2 + BMI") + tt[[3]] + theme(axis.text.y = element_blank(),axis.ticks.y =element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+ ggtitle("Model 2 + smoking variables"))/ 
  (tt[[4]]+ ggtitle("Model 2 + sleep duration")  + tt[[5]] + theme(axis.text.y = element_blank(),axis.ticks=element_blank()) + ggtitle("Model 2 + vaping")+plot_spacer())
ggsave(plot = p_c, filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_model3vars.png",width=12,height=7)




# jobs --------------------------------------------------------------------
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_jobvars.xlsx")

p_c <- (tt[[1]]+ ggtitle("Model 3")+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank()) + tt[[2]] + theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.text.y = element_blank(),axis.ticks.y=element_blank()) + ggtitle("Model 3 + manual labour") + tt[[3]] + theme(axis.text.y = element_blank(),axis.ticks.y =element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+ ggtitle("Model 3 + years in current job"))/ 
  (tt[[4]]+ ggtitle("Model 3 + walk or stand at work")  + tt[[5]] + theme(axis.text.y = element_blank(),axis.ticks=element_blank()) + ggtitle("Model 3 + all job variables")+plot_spacer())
ggsave(plot = p_c, filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_jobvars.png",width=12,height=7)

#income

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_income_LT18.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_income_LT18.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_income_18-30.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_income_18-30.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_income_31-52.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_income_31-52.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_income_53-100.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_income_53-100.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_income_GT100.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_income_GT100.png",width=5,height=4)

#man labour
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_manlabour_NR.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_manlabour_NR.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_manlabour_O.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_manlabour_O.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_manlabour_A.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_manlabour_A.png",width=5,height=4)


# Chronotype --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_CT_E.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_CT_E_m3.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_CT_I.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_CT_I_m3.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_CT_M.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_CT_M_m3.png",width=5,height=4)

tt <- plot_OR_UKB_CT("~/OurFutureHealth/OFHresults/UKB_CS_SW_CT_E.xlsx")
ggsave(plot = tt$Morning, filename="OurFutureHealth/ofh-shiftwork/plots/UKB_CS_SW_CT_M.png",width=5,height=4)
ggsave(plot = tt$Intermediate, filename="OurFutureHealth/ofh-shiftwork/plots/UKB_CS_SW_CT_I.png",width=5,height=4)
ggsave(plot = tt$Evening, filename="OurFutureHealth/ofh-shiftwork/plots/UKB_CS_SW_CT_E.png",width=5,height=4)

# Age --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q1.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_age_1_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q2.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_age_2_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q3.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_age_3_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q4.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_age_4_m3.png",width=3.75,height=4)
# Age binary --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q1.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_age_1_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q2.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_age_2_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q3.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_age_3_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_age_q4.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_age_4_m3.png",width=3.75,height=4)


# Ethnicity --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_eth_A.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_eth_A_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_eth_Black.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_eth_Black_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_eth_Chinese.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_eth_Chinese_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_eth_Mixed.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_eth_Mixed_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_eth_Other.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_eth_Other_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_eth_WB.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_eth_WB_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_eth_WO.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_eth_WO_m3.png",width=3.75,height=4)

# Ethnicity =- binary shift work --------------------------------------------------------------

tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_A.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_eth_A_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Black.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_eth_Black_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Chinese.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_eth_Chinese_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Mixed.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_eth_Mixed_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_Other.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_eth_Other_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_WB.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_eth_WB_m3.png",width=3.75,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SWb_eth_WO.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SWb_eth_WO_m3.png",width=3.75,height=4)
