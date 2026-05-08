library(tidyverse)
library(patchwork)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")


tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW.xlsx")

tt[[1]] + tt[[2]] + tt[[3]]

ggsave(plot = tt[[1]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_m1.png",width=5,height=4)
ggsave(plot = tt[[2]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_m2.png",width=5,height=4)
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_m3.png",width=5,height=4)


tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_CT_E.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_CT_E_m3.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_CT_I.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_CT_I_m3.png",width=5,height=4)
tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_CT_M.xlsx")
ggsave(plot = tt[[3]], filename="OurFutureHealth/ofh-shiftwork/plots/CS_SW_CT_M_m3.png",width=5,height=4)



tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_manlabour_O.xlsx")

tt[[1]] + tt[[2]] + tt[[3]]
