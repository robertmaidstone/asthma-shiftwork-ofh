library(tidyverse)
library(patchwork)

source("~/OurFutureHealth/ofh-shiftwork/plotting_functions.R")


tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW.xlsx")

tt[[1]] + tt[[2]] + tt[[3]]


tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_age_q1.xlsx")

tt[[1]] + tt[[2]] + tt[[3]]


tt <- plot_OR("~/OurFutureHealth/OFHresults/CS_SW_manlabour_O.xlsx")

tt[[1]] + tt[[2]] + tt[[3]]
