
library(ggplot2)
system('dx download "part_data.csv"')
system('dx download "APCdata.csv"')

read.csv("part_data.csv") -> data_part
read.csv("APCdata.csv") -> data_apc

data_apc[!(is.na(as.Date(data_apc$admidate)) | as.Date(data_apc$admidate)<as.Date("1900-01-01")),] -> data_apc

dplyr::mutate(.data = data_apc, Asthma_APC = dplyr::if_any(everything(), ~ grepl("J45|J46", .x))) -> data_temp

data_temp$admidate <- as.Date(data_temp$admidate)

first_asthma <- data_temp |>
  dplyr::filter(Asthma_APC) |>            # keep only asthma rows
  dplyr::group_by(pid) |>
  dplyr::summarise(first_asthma_date = min(admidate), .groups = "drop") |>
  dplyr::mutate(asthma=T)

data_temp |>  
  dplyr::group_by(pid) |>
  dplyr::summarise(asthma = any(Asthma_APC), .groups = "drop") |>
  dplyr::filter(!asthma) |>
  dplyr::select(pid) |> unique() |>
  dplyr::mutate(asthma=F,first_asthma_date=NA)-> first_asthma_noasthma

rbind(first_asthma,first_asthma_noasthma) |> write.csv( "asthma_fo.csv", row.names = FALSE)

