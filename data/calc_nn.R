
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(pbmcapply)
  library(tidyr)
  library(ggplot2)
})

source("R/03-analysis.R")

  
quotes_line <- readRDS("tmp/quotes_line.rds")

data_wide_0 <- quotes_line %>%
  group_by(Ticker) %>%
  widen(window = 3, cols = c("Open", "Low", "High", "Close"), include_current = TRUE)

na_rows_bool <- rowSums(is.na(data_wide_0)) > 0

knn_data <- data_wide_0 %>%
  mutate(Date = quotes_line$Date) %>%
  mutate_all(~ifelse(na_rows_bool, NA, .))

nn_data <- select(knn_data, !c(ends_with("_0")))
rm(knn_data)      
gc()

nn_data_hash <- digest::digest(nn_data)

nn_eucl <- find_nn(select(nn_data, -Date), dates = nn_data$Date, norm = "euclidean")
file_nn_data_eucl <- here::here(paste0("data/nn_eucl_olhc_w3_", nn_data_hash,".rds"))
saveRDS(nn_eucl, file_nn_data_eucl)

nn_manh <- find_nn(select(nn_data, -Date), dates = nn_data$Date, norm = "manhattan")
file_nn_data_manh <- here::here(paste0("data/nn_manh_olhc_w3_", nn_data_hash,".rds"))
saveRDS(nn_manh, file_nn_data_manh)
