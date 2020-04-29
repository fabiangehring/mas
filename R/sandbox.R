
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(pbmcapply)
  library(tidyr)
  library(ggplot2)
  library(data.table)
  library(dtplyr)
  library(arrow)
  library(microbenchmark)
  library(profvis)
})

Rcpp::sourceCpp('src/bootstrap_nn_idx.cpp')
source("R/03-analysis.R")

nn_idx <- arrow::read_arrow("data/nn_idx_eucl_olhc_w3_38a896430298c738055505dc89e042ac.feather") %>% as.matrix()

quotes_line <- arrow::read_feather("tmp/quotes_line.feather")

size_map <- quotes_line %>%
  select(Date) %>%
  group_by(Date) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(size = cumsum(count) - count) %>% 
  select(-count)

size <- quotes_line %>% select(Date) %>% left_join(size_map, by = "Date") %>% .[["size"]]

set.seed(123456)
seeds <- sample(1:9999999, 2)
boot_nn_idx <- pbmcapply::pbmclapply(seeds, function(seed) bootstrap_nn_idx(nn_idx, size, 20, seed), mc.cores = 3)
