
hash_list <- tibble::tribble(
  ~file, ~ending, ~hash, 
  "quotes_raw", "feather", "776f84d00775909a851387455da30e8f",
  "stocks_raw", "feather", "1dc8906b62813ac4ef5b5e6d1e2a3eb1",
  
  "quotes_clean", "feather", "e7d78a2e9a7bfe796b90d809c46929c5",

  "data_wide_3", "feather", "9d8e0d252b437f29d7b770a3cdd60c9b",
  "data_wide_3_all", "feather", "34c059cb3975b94349ee933ebc62dd71",
)


check_hashes <- function() {
  data_files <- setdiff(list.files("data"), list.dirs("data", full.names = FALSE))
  if (!isTRUE(all.equal(sort(data_files), sort(paste0(hash_list$file, ".", hash_list$ending))))) {
    stop("Actual files and expected files are not the same.")
  }
  
  counter <- 1
  for (curr_file in data_files) {
    message(paste0("Processing: ", curr_file, " (", counter, "/", length(data_files), ")"))

    curr_name <- tools::file_path_sans_ext(curr_file)
    
    if (tolower(get_ending(curr_name)) == "rds") {
      obj <- readRDS(paste0("data/", curr_file))
    } else if (tolower(get_ending(curr_name)) == "feather") {
      obj <- arrow::read_feather(paste0("data/", curr_file))
    } else {
      stop(paste0("Unknown file ending for ", curr_file))
    }
    
    hash_actual <- digest::digest(obj)
    has_expected <- get_hash(curr_name)
    
    if (hash_actual != has_expected) {
      stop(paste0("Unexpected hash for file ", curr_file))
    }
    counter <- counter + 1
  }
  message("Check of hashes successful.")
}

get_ending <- function(x) {
  dplyr::filter(hash_list, file == x)$ending
}

get_hash <- function(x) {
  dplyr::filter(hash_list, file == x)$hash
}

# check_hashes()
