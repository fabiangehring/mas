
hash_list <- tibble::tribble(
  ~file, ~ending, ~hash, 
  "quotes_raw", "feather", "776f84d00775909a851387455da30e8f",
  "stocks_raw", "feather", "1dc8906b62813ac4ef5b5e6d1e2a3eb1",
  
  "quotes_clean", "feather", "7f3e895442813405f2a27de8d3b7b5b8"
)


check_hashes <- function() {
  data_files <- list.files("data")
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
