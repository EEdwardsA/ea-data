Ramd::define("variable_names", function(variable_names) {
  message("Processing...")
  tryCatch({
    data2018_ <- readr::read_csv("data/2018/2018-ea-survey-confidential-no-anon.csv")
  }, error = function(e) {
    stop("Loading 2018 data did not work. Note that this requires a confidential data ",
         "file to be installed in the `data/2018` folder. This file is something only ",
         "members of the EA Survey team would have access to. However, the 2018 CSV ",
         "produced by this script is already available, so you do not need to be ",
         "able to run this script to do analysis.")
  })
  data2018 <- plyr::rename(data2018_, variable_names)
  names_that_did_not_work <- setdiff(unlist(unname(variable_names)), names(data2018))
  if (length(names_that_did_not_work) > 0) {
    stop("Error: some variables did not import -- ",
      paste0(names_that_did_not_work, collapse = ", "))
  }
  data2018 <- data2018[, unlist(variable_names)]
  hash_email <- function(email, salt) {
    if (is.na(email) || identical(email, "")) { NA }
    else { digest::digest(paste0(email, salt)) }
  }
  email_salt_file <- file("data/email_salt.txt")
  email_salt <- readLines(email_salt_file)
  close(email_salt_file)
  data2018$ea_id <- lapply(data2018$email_address, hash_email, salt = email_salt) %>% unlist
  data2018$name <- NULL
  data2018$email_address <- NULL
  message("Writing out...")
  readr::write_csv(data2018, "data/2018/2018-ea-survey-anon.csv")
  message("Written...")
})
