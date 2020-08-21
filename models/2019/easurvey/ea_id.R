Ramd::define("variable_names", function(variable_names) {
  message("Processing...")
  tryCatch({
    data2019_ <- readr::read_csv("data/2019/2019-ea-survey-confidential-no-anon.csv")
  }, error = function(e) {
    stop("Loading 2019 data did not work. Note that this requires a confidential data ",
         "file to be installed in the `data/2019` folder. This file is something only ",
         "members of the EA Survey team would have access to. However, the 2019 CSV ",
         "produced by this script is already available, so you do not need to be ",
         "able to run this script to do analysis.")
  })
  last_i <- 0
  for (i in seq_along(names(data2019_))) {
    if (is.na(names(data2019_)[[i]])) {
      names(data2019_)[[i]] <- paste(names(data2019_)[[last_i]], data2019_[[1, i]])
    } else {
      last_i <- i
    }
  }
	duplicated_definitions <- names(variable_names)[which(duplicated(names(variable_names)))]
  if (length(duplicated_definitions) > 0) {
    stop("Error: duplicate definition of variables -- ",
      paste0(duplicated_definitions, collapse = ", "))
  }
	duplicated_resolutions <- unname(variable_names)[which(duplicated(unname(variable_names)))]
  if (length(duplicated_resolutions) > 0) {
    stop("Error: duplicate resolution of variable names -- ",
      paste0(duplicated_resolutions, collapse = ", "))
  }
	duplicated_names <- names(data2019_)[which(duplicated(names(data2019_)))]
  if (length(duplicated_names) > 0) {
    stop("Error: duplicate names in dataset -- ",
      paste0(duplicated_names, collapse = ", "))
  }
	unmatched_names <- unname(unlist(variable_names[which(!(names(variable_names) %in% names(data2019_)))]))
  if (length(unmatched_names) > 0) {
    stop("Error: the following variables don't resolve -- ",
      paste0(unmatched_names, collapse = ", "))
  }
  data2019 <- data2019_[-1]
  data2019 <- plyr::rename(data2019, variable_names)
	no_resolution <- setdiff(unlist(unname(variable_names)), names(data2019))
  if (length(no_resolution) > 0) {
    stop("Error: the following variables don't resolve -- ",
      paste0(no_resolution, collapse = ", "))
  }
	no_definition <- setdiff(names(data2019), unlist(unname(variable_names)))
  if (length(no_definition) > 0) {
    warning("Warning: ", length(no_definition), " items in the dataset did not get a variable assignment.")
  }
  data2019 <- data2019[, unlist(setdiff(variable_names, no_resolution))]

  message("Writing out...")
  readr::write_csv(data2019, "data/2019/2019-ea-survey-confidential-no-anon-processed.csv")
  
  message("Censoring... email...")
  hash_email <- function(email, salt) {
    if (is.na(email) || identical(email, "")) { NA }
    else { digest::digest(paste0(email, salt)) }
  }
  email_salt_file <- file("data/email_salt.txt")
  email_salt <- readLines(email_salt_file)
  close(email_salt_file)
  data2019$ea_id <- lapply(data2019$email_address, hash_email, salt = email_salt) %>% unlist

  message("Writing out...")
  readr::write_csv(data2019, "data/2019/2019-ea-survey-anon.csv")
  message("Written...")
})
