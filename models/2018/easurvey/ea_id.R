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
  
  message("Censoring... email...")
  hash_email <- function(email, salt) {
    if (is.na(email) || identical(email, "")) { NA }
    else { digest::digest(paste0(email, salt)) }
  }
  email_salt_file <- file("data/email_salt.txt")
  email_salt <- readLines(email_salt_file)
  close(email_salt_file)
  data2018$ea_id <- lapply(data2018$email_address, hash_email, salt = email_salt) %>% unlist
  data2018$email_address <- NULL

  message("Censoring... name...")
  data2018$name <- NULL

  message("Censoring... location...")
  data2018$city <- NULL
  ok_countries <- c("United States of America", "United Kingdom of Great Britain and Northern Ireland", "Germany", "Australia", "Canada", "Switzerland", "Netherlands", "Sweden")
  data2018$country <- ifelse(data2018$country %in% ok_countries, data2018$country, "Other")

  message("Censoring... age...")
  data2018$age <- 2018 - as.numeric(gsub("[^a-zA-Z0-9!-.,?/ ]", "", data2018$birth_year))
  data2018$age <- ifelse(data2018$age <= 0, NA, data2018$age)
  data2018$age <- ifelse(data2018$age >= 100, NA, data2018$age)
  data2018$birth_year <- NULL
  data2018$age <- ifelse(data2018$age %within% c(13, 17), "13-17",
                         ifelse(data2018$age %within% c(18, 24), "18-24",
                         ifelse(data2018$age %within% c(25, 34), "25-34",
                         ifelse(data2018$age %within% c(45, 54), "45-54",
                         ifelse(data2018$age %within% c(55, 64), "55-64", "65+")))))

  message("Writing comments...")
  write_comments <- resource("lib/write_comments")
  write_comments(data2018, "data/2018/2018-survey-comments.txt")
  message("Written...")
  message("Censoring... comments...")
  censored_cols <- 7
  for (var in get_vars(data2018, "comment")) {
    data2018[[var]] <- NULL
    censored_cols <- censored_cols + 1
  }
  data2018$why_donate_less <- NULL
  censored_cols <- censored_cols + 1

  message("Censoring... based on share preferences...")
  share_count <- apply(apply(data2018[, get_vars(data2018, "no_share")], 2, is.na), 1, sum)
  share_vars <- setdiff(get_vars(data2018, "share"), get_vars(data2018, "no_share"))
  no_share_count <- apply(apply(data2018[, share_vars], 2, is.na), 1, sum)
  drop_me <- (no_share_count - max(no_share_count) - share_count == 0)
  original_dim <- nrow(data2018)
  data2018 <- data2018[!drop_me, ]
  message("Censored ", original_dim - nrow(data2018), " rows and ", censored_cols, " columns due to privacy...")

  message("Writing out...")
  readr::write_csv(data2018, "data/2018/2018-ea-survey-anon.csv")
  message("Written...")
})
