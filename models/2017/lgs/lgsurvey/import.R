Ramd::define("variable_names", function(variable_names) {
  message("Processing...")
  tryCatch({
    lgs_ <- readr::read_csv("data/2017/2017-local-groups-survey-confidential-no-anon.csv")
  }, error = function(e) {
    stop("Local Group 2017 data did not work. Note that this data is not publicly available ",
         "and needs to be provided to you by a member of the EA Survey team.")
  })
  lgs <- lgs_
  for (name in variable_names) {
    if (!(identical(as.character(names(lgs)[[name$id]]), as.character(name$value)))) {
      stop("Expected to convert ", sQuote(name$value), " to ", sQuote(name$name),
           " at id ", name$id, " but got ", sQuote(names(lgs)[[name$id]]), " instead.")
    }
    names(lgs)[[name$id]] <- name$name
  }

  # Drop unnecessary variables
  lgs <- lgs[, unlist(lapply(variable_names, `[[`, "name"))]

  # Remove first row (garbled)
  lgs <- lgs[seq(2, nrow(lgs)), ]

  lgs$group_name <- ifelse(is.na_like(lgs$group_name), lgs$group_name2, lgs$group_name)
  lgs$group_name2 <- NULL

  lgs$has_group_email <- !is.na_like(lgs$has_group_email)
  lgs$want_group_email <- ifelse(lgs$has_group_email,
                                 ifelse(is.na_like(lgs$want_group_email), "No", "Yes"),
                                 "Already has one")
  lgs$has_website <- !is.na_like(lgs$has_website)
  lgs$has_meetup <- !is.na_like(lgs$has_meetup)
  lgs$has_active_eas <- !is.na_like(lgs$has_active_eas)

  message("Writing out...")
  readr::write_csv(lgs, "data/2017/2017-local-groups-survey-parsed.csv")
  message("Written...")
})
