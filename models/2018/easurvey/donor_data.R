#TODO REDO!
Ramd::define("variable_names", function(variable_names) {
  options("stringsAsFactors" = FALSE)
  CONVERSION_AS_OF_DATE <- "2017-08-05" # A single date for currency conversions.
  message("Processing...")
  tryCatch({
    data2017_ <- c("data/2017/ea-survey-2017-confidential-no-anon.csv",
                   "data/2017/ea-survey-2017-donations-only-confidential-no-anon.csv") %/>%
                   function(x) { suppressWarnings(readr::read_csv(x)) }
  }, error = function(e) {
    stop("Loading 2017 data did not work. This script compiles private donor data that ",
         "is not intended to be run by the public. While the script is public, the data ",
         " is not and only members of the EA Survey team have the files needed to run ",
         "this script.")
  })
  data2017 <- data2017_ %/>% function(df) {
     suppressWarnings(suppressMessages(plyr::rename(df, variable_names))) }
  data2017 <- data2017 %_>% plyr::rbind.fill
  hash_email <- function(email, salt) {
    if (is.na(email) || identical(email, "")) { NA }
    else { digest::digest(paste0(email, salt)) }
  }
  email_salt_file <- file("data/email_salt.txt")
  email_salt <- readLines(email_salt_file)
  close(email_salt_file)
  data2017$ea_id <- data2017$email_address %/>% (function(x) hash_email(x, email_salt)) %>% unlist
  data2017 <- plyr::rename(data2017, list("Can_we_share_your_name_with_the_Centre_For_Effective_Altruism_and_80,000_Hours_to_help_them_with_their_impact_evaluation_and_research?" = "can_share",
                                          "Are_you_happy_to_include_details_of_your_donations_on_the_EA_Donation_Registry?_[Donations_(2015)]" = "can_share_2015_donations",
                                          "Are_you_happy_to_include_details_of_your_donations_on_the_EA_Donation_Registry?_[Donations_(2016)]" = "can_share_2016_donations",
                                          "Would_you_like_to_publish_your_donation_and_demographic_answers_on_a_personal EA_Profile?_You_will_have_the_opportunity_to_edit_the_page_to_hide_any_information_you_would_not_like_others_to_see." = "can_share_ea_profile",
                                          "First_Name" = "first_name",
                                          "Last_Name" = "last_name",
                                          "Are_you_sure_you_don't_want_to_give_your_name?_Please_enter_it_here_if_you'd_like._[First]" = "first_name2",
                                          "Are_you_sure_you_don't_want_to_give_your_name?_Please_enter_it_here_if_you'd_like._[Last]" = "last_name2"))
  data2017$full_name <- ifelse(is.na(data2017$first_name),
                               ifelse(is.na(data2017$first_name2), NA, paste(data2017$first_name2, data2017$last_name2)),
                               paste(data2017$first_name, data2017$last_name))
  data2017$full_name <- ifelse(data2017$full_name == " " | data2017$full_name == "", NA, data2017$full_name)
  data2017_sharable <- data2017 %>% filter(can_share_2015_donations == "Yes") %>%
                                    filter(can_share_2016_donations == "Yes") %>%
                                    filter(can_share_ea_profile %in% c("Yes - please create an EA Profile for me.", "I already have one.") | can_share == "Yes") %>%
                                    filter(can_share != "No") %>%
                                    filter(can_share_ea_profile != "No - please don't create an EA profile for me.")
  data2017_sharable <- data2017_sharable[, intersect(names(data2017_sharable),
                                                     c(unname(unlist(variable_names)),
                                                       "can_share",
                                                       "can_share_2015_donations",
                                                       "can_share_2016_donations",
                                                       "can_share_ea_profile",
                                                       "full_name"))]
  convert_money <- resource("lib/convert_money")
  data2017_sharable <- convert_money(data2017_sharable, CONVERSION_AS_OF_DATE)
  message("Writing out...")
  data2017_sharable$id <- NULL
  data2017_sharable$ea_org_comment <- NULL
  readr::write_csv(data2017_sharable, "data/2017/2017-ea-survey-sharable-data.csv")
  message("Written...")
})
