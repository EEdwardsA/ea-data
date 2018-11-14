convert_money <- function(data, as_of) {

  first_pass <- data$currency_donate_1 %/>%
                  fn(x, strsplit(x, " - ")[[1]]) %/>%
                  fn(x, if (identical(x, "- US Dollar")) { "USD" } else { x }) %/>%
                  fn(x, if (length(x) >= 1) { x[[1]] } else { NA }) %/>%
                  function(cc) {
                    if (is.na(cc)) { NA_character_ }
                    else if (cc %in% currencyr::currency_codes()) { cc }
                    else { NA_character_ }
                  }

  split_currency <- function(currency, sep) {
    strsplit(currency, sep)[[c(1, 1)]] %>% trimws
  }
  clean_currency <- function(currency) {
    currency <- toupper(gsub("[^a-zA-Z0-9!-.,?/ ]", "", currency))
    if (grepl("-", currency)) { split_currency(currency, "-") }
    else if (grepl("\\(", currency)) { split_currency(currency, "\\(") }
    else { currency }
  }
  try_to_get_currency <- function(currency) {
    if (!checkr::is.simple_string(currency)) { return(NA_character_) }
    if (currency %in% currencyr::currency_codes()) { return(currency) }
    attempt <- currencyr::get_code_from_unit(currency)
    if (checkr::is.simple_string(attempt)) { return(attempt) }
    return(NA_character_)
  }

  second_pass <- data$currency_donate_2 %/>% clean_currency %/>% try_to_get_currency
  combine <- function(x, y) { if (is.na(x)) { y } else { x }}
  currency_map <- Vectorize(combine)(first_pass, second_pass)
  data$currency <- currency_map

  currency_vars <- surveytools2::get_vars(data, c("donate_", "income"), collapse = TRUE)
  currency_vars <- setdiff(currency_vars, c("why_donate_less", "currency_donate_1", "currency_donate_2"))

  is_number_string <- function(x) { !is.na(suppressWarnings(as.numeric(x))) }

  to_usd <- function(num, current_currency) {
    if (is.na(num)) { return(as.numeric(NA)) }
    if (is.na(current_currency)) { return(as.numeric(NA)) }
    if (identical(as.numeric(num), 0)) { return(0) }
    if (identical(current_currency, "USD")) { return(as.numeric(num)) }
    currencyr::convert(as.numeric(num),
                       from = current_currency,
                       as_of = as_of)$value
  }

  get_numbers <- function(str) {
    # Go character by character until you find a number, then take numbers until
    # you don't have one, then stop.
    #
    # get_numbers("hello") -> NA 
    # get_numbers("16") -> 16
    # get_numbers("3,000") -> 3000
    # get_numbers("I donated 16") -> 16
    # get_numbers("3,000 - 4,000") -> 3000
    str <- strsplit(str, "")[[1]]
    new_number <- ""
    started <- FALSE
    for (letter in str) {
      if (letter %in% strsplit("0123456789,", "")[[1]]) {
        if (letter != ",") {
          new_number <- paste0(new_number, letter)
          started <- TRUE
        }
      } else {
        if (started) { break }
      }
    }
    as.numeric(new_number)
  }


  for (var in currency_vars) {
    message("...Processing ", var)
    values <- unlist(lapply(data[[var]], function(num) suppressWarnings(get_numbers(num))))
    data[[paste0(var, "_c")]] <- unlist(Map(to_usd, values, currency_map))
  }
  data
}
