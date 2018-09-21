options("stringsAsFactors" = FALSE)
CONVERSION_AS_OF_DATE <- "2018-08-05" # A single date for currency conversions.

csv_path <- "data/2018/2018-ea-survey-anon.csv"
tryCatch({
  data <- readr::read_csv(csv_path)
}, error = function(e) {
  stop(csv_path, " could not be loaded. You may need to run the `ea_id.R` ",
       "script first. Note that only EA Survey team members have this data, ",
       "and that the public data is already the data produced by this script.",
       " Running this script is not necessary to do the analysis.")
})

convert_money <- resource("lib/convert_money")
data <- convert_money(data, CONVERSION_AS_OF_DATE)
readr::write_csv(data, "data/2018/2018-ea-survey-anon-currencied.csv")
message("...Written!")
