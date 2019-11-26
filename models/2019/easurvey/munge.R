DRAFT_NUM <- 7

csv_path <- "data/2019/2019-ea-survey-anon-currencied.csv"
tryCatch({
  data <- readr::read_csv(csv_path)
}, error = function(e) {
  stop(csv_path, " could not be loaded. You may need to run the `ea_id.R` ",
       "script first. Note that only EA Survey team members have this data, ",
       "and that the public data is already the data produced by this script.",
       " Running this script is not necessary to do the analysis.")
})

message(NROW(data), " results before dropping insincere")
data <- dplyr::filter(data, grepl("Yes", sincere))
message(NROW(data), " results after dropping insincere")

message(length(na.omit(data$is_ea1)), " answered EA I question")
data <- data[data$is_ea1 == "Yes" & !is.na(data$is_ea1), ]
message(NROW(data), " after dropping non-EA I")

message(length(na.omit(data$is_ea2)), " answered EA II question")
data <- data[data$is_ea2 == "Yes" & !is.na(data$is_ea2), ]
message(NROW(data), " after dropping non-EA II")

message("GWWC Year")
data$gwwc_year <- as.numeric(sub("[^0-9]", "", data$gwwc_year))
data$gwwc_year <- ifelse(data$gwwc_year < 2009, NA, data$gwwc_year)
data$gwwc_year <- ifelse(data$gwwc_year > 2019, NA, data$gwwc_year)
data$member_gwwc <- (data$member_gwwc == "Year I took the pledge:")

message("% income donated")
p <- data$donate_2018_c / data$income_2018_c
p[is.infinite(p)] <- NA
p[is.nan(p)] <- NA
data$p_donate_2018 <- p

message("binary cause import")
for (variable in get_vars(data, "cause_import")) {
  data[[paste0(variable, "_b")]] <- swap_by_value(data, variable,
                                    list("5" = ":)",
                                         "4" = ":)",
                                         "3" = ":(",
                                         "2" = ":(",
                                         "1" = ":("))[[variable]]
}


# I know gender is not a binary, but this is still useful for analysis. My apologies.
message("can gender be binary?")
data$gender_b <- drop_values(data$gender, c("Other", "Prefer Not to Answer"))

message("veg binary")
data$veg_b <- data$veg %in% c("Vegan", "Vegetarian")

message("orgs by cause")
orgs_by_cause <- list("meta" = c("RC", "80K", "CFAR", "CEA", "EF", "ea_fund_meta"),
                      "cause_pri" = c("ACE", "GW"),
                      "poverty" = c("AMF", "DTW", "GD", "SCI", "ea_fund_global_health"),
                      "animal_welfare" = c("GF", "MFA", "THL", "ea_fund_animal_welfare"),
                      "far_future" = c("MIRI", "ea_fund_ltf"))
for (cause in names(orgs_by_cause)) {
  out <- paste(orgs_by_cause[[cause]], "2018", "c", sep = "_") %>%
           get_vars(data, ., ignore.case = TRUE) %/>%
           first %:>%
           Negate(is.na) %/>%
           (function(x) { data[[x]] }) %_>%
           fn(x, y, nas_are_zeros(x) + nas_are_zeros(y))
  data[[paste("donate", "cause", cause, "2018", "c", sep = "_")]] <- out
}

message("student")
data$student <- ifelse(is.na(data$employed_student_part), ifelse(is.na(data$employed_student_full), FALSE, TRUE), TRUE)

message("clean binary vars")
vars_to_clean <- c("race", "employed", "studied", "involved", "member", "activity",
                   "done_80K", "80K_coach_applied", "ea_career", "religion",
                   "experience", "ea_barrier", "retention", "drift_reason") 
vars_to_clean <- lapply(vars_to_clean, function(v) get_vars(data, v)) %>% flatten
vars_to_clean <- setdiff(vars_to_clean, "member_gwwc")
for (var in vars_to_clean) {
  if (length(unique(data[[var]])) == 2) {
    data[[var]] <- ifelse(is.na(data[[var]]), FALSE, TRUE)
  }
}

message("clean favor")
data$ea_know_favor <- gsub("-", " to ", data$ea_know_favor)

message("clean DPE")
data$dpe2 <- as.numeric(data$dpe2)

message("Writing INTERNAL draft")
readr::write_csv(data, paste0("data/2019/2019-ea-survey-INTERNAL-draft", DRAFT_NUM, ".csv"))
message("...Written")

message("Censoring... location...")
data$city <- NULL
ok_countries <- c("United States of America", "United Kingdom of Great Britain and Northern Ireland", "Germany", "Australia", "Canada", "Switzerland", "Netherlands", "Sweden")
data$country <- ifelse(data$country %in% ok_countries, data$country, "Other")

message("Censoring... age...")
data$age <- 2019 - as.numeric(gsub("[^a-zA-Z0-9!-.,?/ ]", "", data$birth_year))
data$age <- ifelse(data$age <= 0, NA, data$age)
data$age <- ifelse(data$age >= 100, NA, data$age)
data$birth_year <- NULL
data$age <- ifelse(data$age %within% c(13, 17), "13-17",
                       ifelse(data$age %within% c(18, 24), "18-24",
                       ifelse(data$age %within% c(25, 34), "25-34",
                       ifelse(data$age %within% c(34, 44), "34-44",
                       ifelse(data$age %within% c(45, 54), "45-54",
                       ifelse(data$age %within% c(55, 64), "55-64", "65+"))))))

message("Censoring... income...")
data$income_2018 <- NULL
data$donate_2018 <- NULL
data$income_2018_c <- as.numeric(data$income_2018_c)
data$income_2018_c <- ifelse(data$income_2018_c %within% c(0, 10000), "$0 to 10,000",
                        ifelse(data$income_2018_c %within% c(10001, 30000), "$10,001 to $30,000",
                        ifelse(data$income_2018_c %within% c(30001, 50000), "$30,001 to $50,000",
                        ifelse(data$income_2018_c %within% c(50001, 70000), "$50,001 to $70,000",
                        ifelse(data$income_2018_c %within% c(70001, 90000), "$70,001 to $90,000",
                        ifelse(data$income_2018_c %within% c(90001, 125000), "$90,001 to $125,000",
                        ifelse(data$income_2018_c %within% c(125001, 200000), "$125,001 to $200,000",
                               "Over $200,000")))))))

message("Censoring... donations...")
data$donate_2018_c <- as.numeric(data$donate_2018_c)
data$donate_2018_c <- ifelse(data$donate_2018_c %within% c(0, 100), "$0 to 100",
                        ifelse(data$donate_2018_c %within% c(101, 1000), "$101 to $1,000",
                        ifelse(data$donate_2018_c %within% c(1001, 5000), "$1,001 to $5,000",
                        ifelse(data$donate_2018_c %within% c(5001, 10000), "$5,001 to $10,000",
                        ifelse(data$donate_2018_c %within% c(10001, 30000), "$10,001 to $30,000",
                        ifelse(data$donate_2018_c %within% c(30001, 50000), "$30,001 to $50,000",
                        ifelse(data$donate_2018_c %within% c(50001, 100000), "$50,001 to $100,000",
                        ifelse(data$donate_2018_c %within% c(100001, 200000), "$100,001 to $200,000",
                               "Over $200,000"))))))))

message("Writing PUBLIC draft")
readr::write_csv(data, paste0("data/2019/2019-ea-survey-PUBLIC-draft", DRAFT_NUM, ".csv"))
message("...Written!")
