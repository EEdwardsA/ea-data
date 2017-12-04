Ramd::define("referrers", "simple_referrers", "cities",
             function(referrer_list, simple_referrer_list, city_swap_list) {
  list(
    import = list(file = "data/2017/imsurvey2017-anonymized-currencied.csv"),

    data   = list(
      "Drop insincere"   = function(df) {
          message(NROW(df), " results before dropping insincere")
          df <- dplyr::filter(df, grepl("Yes", sincere))
          message(NROW(df), " results after dropping insincere")
          df
      }
      , "Drop non-EA"    = function(df) {
          message(length(na.omit(df$is_ea)), " answered EA question")
          df <- dplyr::filter(df, grepl("Yes", is_ea))
          message(NROW(df), " after before dropping non-EA")
          df
      }
      , "Make age"     = function(df) {
          df$age <- as.character(2017 - as.numeric(df$birth_year))
          df <- swap_by_value(df, "age", list("0" = NA, "2" = NA))
          df$age <- as.numeric(df$age)
          df
      }
      , "Clean up city"  = function(df) {
        swap_by_value(df, "city", city_swap_list)
      }
      , "Clean up country" = function(df) {
        swap_list <- list("USA" = "United States",
                          "UK" = "United Kingdom",
                          "Hong Kong SAR China" = "China",
                          "Question_time:_F4" = NA)
        swap_by_value(df, "country", swap_list)
      }
      , "Make % inc donate [2015]" = function(df) {
          p <- df$donate_2015_c / df$income_2015_individual_c
          p[is.infinite(p)] <- NA
          df$p_donate_2015 <- p
          df
      }
      , "Make % inc donate [2016]" = function(df) {
          p <- df$donate_2016_c / df$income_2016_individual_c
          p[is.infinite(p)] <- NA
          df$p_donate_2016 <- p
          df
      }
      , "Make referrer_url" = function(df) {
          df$referrer2 <- surveytools2::swap_by_value(df, "referrer_url", referrer_list, grep = TRUE)$referrer_url
          df$referrer2[is.na(df$referrer2)] <- "Missing"
          df$referrer2[!(df$referrer2 %in% unlist(unname(referrer_list)))] <- "Other"
          df
      }
      , "Make simple_referrer" = function(df) {
         df$referrer3 <- surveytools2::swap_by_value(df, "referrer_url", simple_referrer_list, grep = TRUE)$referrer_url
         df$referrer3[is.na(df$referrer3)] <- "Missing"
         df$referrer3[!(df$referrer3 %in% unlist(unname(simple_referrer_list)))] <- "Other"
         df
      }
      , "Make binary cause breakdown" = function(df) {
				for (variable in get_vars(df, "cause_import")) {
					df[[paste0(variable, "_b")]] <- swap_by_value(df, variable,
																						list("I do not think any EA resources should be devoted to this cause" = ":(",
																								 "I do not think this is a priority, but I am glad some EAs are looking into it" = ":(",
																								 "Not Considered / Not Sure" = ":(",
																								 "This cause deserves significant investment but less than the top priorities" = ":(",
																								 "This cause should be a near-top priority" = ":)",
																								 "This cause should be the top priority" = ":)"))[[variable]]
        }
        df
      }
      , "Make number of priorities" = function(df) {
        cause_vars <- grep("_b", get_vars(df, "cause_import"), invert = TRUE, value = TRUE)
        df$num_top_cause_priorities <- count_vars(df, cause_vars, "This cause should be the top priority")$value
        df
      }
      , "Make gender binary" = function(df) {
          # I know gender is not a binary, but this is still useful for analysis. My apologies.
          df$gender_b <- drop_values(df$gender, c("Other", "Prefer Not to Answer"))
          df
        }
      , "Make diet (veg*n vs. other) binary" = function(df) {
          df$veg_b <- df$veg %in% c("Vegan", "Vegetarian")
          df
        }
      , "Politics (left vs. non-left)" = function(df) {
          df$left <- swap_by_value(df, "politics", list("Centre Left" = "Left",
                                                        "Left" = "Left",
                                                        "Other" = NA,
                                                        "Libertarian" = "Non-Left",
                                                        "Centre" = "Non-Left",
                                                        "Prefer Not to Answer" = NA,
                                                        "Centre Right" = "Non-Left",
                                                        "Right" = "Non-Left"))$politics
          df
        }
      , "Donations by cause area" = function(df) {
					orgs_by_cause <- list("meta" = c("RC", "80K", "CFAR", "CEA", "CS", "EF", "TLYCS"),
																"cause_pri" = c("ACE", "FRI", "GW"),
																"poverty" = c("AMF", "DTW", "END", "GD", "MC", "SCI", "Sightsavers"),
																"animal_welfare" = c("faunalytics", "GF", "MFA", "SP", "THL"),
																"far_future" = c("FHI", "MIRI"))

					for (year in c("2015", "2016")) {
						for (cause in names(orgs_by_cause)) {
							out <- paste(orgs_by_cause[[cause]], year, "c", sep = "_") %>%
											 get_vars(df, ., ignore.case = TRUE) %/>%
											 first %/>%
											 (function(x) { df[[x]] }) %_>%
                       fn(x, y, nas_are_zeros(x) + nas_are_zeros(y))
							df[[paste("donate", "cause", cause, year, "c", sep = "_")]] <- out
						}
					}
					df
        }
      , "Write and drop comments" = function(df) {
        write_comments <- resource("lib/write_comments")
        write_comments(df, "data/2017/2017-survey-comments.txt")
        for (var in get_vars(df, "comment")) {
          df[[var]] <- NULL
        }
        df
      }
    )

    , analyze = list(
      list(write = "data/2017/2017-survey-analysis-tables.txt"),
      # list(write = "stdout"), # <-- toggle this to print to the screen.
      list(
        "cause_import_animal_welfare"          = function(df) tab(df, cause_import_animal_welfare)
        , "cause_import_cause_prioritization"  = function(df) tab(df, cause_import_cause_prioritization)
        , "cause_import_environmentalism"      = function(df) tab(df, cause_import_environmentalism)
        , "cause_import_ai"                    = function(df) tab(df, cause_import_ai)
        , "cause_import_non_ai_far_future"     = function(df) tab(df, cause_import_non_ai_far_future)
        , "cause_import_poverty"               = function(df) tab(df, cause_import_poverty)
        , "cause_import_rationality"           = function(df) tab(df, cause_import_rationality)
        , "cause_import_politics"              = function(df) tab(df, cause_import_politics)
        , "cause_import_meta"                  = function(df) tab(df, cause_import_meta)
				, "binary cause view"                  = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, var)) } }
        , "causes for only people with one priority" = function(df) {
                                                  for (var in grep("_b", get_vars(df, "cause_import"), invert = TRUE, value = TRUE)) {
                                                    print(tab_(filter(df, num_top_cause_priorities == 1), var))
                                                  }}
				, "binary cause view x gender"         = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, list("gender_b", var), freq = FALSE, percent = TRUE)) } }
				, "binary cause view x diet"           = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, list("veg_b", var), freq = FALSE, percent = TRUE)) } }
        , "diet x cause_import_animal_welfare" = function(df) ctab(df, veg, cause_import_animal_welfare, na.rm = TRUE)
        , "diet x cause_import_animal_welfare 2"    = function(df) tab(df, veg %in% c("Vegan", "Vegetarian"), cause_import_animal_welfare_b, na.rm = TRUE, percent = TRUE)
        , "city x cause area"                       = function(df) {
                                                        top_ten_cities <- tab(df, city, top = 10) %>% names %>% .[-1]
                                                        for (city in top_ten_cities) {
                                                          message(city)
                                                          for (var in get_vars(df, "cause_import.+_b")) {
                                                            df %>% dplyr::filter_(paste0("city == '", city, "'")) %>% tab_(., var) %>% print(.)
                                                          }
                                                        }
                                                      }
        , "Bay Area x AI"                           = function(df) ctab(df, cause_import_ai_b, city == "SF Bay", na.rm = TRUE)
        , "AI by joining 2013 or earlier"           = function(df) tab(df, cause_import_ai_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), percent = TRUE, na.rm = TRUE, byrow = FALSE)
        , "AI by joining 2013 or earlier II"        = function(df) tab(df, cause_import_ai_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), percent = TRUE, byrow = FALSE)
        , "AI by joining 2013 or earlier III"       = function(df) ctab(df, cause_import_ai_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), na.rm = TRUE)
        , "AI by joining 2012 or earlier"           = function(df) ctab(df, cause_import_ai_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012"), na.rm = TRUE)
        , "AI by joining 2011 or earlier"           = function(df) ctab(df, cause_import_ai_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011"), na.rm = TRUE)
        , "AI by joining 2014 or earlier"           = function(df) ctab(df, cause_import_ai_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013", "2014"), na.rm = TRUE)
        , "Poverty by joining 2013 or earlier"      = function(df) tab(df, cause_import_poverty_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), percent = TRUE, na.rm = TRUE, byrow = FALSE)
        , "Poverty by joining 2013 or earlier II"   = function(df) tab(df, cause_import_poverty_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), percent = TRUE, byrow = FALSE)
        , "Poverty by joining 2013 or earlier III"  = function(df) ctab(df, cause_import_poverty_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), na.rm = TRUE)
        , "Poverty by joining 2012 or earlier"      = function(df) ctab(df, cause_import_poverty_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012"), na.rm = TRUE)
        , "Poverty by joining 2011 or earlier"      = function(df) ctab(df, cause_import_poverty_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011"), na.rm = TRUE)
        , "Poverty by joining 2014 or earlier"      = function(df) ctab(df, cause_import_poverty_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013", "2014"), na.rm = TRUE)
        , "Poverty by joining 2015 or earlier"      = function(df) tab(df, cause_import_poverty_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), percent = TRUE, na.rm = TRUE, byrow = FALSE)
        , "AR by joining 2013 or earlier"           = function(df) tab(df, cause_import_animal_welfare_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), percent = TRUE, na.rm = TRUE, byrow = FALSE)
        , "AR by joining 2013 or earlier II"         = function(df) tab(df, cause_import_animal_welfare_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), percent = TRUE, byrow = FALSE)
        , "AR by joining 2013 or earlier III"        = function(df) ctab(df, cause_import_animal_welfare_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), na.rm = TRUE)
        , "Politics by joining 2013 or earlier"     = function(df) tab(df, cause_import_politics_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), percent = TRUE, na.rm = TRUE, byrow = FALSE)
        , "Politics by joining 2013 or earlier II"   = function(df) tab(df, cause_import_politics_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), percent = TRUE, byrow = FALSE)
        , "Politics by joining 2013 or earlier III"  = function(df) ctab(df, cause_import_politics_b, which_year_EA %in%  c("Before 2009", "2009", "2010", "2011", "2012", "2013"), na.rm = TRUE)
        , "summarize donations 2015"                = function(df) var_summary(df$donate_2015_c, verbose = TRUE)
        , "summarize donations 2016"                = function(df) var_summary(df$donate_2016_c, verbose = TRUE)
        , "donate in 2015 and 2016?"                = function(df) tab(df, donate_2015_c > 0, donate_2016_c > 0)
        , "info on donating in both years?"         = function(df) tab(df, !is.na(donate_2015_c), !is.na(donate_2016_c))
        , "median 2015 -> 2016 $ donate increase"   = function(df) { df %>% dplyr::filter(donate_2015_c > 0, donate_2016_c > 0) %>% select(donate_2015_c, donate_2016_c) %>% mutate(increase = donate_2016_c - donate_2015_c) %>% summarise(median(increase)) }
        , "median 2015 -> 2016 income increase"     = function(df) { df %>% dplyr::filter(income_2015_individual_c > 0, income_2016_individual_c > 0) %>% select(income_2015_individual_c, income_2016_individual_c) %>% mutate(increase = income_2016_individual_c - income_2015_individual_c) %>% summarise(median(increase)) }
        , "median 2015 -> 2016 % donate increase"   = function(df) { df %>% dplyr::filter(p_donate_2015 > 0, p_donate_2016 > 0) %>% select(p_donate_2015, p_donate_2016) %>% mutate(increase = p_donate_2016 - p_donate_2015) %>% summarise(median(increase)) }
        , "mean 2015 -> 2016 $ donate increase"   = function(df) { df %>% dplyr::filter(donate_2015_c > 0, donate_2016_c > 0) %>% select(donate_2015_c, donate_2016_c) %>% mutate(increase = donate_2016_c - donate_2015_c) %>% summarise(mean(increase)) }
        , "mean 2015 -> 2016 income increase"     = function(df) { df %>% dplyr::filter(income_2015_individual_c > 0, income_2016_individual_c > 0) %>% select(income_2015_individual_c, income_2016_individual_c) %>% mutate(increase = income_2016_individual_c - income_2015_individual_c) %>% summarise(mean(increase)) }
        , "mean 2015 -> 2016 % donate increase"   = function(df) { df %>% dplyr::filter(p_donate_2015 > 0, p_donate_2016 > 0) %>% select(p_donate_2015, p_donate_2016) %>% mutate(increase = p_donate_2016 - p_donate_2015) %>% summarise(mean(increase)) }
        , "donations x student"                = function(df) ctab(df, donate_2016_c, student)
        , "summary donations for students"     = function(df) var_summary(dplyr::filter(df, student == "Yes")$donate_2016_c, verbose = TRUE)
        , "summary donations for non-students" = function(df) var_summary(dplyr::filter(df, student == "No")$donate_2016_c, verbose = TRUE)
        , "donation quantile 1"                = function(df) quantile(df$donate_2016_c, probs = seq(0.1, 1, len = 10), na.rm = TRUE)
        , "donation quantile 2"                = function(df) quantile(df$donate_2016_c, probs = seq(0.91, 1, len = 10), na.rm = TRUE)
        , "summarize % donations 2015"         = function(df) var_summary(dplyr::filter(df, income_2015_individual_c > 10000)$p_donate_2015 * 100, verbose = TRUE)
        , "summarize % donations 2016"         = function(df) var_summary(dplyr::filter(df, income_2016_individual_c > 10000)$p_donate_2016 * 100, verbose = TRUE)
        , "% donation quantile"                = function(df) quantile(dplyr::filter(df, income_2016_individual_c > 10000)$p_donate_2016 * 100, probs = seq(0.1, 1, len = 10), na.rm = TRUE)
        , "% donation breakdown"               = function(df) breakdown(dplyr::filter(df, income_2016_individual_c > 10000), "p_donate_2016", c(0.01, 0.02, 0.03, 0.05, 0.1, 0.15, seq(0.2, 0.9, by =  0.1)))
        , "RC Donations 2015 (Total)"          = function(df) var_summary(df$donate_RC_2015_c, verbose = TRUE)
        , "RC Donations 2016 (Total)"          = function(df) var_summary(df$donate_RC_2016_c, verbose = TRUE)
        , "Did donate to RC 2015?"             = function(df) tab(df, donate_RC_2015_c > 0)
        , "Did donate to RC 2016?"             = function(df) tab(df, donate_RC_2016_c > 0)
        , "80K Donations 2015 (Total)"         = function(df) var_summary(df$donate_80K_2015_c, verbose = TRUE)
        , "80K Donations 2016 (Total)"         = function(df) var_summary(df$donate_80K_2016_c, verbose = TRUE)
        , "Did donate to 80K 2015?"            = function(df) tab(df, donate_80K_2015_c > 0)
        , "Did donate to 80K 2016?"            = function(df) tab(df, donate_80K_2016_c > 0)
        , "ACE Donations 2015 (Total)"         = function(df) var_summary(df$donate_ace_2015_c, verbose = TRUE)
        , "ACE Donations 2016 (Total)"         = function(df) var_summary(df$donate_ace_2016_c, verbose = TRUE)
        , "Did donate to ACE 2015?"            = function(df) tab(df, donate_ace_2015_c > 0)
        , "Did donate to ACE 2016?"            = function(df) tab(df, donate_ace_2016_c > 0)
        , "AMF Donations 2015 (Total)"         = function(df) var_summary(df$donate_amf_2015_c, verbose = TRUE)
        , "AMF Donations 2016 (Total)"         = function(df) var_summary(df$donate_amf_2016_c, verbose = TRUE)
        , "Did donate to AMF 2015?"            = function(df) tab(df, donate_amf_2015_c > 0)
        , "Did donate to AMF 2016?"            = function(df) tab(df, donate_amf_2016_c > 0)
        , "CFAR Donations 2015 (Total)"        = function(df) var_summary(df$donate_cfar_2015_c, verbose = TRUE)
        , "CFAR Donations 2016 (Total)"        = function(df) var_summary(df$donate_cfar_2016_c, verbose = TRUE)
        , "Did donate to CFAR 2015?"           = function(df) tab(df, donate_cfar_2015_c > 0)
        , "Did donate to CFAR 2016?"           = function(df) tab(df, donate_cfar_2016_c > 0)
        , "CEA Donations 2015 (Total)"         = function(df) var_summary(df$donate_cea_2015_c, verbose = TRUE)
        , "CEA Donations 2016 (Total)"         = function(df) var_summary(df$donate_cea_2016_c, verbose = TRUE)
        , "Did donate to CEA 2015?"            = function(df) tab(df, donate_cea_2015_c > 0)
        , "Did donate to CEA 2016?"            = function(df) tab(df, donate_cea_2016_c > 0)
        , "CS Donations 2015 (Total)"          = function(df) var_summary(df$donate_cs_2015_c, verbose = TRUE)
        , "CS Donations 2016 (Total)"          = function(df) var_summary(df$donate_cs_2016_c, verbose = TRUE)
        , "Did donate to CS 2015?"             = function(df) tab(df, donate_cs_2015_c > 0)
        , "Did donate to CS 2016?"             = function(df) tab(df, donate_cs_2016_c > 0)
        , "DTW Donations 2015 (Total)"         = function(df) var_summary(df$donate_dtw_2015_c, verbose = TRUE)
        , "DTW Donations 2016 (Total)"         = function(df) var_summary(df$donate_dtw_2016_c, verbose = TRUE)
        , "Did donate to DTW 2015?"            = function(df) tab(df, donate_dtw_2015_c > 0)
        , "Did donate to DTW 2016?"            = function(df) tab(df, donate_dtw_2016_c > 0)
        , "EF Donations 2015 (Total)"          = function(df) var_summary(df$donate_ef_2015_c, verbose = TRUE)
        , "EF Donations 2016 (Total)"          = function(df) var_summary(df$donate_ef_2016_c, verbose = TRUE)
        , "Did donate to EF 2015?"             = function(df) tab(df, donate_ef_2015_c > 0)
        , "Did donate to EF 2016?"             = function(df) tab(df, donate_ef_2016_c > 0)
        , "END Fund Donations 2015 (Total)"    = function(df) var_summary(df$donate_end_2015_c, verbose = TRUE)
        , "END Fund Donations 2016 (Total)"    = function(df) var_summary(df$donate_end_2016_c, verbose = TRUE)
        , "Did donate to END Fund 2015?"       = function(df) tab(df, donate_end_2015_c > 0)
        , "Did donate to END Fund 2016?"       = function(df) tab(df, donate_end_2016_c > 0)
        , "Faunalytics Donations 2015 (Total)" = function(df) var_summary(df$donate_faunalytics_2015_c, verbose = TRUE)
        , "Faunalytics Donations 2016 (Total)" = function(df) var_summary(df$donate_faunalytics_2016_c, verbose = TRUE)
        , "Did donate to Faunalytics 2015?"    = function(df) tab(df, donate_faunalytics_2015_c > 0)
        , "Did donate to Faunalytics 2016?"    = function(df) tab(df, donate_faunalytics_2016_c > 0)
        , "FRI Donations 2015 (Total)"    = function(df) var_summary(df$donate_fri_2015_c, verbose = TRUE)
        , "FRI Donations 2016 (Total)"    = function(df) var_summary(df$donate_fri_2016_c, verbose = TRUE)
        , "Did donate to FRI 2015?"       = function(df) tab(df, donate_fri_2015_c > 0)
        , "Did donate to FRI 2016?"       = function(df) tab(df, donate_fri_2016_c > 0)
        , "FHI Donations 2015 (Total)"    = function(df) var_summary(df$donate_fhi_2015_c, verbose = TRUE)
        , "FHI Donations 2016 (Total)"    = function(df) var_summary(df$donate_fhi_2016_c, verbose = TRUE)
        , "Did donate to FHI 2015?"       = function(df) tab(df, donate_fhi_2015_c > 0)
        , "Did donate to FHI 2016?"       = function(df) tab(df, donate_fhi_2016_c > 0)
        , "GD Donations 2015 (Total)"     = function(df) var_summary(df$donate_gd_2015_c, verbose = TRUE)
        , "GD Donations 2016 (Total)"     = function(df) var_summary(df$donate_gd_2016_c, verbose = TRUE)
        , "Did donate to GD 2015?"        = function(df) tab(df, donate_gd_2015_c > 0)
        , "Did donate to GD 2016?"        = function(df) tab(df, donate_gd_2016_c > 0)
        , "GW Donations 2015 (Total)"     = function(df) var_summary(df$donate_gw_2015_c, verbose = TRUE)
        , "GW Donations 2016 (Total)"     = function(df) var_summary(df$donate_gw_2016_c, verbose = TRUE)
        , "Did donate to GW 2015?"        = function(df) tab(df, donate_gw_2015_c > 0)
        , "Did donate to GW 2016?"        = function(df) tab(df, donate_gw_2016_c > 0)
        , "GF Donations 2015 (Total)"     = function(df) var_summary(df$donate_gf_2015_c, verbose = TRUE)
        , "GF Donations 2016 (Total)"     = function(df) var_summary(df$donate_gf_2016_c, verbose = TRUE)
        , "Did donate to GF 2015?"        = function(df) tab(df, donate_gf_2015_c > 0)
        , "Did donate to GF 2016?"        = function(df) tab(df, donate_gf_2016_c > 0)
        , "MIRI Donations 2015 (Total)"   = function(df) var_summary(df$donate_miri_2015_c, verbose = TRUE)
        , "MIRI Donations 2016 (Total)"   = function(df) var_summary(df$donate_miri_2016_c, verbose = TRUE)
        , "Did donate to MIRI 2015?"      = function(df) tab(df, donate_miri_2015_c > 0)
        , "Did donate to MIRI 2016?"      = function(df) tab(df, donate_miri_2016_c > 0)
        , "MC Donations 2015 (Total)"     = function(df) var_summary(df$donate_mc_2015_c, verbose = TRUE)
        , "MC Donations 2016 (Total)"     = function(df) var_summary(df$donate_mc_2016_c, verbose = TRUE)
        , "Did donate to MC 2015?"        = function(df) tab(df, donate_mc_2015_c > 0)
        , "Did donate to MC 2016?"        = function(df) tab(df, donate_mc_2016_c > 0)
        , "MFA Donations 2015 (Total)"    = function(df) var_summary(df$donate_mfa_2015_c, verbose = TRUE)
        , "MFA Donations 2016 (Total)"    = function(df) var_summary(df$donate_mfa_2016_c, verbose = TRUE)
        , "Did donate to MFA 2015?"       = function(df) tab(df, donate_mfa_2015_c > 0)
        , "Did donate to MFA 2016?"       = function(df) tab(df, donate_mfa_2016_c > 0)
        , "SCI Donations 2015 (Total)"    = function(df) var_summary(df$donate_sci_2015_c, verbose = TRUE)
        , "SCI Donations 2016 (Total)"    = function(df) var_summary(df$donate_sci_2016_c, verbose = TRUE)
        , "Did donate to SCI 2015?"       = function(df) tab(df, donate_sci_2015_c > 0)
        , "Did donate to SCI 2016?"       = function(df) tab(df, donate_sci_2016_c > 0)
        , "Sightsavers Donations 2015 (Total)" = function(df) var_summary(df$donate_sightsavers_2015_c, verbose = TRUE)
        , "Sightsavers Donations 2016 (Total)" = function(df) var_summary(df$donate_sightsavers_2016_c, verbose = TRUE)
        , "Did donate to Sightsavers 2015?"    = function(df) tab(df, donate_sightsavers_2015_c > 0)
        , "Did donate to Sightsavers 2016?"    = function(df) tab(df, donate_sightsavers_2016_c > 0)
        , "SP Donations 2015 (Total)"     = function(df) var_summary(df$donate_sp_2015_c, verbose = TRUE)
        , "SP Donations 2016 (Total)"     = function(df) var_summary(df$donate_sp_2016_c, verbose = TRUE)
        , "Did donate to SP 2015?"        = function(df) tab(df, donate_sp_2015_c > 0)
        , "Did donate to SP 2016?"        = function(df) tab(df, donate_sp_2016_c > 0)
        , "THL Donations 2015 (Total)"    = function(df) var_summary(df$donate_thl_2015_c, verbose = TRUE)
        , "THL Donations 2016 (Total)"    = function(df) var_summary(df$donate_thl_2016_c, verbose = TRUE)
        , "Did donate to THL 2015?"       = function(df) tab(df, donate_thl_2015_c > 0)
        , "Did donate to THL 2016?"       = function(df) tab(df, donate_thl_2016_c > 0)
        , "TLYCS Donations 2015 (Total)"  = function(df) var_summary(df$donate_tlycs_2015_c, verbose = TRUE)
        , "TLYCS Donations 2016 (Total)"  = function(df) var_summary(df$donate_tlycs_2016_c, verbose = TRUE)
        , "Did donate to TLYCS 2015?"     = function(df) tab(df, donate_tlycs_2015_c > 0)
        , "Did donate to TLYCS 2016?"     = function(df) tab(df, donate_tlycs_2016_c > 0)
        , "Meta Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_cause_meta_2015_c, verbose = TRUE)
        , "Meta Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_cause_meta_2016_c, verbose = TRUE)
        , "Did donate to Meta Cause 2015?"     = function(df) tab(df, donate_cause_meta_2015_c > 0)
        , "Did donate to Meta Cause 2016?"     = function(df) tab(df, donate_cause_meta_2016_c > 0)
        , "Global Poverty Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_cause_poverty_2015_c, verbose = TRUE)
        , "Global Poverty Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_cause_poverty_2016_c, verbose = TRUE)
        , "Did donate to Global Poverty Cause 2015?"     = function(df) tab(df, donate_cause_poverty_2015_c > 0)
        , "Did donate to Global Poverty Cause 2016?"     = function(df) tab(df, donate_cause_poverty_2016_c > 0)
        , "Cause Prioritization Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_cause_cause_pri_2015_c, verbose = TRUE)
        , "Cause Prioritization Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_cause_cause_pri_2016_c, verbose = TRUE)
        , "Did donate to Cause Prioritization Cause 2015?"     = function(df) tab(df, donate_cause_cause_pri_2015_c > 0)
        , "Did donate to Cause Prioritization Cause 2016?"     = function(df) tab(df, donate_cause_cause_pri_2016_c > 0)
        , "Animal Welfare Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_cause_animal_welfare_2015_c, verbose = TRUE)
        , "Animal Welfare Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_cause_animal_welfare_2016_c, verbose = TRUE)
        , "Did donate to Animal Welfare Cause 2015?"     = function(df) tab(df, donate_cause_animal_welfare_2015_c > 0)
        , "Did donate to Animal Welfare Cause 2016?"     = function(df) tab(df, donate_cause_animal_welfare_2016_c > 0)
        , "Far Future Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_cause_far_future_2015_c, verbose = TRUE)
        , "Far Future Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_cause_far_future_2016_c, verbose = TRUE)
        , "Did donate to Far Future Cause 2015?"     = function(df) tab(df, donate_cause_far_future_2015_c > 0)
        , "Did donate to Far Future Cause 2016?"     = function(df) tab(df, donate_cause_far_future_2016_c > 0)
        , "age"                           = function(df) tab(df, age)
        , "age plot"                      = function(df) { ggplot(df, aes(age)) + geom_histogram(color="black", fill="lightblue") + scale_x_continuous("Age") + ggtitle("Ages of EAs") }
        , "gender"                        = function(df) tab(df, gender)
        , "race_white"                    = function(df) tab(df, race_white)
        , "race_black"                    = function(df) tab(df, race_black)
        , "race_hispanic"                 = function(df) tab(df, race_hispanic)
        , "race_asian"                    = function(df) tab(df, race_asian)
        , "multi-racial"                  = function(df) table(get_vars(df, "race") %/>% fn(x, df[[x]]) %/>% fn(x, ifelse(x == "Yes", 1, 0)) %_>% fn(x, y, x + y))
        , "politics"                      = function(df) tab(df, politics)
        , "left"                          = function(df) tab(df, left)
				, "binary cause view x left"      = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(comparison_table_(df, var, "left", na.rm = TRUE)) } }
        , "left x race"                   = function(df) ctab(df, race_white, left, na.rm = TRUE)
        , "left x city"                   = function(df) ctab(df, left, city, top = 5, na.rm = TRUE)
        , "left x Bay"                    = function(df) ctab(df, left, city == "SF Bay", na.rm = TRUE)
        , "left x NYC"                    = function(df) ctab(df, left, city == "New York City", na.rm = TRUE)
        , "race x city"                   = function(df) ctab(df, race_white, city, top = 5, na.rm = TRUE)
        , "race x NYC"                    = function(df) ctab(df, race_white, city == "New York City", na.rm = TRUE)
        , "student"                       = function(df) tab(df, student, percent = TRUE)
        , "country"                       = function(df) tab(df, country)
        , "city"                          = function(df) tab(df, city)
        , "religion"                      = function(df) tab(df, religion)
        , "veg"                           = function(df) tab(df, veg)
        , "veg x left"                    = function(df) ctab(df, veg_b, left, na.rm = TRUE)
        , "moral philosophy"              = function(df) tab(df, moral_philosophy)
        , "opportunity or obligation"     = function(df) tab(df, ea_opportunity_or_obligation)
        , "employment_status"             = function(df) tab(df, employment_status)
        , "employment field"              = function(df) tab(df, field)
        , "studied_cs"                    = function(df) tab(df, studied_cs)
        , "studied_econ"                  = function(df) tab(df, studied_econ)
        , "studied_engineering"           = function(df) tab(df, studied_engineering)
        , "studied_maths"                 = function(df) tab(df, studied_maths)
        , "studied_medicine"              = function(df) tab(df, studied_medicine)
        , "studied_psych"                 = function(df) tab(df, studied_psych)
        , "studied_phil"                  = function(df) tab(df, studied_phil)
        , "studied_physics"               = function(df) tab(df, studied_physics)
        , "studied_humanities"            = function(df) tab(df, studied_humanities)
        , "studied_social_science"        = function(df) tab(df, studied_social_science)
        , "studied_other_science"         = function(df) tab(df, studied_other_science)
        , "ea_donate"                     = function(df) tab(df, ea_donate)
        , "ea_volunteer"                  = function(df) tab(df, ea_volunteer)
        , "ea_volunteer x non-profit"     = function(df) ctab(df, ea_volunteer, employment_status == "Employed by a non-profit organization", na.rm = TRUE)
        , "ea_volunteer x ETG"            = function(df) ctab(df, ea_volunteer, ea_career_type == "Earning to give", na.rm = TRUE)
        , "volunteer or non-profits x AI" = function(df) ctab(df, cause_import_ai_b, ea_volunteer == "Yes" | employment_status == "Employed by a non-profit organization", na.rm = TRUE)
        , "volunteer non-profits x poverty"     = function(df) ctab(df, cause_import_poverty_b, ea_volunteer == "Yes" | employment_status == "Employed by a non-profit organization", na.rm = TRUE)
        , "volunteer non-profits x AR"          = function(df) ctab(df, cause_import_animal_welfare_b, ea_volunteer == "Yes" | employment_status == "Employed by a non-profit organization", na.rm = TRUE)
        , "volunteer non-profits x environment" = function(df) ctab(df, cause_import_environmentalism_b, ea_volunteer == "Yes" | employment_status == "Employed by a non-profit organization", na.rm = TRUE)
        , "volunteer non-profits x cause pri"   = function(df) ctab(df, cause_import_cause_prioritization_b, ea_volunteer == "Yes" | employment_status == "Employed by a non-profit organization", na.rm = TRUE)
        , "volunteer non-profits x new to EA?"  = function(df) ctab(df, ea_volunteer == "Yes" | employment_status == "Employed by a non-profit organization", na.rm = TRUE, which_year_EA %in% c("2016", "2017"))
        , "volunteer non-profits x education"   = function(df) ctab(df, ea_volunteer == "Yes" | employment_status == "Employed by a non-profit organization", na.rm = TRUE, education)
        , "ea_career"                     = function(df) tab(df, ea_career)
        , "ea_career_type"                = function(df) tab(df, ea_career_type)
        , "ea_volunteer_hours"            = function(df) tab(df, ea_volunteer_hours)
        , "ea_paid_hours"                 = function(df) tab(df, ea_paid_hours)
        , "gender x poverty donations 2015" = function(df) ctab(df, filter(donate_2015_c > 0), donate_cause_poverty_2015_c, gender_b)
        , "gender x poverty donations 2016" = function(df) ctab(df, filter(donate_2016_c > 0), donate_cause_poverty_2016_c, gender_b)
        , "gender x AR donations 2016"    = function(df) ctab(df, filter(donate_2016_c > 0), donate_cause_animal_welfare_2016_c, gender_b)
        , "donations x ETG 2015"          = function(df) ctab(df, filter(student == "No"), donate_2015_c, ea_career_type == "Earning to give")
        , "donations x ETG 2016"          = function(df) ctab(df, filter(student == "No"), donate_2016_c, ea_career_type == "Earning to give")
        , "age x ETG"                     = function(df) ctab(df, age, ea_career_type == "Earning to give", na.rm = TRUE)
        , "total donations x ETG 2015"    = function(df) df %>% group_by(ea_career_type == "Earning to give") %>% summarise(total_donations = sum(donate_2015_c, na.rm = TRUE)) %>% mutate(percent_donations = total_donations / sum(df$donate_2015_c, na.rm = TRUE))
        , "total donations x ETG 2016"    = function(df) df %>% group_by(ea_career_type == "Earning to give") %>% summarise(total_donations = sum(donate_2016_c, na.rm = TRUE)) %>% mutate(percent_donations = total_donations / sum(df$donate_2016_c, na.rm = TRUE))
        , "ETG donations x act now-later 2016" = function(df) ctab(df, filters(student == "No", ea_career_type == "Earning to give"), donate_2016_c, act_now_or_later)
        , "ETG donations 2015"            = function(df) var_summary(dplyr::filter(df, ea_career_type == "Earning to give")$donate_2015_c, verbose = TRUE)
        , "ETG donations 2016"            = function(df) var_summary(dplyr::filter(df, ea_career_type == "Earning to give")$donate_2016_c, verbose = TRUE)
        , "EA year"                       = function(df) tab(df, which_year_EA)
        , "age x AI"                      = function(df) ctab(df, cause_import_ai_b, age >= 27, na.rm = TRUE)
        , "age x poverty"                 = function(df) ctab(df, cause_import_poverty_b, age >= 27, na.rm = TRUE)
        , "how heard"                     = function(df) tab(df, first_heard_EA)
        , "First heard EA by year"        = function(df) { for (var in unique(df$which_year_EA)) { message(var); print(tab(dplyr::filter_(df, paste0("which_year_EA == '", var, "'")), first_heard_EA, freq = FALSE, percent = TRUE)) }}
        , "LessWrong refer over year"     = function(df) tab(df, which_year_EA, first_heard_EA == "LessWrong", na.rm = TRUE, percent = TRUE)
        , "GWWC refer over year"          = function(df) tab(df, which_year_EA, first_heard_EA == "Giving What We Can", na.rm = TRUE, percent = TRUE)
        , "80K refer over year"           = function(df) tab(df, which_year_EA, first_heard_EA == "80,000 Hours", na.rm = TRUE, percent = TRUE)
        , "SSC refer over year"           = function(df) tab(df, which_year_EA, first_heard_EA == "Slate Star Codex", na.rm = TRUE, percent = TRUE)
        , "Heard EA attributed donations" = function(df) { df %>% group_by(first_heard_EA) %>% summarise(sum(donate_2016_c, na.rm = TRUE)) %>% setNames(., list("first_heard_EA", "sum_donations")) %>% arrange(-sum_donations) }
        , "Heard EA attributed donations" = function(df) { df %>% group_by(first_heard_EA) %>% summarise(sum(donate_2016_c, na.rm = TRUE)) %>% setNames(., list("first_heard_EA", "sum_donations")) %>% arrange(-sum_donations) }
        , "Heard EA attributed donors"    = function(df) tab(df, first_heard_EA, donate_2016_c > 0, percent = TRUE, na.rm = TRUE)
        , "Heard EA attributed career"    = function(df) tab(df, first_heard_EA, ea_career, percent = TRUE, na.rm = TRUE)
        , "involved_TLYCS"                = function(df) tab(df, involved_TLYCS)
        , "involved_local_EA"             = function(df) tab(df, involved_local_EA)
        , "involved_lesswrong"            = function(df) tab(df, involved_lesswrong)
        , "involved_givewell"             = function(df) tab(df, involved_givewell)
        , "involved_online_ea"            = function(df) tab(df, involved_online_ea)
        , "involved_personal_contact"     = function(df) tab(df, involved_personal_contact)
        , "involved_80K"                  = function(df) tab(df, involved_80K)
        , "involved_GWWC"                 = function(df) tab(df, involved_GWWC)
        , "involved EA Global"            = function(df) tab(df, involved_ea_global)
        , "involved Book or Blog"         = function(df) tab(df, involved_book_blog)
        , "involved Swiss"                = function(df) tab(df, involved_swiss)
        , "Involvment attributed"         = function(df) { for (var in get_vars(df, "involved")) { message(var); cat("N: ", sum(df[[var]] == "Yes", na.rm = TRUE), "\n", "Donations: ", sum(df[df[[var]] == "Yes",]$donate_2016_c, na.rm = TRUE), "\n", "Donors: ", sum(df[df[[var]] == "Yes",]$donate_2016_c > 0, na.rm = TRUE), "\n", "Careers: ", sum(df[df[[var]] == "Yes",]$ea_career == "Yes", na.rm = TRUE), "\n") }}
        , "member_ea_fb"                  = function(df) tab(df, member_ea_fb)
        , "member_ea_forum"               = function(df) tab(df, member_ea_forum)
        , "member_gwwc"                   = function(df) tab(df, member_gwwc)
        , "member_lw"                     = function(df) tab(df, member_lw)
        , "member_local_group"            = function(df) tab(df, member_local_group)
        , "would_like_more_involvement"   = function(df) tab(df, would_like_more_involvement)
        , "Insecurity about EA"           = function(df) tab(df, insecurity)
        , "EA Welcoming"                  = function(df) tab(df, ea_welcoming)
        , "ea_nps"                        = function(df) tab(df, ea_nps)
        , "confidence in personal EA"     = function(df) tab(df, confident_future_ea_personal)
        , "confidence in EA movement"     = function(df) tab(df, confident_future_ea_movement)
        , "confidence x confidence"       = function(df) ctab(df, confident_future_ea_personal, confident_future_ea_movement, na.rm = TRUE)
        , "MIRI donations by year"        = function(df) ctab(df, filters(donate_2016_c > 0), donate_miri_2016_c, which_year_EA, na.rm = TRUE)
        , "AMF donations by year"         = function(df) ctab(df, filters(donate_2016_c > 0), donate_amf_2016_c, which_year_EA, na.rm = TRUE)
        , "AMF donations by first heard"  = function(df) ctab(df, filters(donate_2016_c > 0), donate_amf_2016_c, first_heard_EA, na.rm = TRUE)
        , "MIRI donations by first heard" = function(df) ctab(df, filters(donate_2016_c > 0), donate_miri_2016_c, first_heard_EA, na.rm = TRUE) 
        , "first heard through local grp" = function(df) tab(df, first_heard_EA == "Local EA group")
        , "first heard local x GWWC mem"  = function(df) ctab(df, member_gwwc, first_heard_EA == "Local EA group")
        , "first heard local x donate"    = function(df) ctab(df, donate_2016_c, first_heard_EA == "Local EA group")
        , "would attend"                  = function(df) tab(df, would_attend_local_group)
        , "involved_local_EA x GWWC invo" = function(df) ctab(df, involved_local_EA, involved_GWWC)
        , "involved_local_EA x GWWC mem"  = function(df) ctab(df, involved_local_EA, member_gwwc)
        , "involved_GWWC x GWWC mem"      = function(df) ctab(df, involved_GWWC, member_gwwc)
        , "involved_local_EA x donate"    = function(df) ctab(df, donate_2016_c, involved_local_EA)
        , "member_local x GWWC"           = function(df) ctab(df, member_local_group, member_gwwc)
        , "involved_local_EA x local mem" = function(df) ctab(df, involved_local_EA, member_local_group)
        , "member_local x year"           = function(df) ctab(df, member_local_group, which_year_EA)
        , "member_local x donate"         = function(df) ctab(df, donate_2016_c, member_local_group)
        , "member_gwwc x involved local"  = function(df) ctab(df, member_gwwc, involved_local_EA)
        , "city x member_gwwc"            = function(df) ctab(df, city, member_gwwc, top = 10)
        , "GWWC non-students donating >10% (2015)" = function(df) tab(df, filters(student == "No", income_2016_individual_c > 10000, which_year_EA %not_in% c(2016, 2017)), member_gwwc, p_donate_2015 >= 0.1, na.rm = TRUE, percent = TRUE)
        , "GWWC non-students donating >10% (2016)" = function(df) tab(df, filters(student == "No", income_2016_individual_c > 10000, which_year_EA != 2017), member_gwwc, p_donate_2016 >= 0.1, na.rm = TRUE, percent = TRUE)
        , "GWWC non-students donating >10% (2015 and 2016)" = function(df) tab(df, filters(student == "No", income_2016_individual_c > 10000, which_year_EA %not_in% c(2016, 2017)), member_gwwc, p_donate_2015 >= 0.1 & p_donate_2016 >= 0.1, na.rm = TRUE, percent = TRUE)
        , "GWWC non-students donating >10% (2015 or 2016)" = function(df) tab(df, filters(student == "No", income_2016_individual_c > 10000, which_year_EA %not_in% c(2016, 2017)), member_gwwc, p_donate_2015 >= 0.1 | p_donate_2016 >= 0.1, na.rm = TRUE, percent = TRUE)
        , "GWWC non-students donating >10% (2015 - 2016 average)" = function(df) tab(df, filters(student == "No", income_2016_individual_c > 10000, which_year_EA %not_in% c(2016, 2017)), member_gwwc, p_donate_2015 + p_donate_2016 >= 0.2, na.rm = TRUE, percent = TRUE)
        , "GWWC pledge adherence by year" = function(df) tab(df, filters(income_2016_individual_c > 10000, student == "No", member_gwwc == "Yes", which_year_EA %not_in% c(2016, 2017)), p_donate_2015 >= 0.1, p_donate_2016 >= 0.1, na.rm = TRUE, percent = TRUE)
				, "binary cause view x GWWC"      = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, list("member_gwwc", var), percent = TRUE)) } }
        , "cause donations x GWWC"        = function(df) { for (var in get_vars(df, "donate_cause_.+2016")) { print(tab_(df, list(lazyeval::as.lazy(paste(var, "> 0")), "member_gwwc"), na.rm = TRUE)) } }
        , "Number of orgs people donate to" = function(df) { get_vars(df, "donate_.+2016_c") %>% grep("cause", ., invert = TRUE, value = TRUE) %>% df[, .] %>% apply(., 1, function(x) x > 0) %>% apply(., 2, sum) %>% table }
        , "referrer self report"          = function(df) tab(df, referrer_self_report)
        , "referrer URL"                  = function(df) tab(df, referrer2)
        , "referrer URL (narrower)"       = function(df) tab(df, referrer3)
        , "referrer3 x age"               = function(df) ctab(df, age, referrer3)
        , "referrer3 x gender"            = function(df) ctab(df, gender_b, referrer3)
        , "referrer3 x white"             = function(df) ctab(df, race_white, referrer3, na.rm = TRUE)
        , "referrer3 x USA"               = function(df) ctab(df, country == "United States", referrer3, na.rm = TRUE)
        , "referrer3 x student"           = function(df) ctab(df, student, referrer3)
        , "referrer3 x veg"               = function(df) ctab(df, veg_b, referrer3)
        , "referrer3 x donation"          = function(df) ctab(df, filter(student == "No"), donate_2016_c, referrer3)
        , "referrer3 x income"            = function(df) ctab(df, filter(student == "No"), income_2016_individual_c, referrer3)
        , "referrer3 x poverty"           = function(df) ctab(df, cause_import_poverty_b, referrer3)
        , "referrer3 x AI"                = function(df) ctab(df, cause_import_ai_b, referrer3)
        , "referrer3 x consequentialism"  = function(df) ctab(df, ifelse(is.na(moral_philosophy), NA, grepl("Consequentialism", moral_philosophy)), referrer3, na.rm = TRUE)
        , "referrer3 x year got involved" = function(df) ctab(df, which_year_EA, referrer3)
      )
    )
  )
})
