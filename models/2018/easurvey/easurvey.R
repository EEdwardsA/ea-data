#TODO: recode gwwc_year, ea_volunteer_hours, extra_cause_*, employed_*, referrer_self_report_*
# TODO: Add scales
#TODO: analyze plan_donate_2018, relationship_status, how_feel_donate, why_donate_less, cost_effectiveness_ratio
list(
  import = list(file = "data/2018/2018-ea-survey-anon-currencied.csv"),

  data   = list(
    "Drop insincere"   = function(df) {
        message(NROW(df), " results before dropping insincere")
        df <- dplyr::filter(df, grepl("Yes", sincere))
        message(NROW(df), " results after dropping insincere")
        df
    }
    , "Drop non-EA I"    = function(df) {
        message(length(na.omit(df$is_ea1)), " answered EA I question")
        df <- df[df$is_ea1 == 1, ]
        message(NROW(df), " after dropping non-EA I")
        df
    }
    , "Drop non-EA II"    = function(df) {
        message(length(na.omit(df$is_ea2)), " answered EA II question")
        df <- df[df$is_ea2 == 1, ]
        message(NROW(df), " after dropping non-EA II")
        df
    }
    , "Make age"     = function(df) {
        df$age <- 2018 - as.numeric(df$birth_year)
        df$age <- ifelse(df$age <= 0, NA, df$age)
        df$age <- ifelse(df$age >= 100, NA, df$age)
        df
    }
    , "Make % inc donate" = function(df) {
        p <- df$donate_2017_c / df$income_2017_individual_c
        p[is.infinite(p)] <- NA
        p[is.nan(p)] <- NA
        df$p_donate_2017 <- p
        df
    }
    , "Make binary cause breakdown" = function(df) {
      for (variable in get_vars(df, "cause_import")) {
        df[[paste0(variable, "_b")]] <- swap_by_value(df, variable,
                                          list("5" = ":)",
                                               "4" = ":)",
                                               "3" = ":(",
                                               "2" = ":(",
                                               "1" = ":("))[[variable]]
      }
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
        df$left <- swap_by_value(df, "politics", list("Center Left" = "Left",
                                                      "Left" = "Left",
                                                      "Other" = NA,
                                                      "Libertarian" = "Non-Left",
                                                      "Center" = "Non-Left",
                                                      "Prefer Not to Answer" = NA,
                                                      "Center Right" = "Non-Left",
                                                      "Right" = "Non-Left"))$politics
        df
      }
    , "Donations by cause area" = function(df) {
        orgs_by_cause <- list("meta" = c("RC", "80K", "CFAR", "CEA", "CS", "EF", "TLYCS"),
                              "cause_pri" = c("ACE", "FRI", "GW"),
                              "poverty" = c("AMF", "DTW", "END", "GD", "MC", "SCI", "Sightsavers"),
                              "animal_welfare" = c("faunalytics", "GF", "MFA", "SP", "THL"),
                              "far_future" = c("FHI", "MIRI"))

        for (cause in names(orgs_by_cause)) {
          out <- paste(orgs_by_cause[[cause]], "2017", "c", sep = "_") %>%
                   get_vars(df, ., ignore.case = TRUE) %/>%
                   first %:>%
                   Negate(is.na) %/>%
                   (function(x) { df[[x]] }) %_>%
                   fn(x, y, nas_are_zeros(x) + nas_are_zeros(y))
          df[[paste("donate", "cause", cause, "2017", "c", sep = "_")]] <- out
        }
        df
      }
    , "Write and drop comments" = function(df) {
      write_comments <- resource("lib/write_comments")
      write_comments(df, "data/2018/2018-survey-comments.txt")
      for (var in get_vars(df, "comment")) {
        df[[var]] <- NULL
      }
      df
    }
    , "Add student" = function(df) {
      df$student <- ifelse(is.na(df$employed_student_part), ifelse(is.na(df$employed_student_full), FALSE, TRUE), TRUE)
      df
    }
    , "Clean binary variables" = function(df) {
      vars_to_clean <- c("ea_career_shifted_path", "ea_career_will_shift_path", "race", "employed", "studied", "referrer", "can_share", "donation_kind", "involved", "member") 
      vars_to_clean <- lapply(vars_to_clean, get_vars, df = df) %>% flatten
      for (var in vars_to_clean) {
        df[[var]] <- ifelse(is.na(df[[var]]), FALSE, TRUE)
      }
      df
    }
    , "Export DF" = function(df) {
      readr::write_csv(data, "data/2018/2018-ea-survey-anon-currencied-processed.csv")
      df
    }
  )

  , analyze = list(
    # list(write = "data/2018/2018-survey-analysis-tables.txt"),
    list(write = "stdout"), # <-- toggle this to print to the screen.
    list(
      "cause_import_animal_welfare"          = function(df) tab(df, cause_import_animal_welfare)
      , "cause_import_cause_prioritization"  = function(df) tab(df, cause_import_cause_prioritization)
      , "cause_import_climate_change"        = function(df) tab(df, cause_import_climate_change)
      , "cause_import_biosecurity"           = function(df) tab(df, cause_import_biosecurity)
      , "cause_import_nuclear_security"      = function(df) tab(df, cause_import_nuclear_security)
      , "cause_import_ai"                    = function(df) tab(df, cause_import_ai)
      , "cause_import_mental_health"         = function(df) tab(df, cause_import_mental_health)
      , "cause_import_xrisk_other"           = function(df) tab(df, cause_import_xrisk_other)
      , "cause_import_poverty"               = function(df) tab(df, cause_import_poverty)
      , "cause_import_rationality"           = function(df) tab(df, cause_import_rationality)
      , "cause_import_meta"                  = function(df) tab(df, cause_import_meta)
      , "binary cause view"                  = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, var)) } }
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
      , "summarize donations 2017"                = function(df) var_summary(df$donate_2017_c, verbose = TRUE)
      , "donations x student"                = function(df) ctab(df, donate_2017_c, student)
      , "summary donations for students"     = function(df) var_summary(dplyr::filter(df, student == "Yes")$donate_2017_c, verbose = TRUE)
      , "summary donations for non-students" = function(df) var_summary(dplyr::filter(df, student == FALSE)$donate_2017_c, verbose = TRUE)
      , "donation quantile 1"                = function(df) quantile(df$donate_2017_c, probs = seq(0.1, 1, len = 10), na.rm = TRUE)
      , "donation quantile 2"                = function(df) quantile(df$donate_2018_c, probs = seq(0.91, 1, len = 10), na.rm = TRUE)
      , "summarize % donations"         = function(df) var_summary(dplyr::filter(df, income_2017_individual_c > 10000)$p_donate_2017 * 100, verbose = TRUE)
      , "% donation quantile"                = function(df) quantile(dplyr::filter(df, income_2017_individual_c > 10000)$p_donate_2017 * 100, probs = seq(0.1, 1, len = 10), na.rm = TRUE)
      , "% donation breakdown"               = function(df) breakdown(dplyr::filter(df, income_2017_individual_c > 10000), "p_donate_2017", c(0.01, 0.02, 0.03, 0.05, 0.1, 0.15, seq(0.2, 0.9, by =  0.1)))
      , "RC Donations (Total)"          = function(df) var_summary(df$donate_RC_2017_c, verbose = TRUE)
      , "Did donate to RC?"             = function(df) tab(df, donate_RC_2017_c > 0)
      , "80K Donations (Total)"         = function(df) var_summary(df$donate_80K_2017_c, verbose = TRUE)
      , "Did donate to 80K?"            = function(df) tab(df, donate_80K_2017_c > 0)
      , "ACE Donations 2017 (Total)"         = function(df) var_summary(df$donate_ace_2017_c, verbose = TRUE)
      , "Did donate to ACE 2017?"            = function(df) tab(df, donate_ace_2017_c > 0)
      , "AMF Donations 2017 (Total)"         = function(df) var_summary(df$donate_amf_2017_c, verbose = TRUE)
      , "Did donate to AMF 2017?"            = function(df) tab(df, donate_amf_2017_c > 0)
      , "CFAR Donations 2017 (Total)"        = function(df) var_summary(df$donate_cfar_2017_c, verbose = TRUE)
      , "Did donate to CFAR 2017?"           = function(df) tab(df, donate_cfar_2017_c > 0)
      , "CEA Donations 2017 (Total)"         = function(df) var_summary(df$donate_cea_2017_c, verbose = TRUE)
      , "Did donate to CEA 2017?"            = function(df) tab(df, donate_cea_2017_c > 0)
      , "DTW Donations 2017 (Total)"         = function(df) var_summary(df$donate_dtw_2017_c, verbose = TRUE)
      , "Did donate to DTW 2017?"            = function(df) tab(df, donate_dtw_2017_c > 0)
      , "EF Donations 2017 (Total)"          = function(df) var_summary(df$donate_ef_2017_c, verbose = TRUE)
      , "Did donate to EF 2017?"             = function(df) tab(df, donate_ef_2017_c > 0)
      , "GD Donations 2017 (Total)"     = function(df) var_summary(df$donate_gd_2017_c, verbose = TRUE)
      , "Did donate to GD 2017?"        = function(df) tab(df, donate_gd_2017_c > 0)
      , "GW Donations 2017 (Total)"     = function(df) var_summary(df$donate_gw_2017_c, verbose = TRUE)
      , "Did donate to GW 2017?"        = function(df) tab(df, donate_gw_2017_c > 0)
      , "GF Donations 2017 (Total)"     = function(df) var_summary(df$donate_gf_2017_c, verbose = TRUE)
      , "Did donate to GF 2017?"        = function(df) tab(df, donate_gf_2017_c > 0)
      , "MIRI Donations 2017 (Total)"   = function(df) var_summary(df$donate_miri_2017_c, verbose = TRUE)
      , "Did donate to MIRI 2017?"      = function(df) tab(df, donate_miri_2017_c > 0)
      , "MFA Donations 2017 (Total)"    = function(df) var_summary(df$donate_mfa_2017_c, verbose = TRUE)
      , "Did donate to MFA 2017?"       = function(df) tab(df, donate_mfa_2017_c > 0)
      , "SCI Donations 2017 (Total)"    = function(df) var_summary(df$donate_sci_2017_c, verbose = TRUE)
      , "Did donate to SCI 2017?"       = function(df) tab(df, donate_sci_2017_c > 0)
      , "THL Donations 2017 (Total)"    = function(df) var_summary(df$donate_thl_2017_c, verbose = TRUE)
      , "Did donate to THL 2017?"       = function(df) tab(df, donate_thl_2017_c > 0)
      , "Meta Cause Donations 2017 (Total)"  = function(df) var_summary(df$donate_cause_meta_2017_c, verbose = TRUE)
      , "Did donate to Meta Cause 2017?"     = function(df) tab(df, donate_cause_meta_2017_c > 0)
      , "Global Poverty Cause Donations 2017 (Total)"  = function(df) var_summary(df$donate_cause_poverty_2017_c, verbose = TRUE)
      , "Did donate to Global Poverty Cause 2017?"     = function(df) tab(df, donate_cause_poverty_2017_c > 0)
      , "Cause Prioritization Cause Donations 2017 (Total)"  = function(df) var_summary(df$donate_cause_cause_pri_2017_c, verbose = TRUE)
      , "Did donate to Cause Prioritization Cause 2017?"     = function(df) tab(df, donate_cause_cause_pri_2017_c > 0)
      , "Animal Welfare Cause Donations 2017 (Total)"  = function(df) var_summary(df$donate_cause_animal_welfare_2017_c, verbose = TRUE)
      , "Did donate to Animal Welfare Cause 2017?"     = function(df) tab(df, donate_cause_animal_welfare_2017_c > 0)
      , "Far Future Cause Donations 2017 (Total)"  = function(df) var_summary(df$donate_cause_far_future_2017_c, verbose = TRUE)
      , "Did donate to Far Future Cause 2017?"     = function(df) tab(df, donate_cause_far_future_2017_c > 0)
      , "age"                           = function(df) tab(df, age)
      , "age plot"                      = function(df) { ggplot(df, aes(age)) + geom_histogram(color="black", fill="lightblue") + scale_x_continuous("Age") + ggtitle("Ages of EAs") }
      , "gender"                        = function(df) tab(df, gender)
      , "race_white"                    = function(df) tab(df, race_white)
      , "race_black"                    = function(df) tab(df, race_black)
      , "race_hispanic"                 = function(df) tab(df, race_hispanic)
      , "race_native_american"          = function(df) tab(df, race_native_american)
      , "race_pacific_islander"         = function(df) tab(df, race_pacific_islander)
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
      , "studied_cs"                    = function(df) tab(df, studied_cs)
      , "studied_econ"                  = function(df) tab(df, studied_econ)
      , "studied_engineering"           = function(df) tab(df, studied_engineering)
      , "studied_math"                 = function(df) tab(df, studied_math)
      , "studied_medicine"              = function(df) tab(df, studied_medicine)
      , "studied_psych"                 = function(df) tab(df, studied_psych)
      , "studied_phil"                  = function(df) tab(df, studied_phil)
      , "studied_physics"               = function(df) tab(df, studied_physics)
      , "studied_humanities"            = function(df) tab(df, studied_humanities)
      , "studied_social_science"        = function(df) tab(df, studied_social_science)
      , "studied_other_science"         = function(df) tab(df, studied_other_science)
      , "studied_vocational"            = function(df) tab(df, studied_vocational)
      , "ea_volunteer_hours"              = function(df) tab(df, ea_volunteer_hours)
      , "ea_career_shifted_path"          = function(df) tab(df, ea_career_shifted_path)
      , "ea_career_will_shift_path"       = function(df) tab(df, ea_career_will_shift_path)
      , "gender x poverty donations"      = function(df) ctab(df, filter(donate_2017_c > 0), donate_cause_poverty_2017_c, gender_b)
      , "gender x AR donations"           = function(df) ctab(df, filter(donate_2017_c > 0), donate_cause_animal_welfare_2017_c, gender_b)
      , "donations x ETG"                 = function(df) ctab(df, filter(student == FALSE), donate_2017_c, ea_career_type == "Earning to give")
      , "age x ETG"                     = function(df) ctab(df, age, ea_career_type == "Earning to give", na.rm = TRUE)
      , "total donations x ETG"         = function(df) df %>% group_by(ea_career_type == "Earning to give") %>% summarise(total_donations = sum(donate_2017_c, na.rm = TRUE)) %>% mutate(percent_donations = total_donations / sum(df$donate_2017_c, na.rm = TRUE))
      , "ETG donations"                 = function(df) var_summary(dplyr::filter(df, ea_career_type == "Earning to give")$donate_2017_c, verbose = TRUE)
      , "EA year"                       = function(df) tab(df, which_year_EA)
      , "age x AI"                      = function(df) ctab(df, cause_import_ai_b, age >= 27, na.rm = TRUE)
      , "age x poverty"                 = function(df) ctab(df, cause_import_poverty_b, age >= 27, na.rm = TRUE)
      , "how heard"                     = function(df) tab(df, first_heard_EA)
      , "First heard EA by year"        = function(df) { for (var in unique(df$which_year_EA)) { message(var); print(tab(dplyr::filter_(df, paste0("which_year_EA == '", gsub("'", "", var), "'")), first_heard_EA, freq = FALSE, percent = TRUE)) }}
      , "LessWrong refer over year"     = function(df) tab(df, which_year_EA, first_heard_EA == "LessWrong", na.rm = TRUE, percent = TRUE)
      , "GWWC refer over year"          = function(df) tab(df, which_year_EA, first_heard_EA == "Giving What We Can", na.rm = TRUE, percent = TRUE)
      , "80K refer over year"           = function(df) tab(df, which_year_EA, first_heard_EA == "80,000 Hours", na.rm = TRUE, percent = TRUE)
      , "SSC refer over year"           = function(df) tab(df, which_year_EA, first_heard_EA == "Slate Star Codex", na.rm = TRUE, percent = TRUE)
      , "Heard EA attributed donations" = function(df) { df %>% group_by(first_heard_EA) %>% summarise(sum(donate_2017_c, na.rm = TRUE)) %>% setNames(., list("first_heard_EA", "sum_donations")) %>% arrange(-sum_donations) }
      , "Heard EA attributed donations" = function(df) { df %>% group_by(first_heard_EA) %>% summarise(sum(donate_2017_c, na.rm = TRUE)) %>% setNames(., list("first_heard_EA", "sum_donations")) %>% arrange(-sum_donations) }
      , "Heard EA attributed donors"    = function(df) tab(df, first_heard_EA, donate_2017_c > 0, percent = TRUE, na.rm = TRUE)
      #, "Heard EA attributed career"    = function(df) tab(df, first_heard_EA, ea_career, percent = TRUE, na.rm = TRUE)
      , "involved_TLYCS"                = function(df) tab(df, involved_tlycs)
      , "involved_local_EA"             = function(df) tab(df, involved_local_EA)
      , "involved_university_EA"        = function(df) tab(df, involved_university_EA)
      , "involved_ace"                  = function(df) tab(df, involved_ace)
      , "involved_ssc"                  = function(df) tab(df, involved_ssc)
      , "involved_online_ea"            = function(df) tab(df, involved_online_ea)
      , "involved_80K"                  = function(df) tab(df, involved_80K)
      , "involved_GWWC"                 = function(df) tab(df, involved_GWWC)
      , "involved EA Global"            = function(df) tab(df, involved_ea_global)
      , "involved Book or Blog"         = function(df) tab(df, involved_book_blog)
      , "involved Swiss"                = function(df) tab(df, involved_swiss)
      , "involved_personal_contact"     = function(df) tab(df, involved_personal_contact)
      #, "Involvment attributed"         = function(df) { for (var in get_vars(df, "involved")) { message(var); cat("N: ", sum(df[[var]] == "Yes", na.rm = TRUE), "\n", "Donations: ", sum(df[df[[var]] == "Yes",]$donate_2017_c, na.rm = TRUE), "\n", "Donors: ", sum(df[df[[var]] == "Yes",]$donate_2017_c > 0, na.rm = TRUE), "\n", "Careers: ", sum(df[df[[var]] == "Yes",]$ea_career == "Yes", na.rm = TRUE), "\n") }}
      , "member_ea_fb"                  = function(df) tab(df, member_ea_fb)
      , "member_ea_forum"               = function(df) tab(df, member_ea_forum)
      , "member_gwwc"                   = function(df) tab(df, member_gwwc)
      , "member_lw"                     = function(df) tab(df, member_lw)
      , "member_local_group"            = function(df) tab(df, member_local_group)
      , "know_local_group"              = function(df) tab(df, know_local_group)
      , "want_local_group"              = function(df) tab(df, want_local_group)
      , "done_80K"                      = function(df) tab(df, done_80K)
      , "EA Welcoming"                  = function(df) tab(df, ea_welcoming)
      , "MIRI donations by year"        = function(df) ctab(df, filters(donate_2017_c > 0), donate_miri_2017_c, which_year_EA, na.rm = TRUE)
      , "AMF donations by year"         = function(df) ctab(df, filters(donate_2017_c > 0), donate_amf_2017_c, which_year_EA, na.rm = TRUE)
      , "AMF donations by first heard"  = function(df) ctab(df, filters(donate_2017_c > 0), donate_amf_2017_c, first_heard_EA, na.rm = TRUE)
      , "MIRI donations by first heard" = function(df) ctab(df, filters(donate_2017_c > 0), donate_miri_2017_c, first_heard_EA, na.rm = TRUE) 
      #, "first heard through local grp" = function(df) tab(df, first_heard_EA == "Local EA group")
      #, "first heard local x GWWC mem"  = function(df) ctab(df, member_gwwc, first_heard_EA == "Local EA group")
      #, "first heard local x donate"    = function(df) ctab(df, donate_2017_c, first_heard_EA == "Local EA group")
      , "involved_local_EA x GWWC invo" = function(df) ctab(df, involved_local_EA, involved_GWWC)
      , "involved_local_EA x GWWC mem"  = function(df) ctab(df, involved_local_EA, member_gwwc)
      , "involved_GWWC x GWWC mem"      = function(df) ctab(df, involved_GWWC, member_gwwc)
      , "involved_local_EA x donate"    = function(df) ctab(df, donate_2017_c, involved_local_EA)
      , "member_local x GWWC"           = function(df) ctab(df, member_local_group, member_gwwc)
      , "involved_local_EA x local mem" = function(df) ctab(df, involved_local_EA, member_local_group)
      , "member_local x year"           = function(df) ctab(df, member_local_group, which_year_EA)
      , "member_local x donate"         = function(df) ctab(df, donate_2017_c, member_local_group)
      , "member_gwwc x involved local"  = function(df) ctab(df, member_gwwc, involved_local_EA)
      , "city x member_gwwc"            = function(df) ctab(df, city, member_gwwc, top = 10)
      , "GWWC non-students donating >10%" = function(df) tab(df, filters(student == FALSE, income_2017_individual_c > 10000, which_year_EA %not_in% c(2017, 2018)), member_gwwc, p_donate_2017 >= 0.1, na.rm = TRUE, percent = TRUE)
      , "binary cause view x GWWC"      = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, list("member_gwwc", var), percent = TRUE)) } }
      , "cause donations x GWWC"        = function(df) { for (var in get_vars(df, "donate_2017_cause_.+2017")) { print(tab_(df, list(lazyeval::as.lazy(paste(var, "> 0")), "member_gwwc"), na.rm = TRUE)) } }
      , "Number of orgs people donate to" = function(df) { get_vars(df, "donate_.+2017_c") %>% grep("cause", ., invert = TRUE, value = TRUE) %>% df[, .] %>% apply(., 1, function(x) x > 0) %>% apply(., 2, sum) %>% table }
      #, "referrer self report"          = function(df) tab(df, referrer_self_report)
      #, "referrer URL"                  = function(df) tab(df, referrer2)
      #, "referrer URL (narrower)"       = function(df) tab(df, referrer3)
      #, "referrer3 x age"               = function(df) ctab(df, age, referrer3)
      #, "referrer3 x gender"            = function(df) ctab(df, gender_b, referrer3)
      #, "referrer3 x white"             = function(df) ctab(df, race_white, referrer3, na.rm = TRUE)
      #, "referrer3 x USA"               = function(df) ctab(df, country == "United States", referrer3, na.rm = TRUE)
      #, "referrer3 x student"           = function(df) ctab(df, student, referrer3)
      #, "referrer3 x veg"               = function(df) ctab(df, veg_b, referrer3)
      #, "referrer3 x donation"          = function(df) ctab(df, filter(student == FALSE), donate_2017_c, referrer3)
      #, "referrer3 x income"            = function(df) ctab(df, filter(student == FALSE), income_2017_individual_c, referrer3)
      #, "referrer3 x poverty"           = function(df) ctab(df, cause_import_poverty_b, referrer3)
      #, "referrer3 x AI"                = function(df) ctab(df, cause_import_ai_b, referrer3)
      #, "referrer3 x consequentialism"  = function(df) ctab(df, ifelse(is.na(moral_philosophy), NA, grepl("Consequentialism", moral_philosophy)), referrer3, na.rm = TRUE)
      #, "referrer3 x year got involved" = function(df) ctab(df, which_year_EA, referrer3)
    )
  )
)
