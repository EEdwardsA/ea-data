options(dplyr.print_max = 50)

list(
  import = function() {
    tryCatch({
      readr::read_csv("data/2017/2017-local-groups-survey-parsed.csv")
    }, error = function(e) {
      stop("Importing model did not work. Note that this model cannot be run without the ",
           "private data from the EA Survey team.")
    })
  }
  , data = list(
      "Drop bad entries" = function(df) {
        df <- filter(df, !(group_name %in% c("test", "a", "Big event group", "uuiuiu",
                                             "rtztrzt", "Test")))
        df <- filter(df, !is.na(group_name))
        df
      }
      , "Clean group name" = function(df) {
        ea_symbols <- c("Effective Altruism:", "Effective Altruism", "Effektiver Altruismus",
                        "Effektiv Altruism", "Altruisme Efficace", "Efektywny Altruizm",
                        "effective altruism", "Effective Altruists", "Effectief Altruïsme",
                        "efektivní altruismus", "Efektyvus altruizmas", "effektiver altruismus",
                        "Effective altruism", "effective altruists", "effektiv altruisme")
        for (symbol in ea_symbols) {
          df$group_name <- gsub(symbol, "EA", df$group_name)
        }
        gwwc_symbols <- c("Giving What We Can", "GWWC:")
        for (symbol in gwwc_symbols) {
          df$group_name <- gsub(symbol, "GWWC", df$group_name)
        }
        df <- swap_by_value(df, "group_name", list("Montreal" = "EA Montreal",
                                                   "AI Ethics Montreal" = "EA Montreal",
                                                   "80,000 Hours: Cambridge" = "80K Cambridge",
                                                   "Madison, WI" = "EA Madison",
                                                   "Spolek pro Efektivní altruismus" = "Czech EA",
                                                   "Spolek pro efektivni altruismus" = "Czech EA",
                                                   "Spolek pro EA (EA Czech Republic)" = "Czech EA",
                                                   "Czech Asociation for EA" = "Czech EA",
                                                   "Spolek pro EA (Czech Assosiation for EA)" = "Czech EA",
                                                   "Spolek pro EA" = "Czech EA",
                                                   "Southampton Student EA" = "EA Southampton",
                                                   "Southampton EA Society" = "EA Southampton",
                                                   "Southampton EA society" = "EA Southampton",
                                                   "SEA" = "Seattle EA",
                                                   "EAe UiO" = "EA UiO",
                                                   "EA UiO (EA University of Oslo)" = "EA UiO",
                                                   "EA UIO" = "EA UiO",
                                                   "EA Poland / EA" = "EA Poland",
                                                   "EA ottawa" = "EA Ottawa",
                                                   "EA ntnu" = "EA NTNU",
                                                   "EA Montreal - EA Montréal" = "EA Montréal",
                                                   "EA Montreal" = "EA Montréal",
                                                   "EA marburg/giessen" = "EA Marburg-Gießen",
                                                   "EA Marburg/Giessen" = "EA Marburg-Gießen",
                                                   "EA local group in Marburg/Gießen" = "EA Marburg-Gießen",
                                                   "EA Lokalgruppe Tübingen" = "EA Tübingen",
                                                   "EAe Bergen" = "EA Bergen",
                                                   "EA Kraków/Fundacja Optimum Pareto" = "EA Kraków",
                                                   "EA D.C." = "EA DC",
                                                   "DC EA" = "EA DC",
                                                   "EA Toulouse/France" = "EA France",
                                                   "EA Cambridge / FuSe" = "EA Cambridge",
                                                   "EA Ville de Quebec" = "EA Ville de Québec",
                                                   "EA Vlaanderen (+ EA KU Leuven + EA Brussels)" = "EA Vlaanderen",
                                                   "Bern" = "EA Bern",
                                                   "EA Uni Bern" = "EA Bern",
                                                   "EA Bern + EA Zürich" = "EA Bern",
                                                   "University of Southampton EA Society" = "EA Southampton",
                                                   "GWWC (Sheffield University student's union society)" = "GWWC Sheffield",
                                                   "Yale EA (Undergraduate Organization)" = "Yale EA",
                                                   "The Life You Can Save - Melbourne" = "TLYCS Melbourne",
                                                   "The Life You Can Save - Melbourne Chapter" = "TLYCS Melbourne"))
        df
      }
      , "Clean num new EAs" = function(df) {
        df <- swap_by_value(df, "num_new_eas", list("~10 to 30" = "30",
                                                    "~30 people attended one event." = "30",
                                                    "~8" = "8",
                                                    "0 (no event yet held)" = "0",
                                                    "10 (guess)" = "10",
                                                    "10-20" = "10",
                                                    "20-30" = "20",
                                                    "30-40" = "30",
                                                    "30-50" = "30",
                                                    "400?" = "400",
                                                    "60 (highly uncertain estimate - ranging from 20 to 100+)" = "60",
                                                    "90%CI = 10-100" = "50",
                                                    "Approx 200" = "200",
                                                    "around 230" = "230",
                                                    "Around 500" = "500"))
        df$num_new_eas <- suppressWarnings(as.numeric(df$num_new_eas))
        df
      }
      , "Clean number group members" = function(df) {
        swap_list <- list("+ 11 https://eavlaanderen.org/over-ea/gezichten/" = "11",
                          "<10" = "5",
                          "~11-15" = "11",
                          "~120" = "120",
                          "~20" = "20",
                          "~3-6" = "3",
                          "~800 (all list members)" = "800",
                          "10 fairly regular people" = "10",
                          "102 in the Facebook group, but only a fraction attends the events." = "10",
                          "196 people in the Facebook group" = "20",
                          "2 (for now)" = "2",
                          "20 that come regularly, 250 on our members list" = "20",
                          "205 (facebook group)" = "21",
                          "213 in the Facebook group" = "21",
                          "4 active members, about 10 people have been active recently" = "10",
                          "4-6" = "4",
                          "40-100" = "40",
                          "5 officers, 12 activists, 80+ followers" = "97",
                          "5-20" = "5",
                          "6 who actively organize events, 10-30 who come to the events" = "20",
                          "6-10" = "6",
                          "8 - 12" = "8",
                          "8-12" = "8",
                          "about 100" = "100",
                          "About 12 - 20 active and over 100 on our mailing list" = "12",
                          "about 15" = "15",
                          "Events typically bring about 7 to14 people with different groups of people for different event types. There are \"340\" FB members, and 267 people on the meetup.com page" = "7",
                          "Facebook: 77, Meetup: 315, have ever been to a meetup: estimate ~30?" = "10",
                          "Hard to say who counts as a member. Maybe 4 core members, but 20 attendees for each meetup." = "20",
                          "I think there are 5 maybe, but I don't think they are local" = "5",
                          "It is hard to tell. People join our meeting on and off. Maybe 7 people. It's not much." = "7",
                          "Meetup + Facebook Membership: Gross: ~150, Net: ~ 85" = "15",
                          "over 200 on meetup but 5-10 show up for a meeting" = "10",
                          "Will work on determining this. Estimate: 20 Regular members. 50-100 total members." = "20",
                          "You have to define what you mean by \"member\". Let's say 4." = "4")
        df <- swap_by_value(df, "num_group_members", swap_list)
        df$num_group_members <- suppressWarnings(as.numeric(df$num_group_members))
        df
      }
      , "Clean EA commitments" = function(df) {
        df <- swap_by_value(df, "num_ea_commitments", list("~35" = "35",
                                                           "0.3" = "0",
                                                           "0.5" = "0",
                                                           "11 currently active members, more in the past" = "11",
                                                           "27 (including all past members)" = "27",
                                                           "3 thus far?" = "3",
                                                           "90%CI= 7-15" = "11",
                                                           "At least a few core members" = "3",
                                                           "maybe 1 or 2" = "1",
                                                           "Several." = "2"))
        df$num_ea_commitments <- suppressWarnings(as.numeric(df$num_ea_commitments))
        df
      }
      , "Clean career changes" = function(df) {
        df <- swap_by_value(df, "num_career_changes", list("2-3" = "2",
                                                           "25%" = "0",
                                                           "40%" = "0",
                                                           "<10" = "5",
                                                           "~10-20" = "10",
                                                           "~25" = "25",
                                                           "0, or maybe 1" = "0",
                                                           "1-2" = "1",
                                                           "1-5" = "1",
                                                           "10-15" = "10",
                                                           "3 (the founding members, but ask again in a month)" = "3",
                                                           "30% to 50%" = "0",
                                                           "5-10 (at least heavily influenced by EA)" = "5",
                                                           "5?" = "5",
                                                           "6 that we know of, including people who have already chosen a career for EA reasons" = "6",
                                                           "90%CI = 1-8" = "3"))
        df$num_career_changes <- suppressWarnings(as.numeric(df$num_career_changes))
        df
      }
      , "Clean GWWC pledges" = function(df) {
        df <- swap_by_value(df, "num_pledges", list("~10" = "10",
                                                    "~25" = "25",
                                                    "~5+" = "5",
                                                    "1 (me!)" = "0",
                                                    "1 (me)" = "0",
                                                    "2 of us (both of us took it before we were involved in local groups)" = "0",
                                                    "2-4" = "3",
                                                    "2-5" = "3",
                                                    "27 (including all past members)" = "27",
                                                    "3 (the founding members, ask again in a month)" = "3",
                                                    "3-5" = "4",
                                                    "at least 1" = "1",
                                                    "At least 2" = "2",
                                                    "at least 6" = "6",
                                                    "Current members or overall historically? Current members = ~2, historically = ~6" = "6",
                                                    "Not sure; we're soon launching a pledge campaign (5+)" = "5",
                                                    "Roughly 4-7." = "5",
                                                    "unknown...at least 3?" = "3"))
        df$num_pledges <- suppressWarnings(as.numeric(df$num_pledges))
        df
      }
      , "Clean noncounterfactual pledges" = function(df) {
        df <- swap_by_value(df, "num_noncounterfactual_pledges", list("~1-2" = "1",
                                                                      "~7" = "7",
                                                                      "-" = "0",
                                                                      "0 (roughly)" = "0",
                                                                      "0 most likely" = "0",
                                                                      "0-2" = "1",
                                                                      "1-3" = "2",
                                                                      "2-5" = "3",
                                                                      "Argh, same issue as above. Overall answer is around 4" = "4",
                                                                      "at least 5" = "5",
                                                                      "Roughly 10" = "10"))
        df$num_noncounterfactual_pledges <- suppressWarnings(as.numeric(df$num_noncounterfactual_pledges))
        df$num_counterfactual_pledges <- df$num_pledges - df$num_noncounterfactual_pledges
        df$num_counterfactual_pledges <- ifelse(df$num_counterfactual_pledges < 0, 0, df$num_counterfactual_pledges)
        df
      }
      , "Clean meetup" = function(df) {
        # Remove everything that is not a number to get raw percentages
        df$pct_members_from_meetupcom <- gsub("[^[:digit:]]", "", df$pct_members_from_meetupcom)
        df$pct_members_from_meetupcom <- suppressWarnings(as.numeric(df$pct_members_from_meetupcom))
        df$pct_members_from_meetupcom <- df$pct_members_from_meetupcom / 100
        df
      }
      , "Make motivation scores numeric" = function(df) {
        df$motivators_rank_interesting_content %<>% as.numeric
        df$motivators_rank_enjoyable_activities %<>% as.numeric
        df$motivators_rank_having_responsibility %<>% as.numeric
        df$motivators_rank_interacting_with_eas %<>% as.numeric
        df
      }
      , "Make top motivator" = function(df) {
        df$motivators_top <- ifelse(df$motivators_rank_interacting_with_eas == 1, "Interacting with EAs",
                                    ifelse(df$motivators_rank_having_responsibility == 1, "Having responsibility",
                                           ifelse(df$motivators_rank_enjoyable_activities == 1, "Enjoyable activities",
                                                  ifelse(df$motivators_rank_interesting_content == 1, "Interesting content", NA))))
        df
      }
      , "Write and drop comments" = function(df) {
        write_comments <- resource("lib/write_comments")
        write_comments(df, "data/2017/2017-lgs-comments.txt")
        for (var in get_vars(df, "comment")) {
          df[[var]] <- NULL
        }
        df
      }
  )
  , analyze = list(
    list(write = "data/2017/2017-lgs-analysis-tables.txt"),
    # list(write = "stdout"), # <-- toggle this to print to the screen.
    list(
      "member vs. leader"                      = function(df) tab(df, is_member)
      , "group survey pop"                     = function(df) tab(df, group_name, top = 20)
      , "group purpose"                        = function(df) tab(df, group_purpose)
      , "outside help - CEA"                   = function(df) { df %>% group_by(group_name) %>% summarise(total = sum(outside_help_CEA == "CEA", na.rm = TRUE) / length(group_name) * 100) %>% arrange(-total) %>% filter(total != 0) }
      , "outside help - GWWC"                  = function(df) { df %>% group_by(group_name) %>% summarise(total = sum(outside_help_gwwc == "Giving What We Can", na.rm = TRUE) / length(group_name) * 100) %>% arrange(-total) %>% filter(total != 0) }
      , "outside help - 80K"                   = function(df) { df %>% group_by(group_name) %>% summarise(total = sum(outside_help_80K == "80,000 Hours", na.rm = TRUE) / length(group_name) * 100) %>% arrange(-total) %>% filter(total != 0) }
      , "outside help - LEAN"                  = function(df) { df %>% group_by(group_name) %>% summarise(total = sum(outside_help_lean == "LEAN", na.rm = TRUE) / length(group_name) * 100) %>% arrange(-total) %>% filter(total != 0) }
      , "outside help - TLYCS"                 = function(df) { df %>% group_by(group_name) %>% summarise(total = sum(outside_help_tlycs == "The Life You Can Save", na.rm = TRUE) / length(group_name) * 100) %>% arrange(-total) %>% filter(total != 0) }
      , "outside help - EAF"                   = function(df) { df %>% group_by(group_name) %>% summarise(total = sum(outside_help_eaf == "Effective Altruism Foundation (EAF)", na.rm = TRUE) / length(group_name) * 100) %>% arrange(-total) %>% filter(total != 0) }
      , "value - group calls"                  = function(df) tab(df, value_group_calls, percent = TRUE)
      , "value - group calls II"               = function(df) tab(df, ifelse(is.na(value_group_calls), NA, value_group_calls %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "has - FB"                             = function(df) tab(df, has_fb, percent = TRUE, na.rm = TRUE)
      , "value - group FB"                     = function(df) tab(df, value_group_fb, percent = TRUE)
      , "value - group FB II"                  = function(df) tab(df, ifelse(is.na(value_group_fb), NA, value_group_fb %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "value - group newsletter"             = function(df) tab(df, value_group_newsletter, percent = TRUE)
      , "value - group newsletter II"          = function(df) tab(df, ifelse(is.na(value_group_newsletter), NA, value_group_newsletter %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "value - group slack"                  = function(df) tab(df, value_group_slack, percent = TRUE)
      , "value - group slack II"               = function(df) tab(df, ifelse(is.na(value_group_slack), NA, value_group_slack %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "has - Meetup.com"                     = function(df) tab(df, has_meetup)
      , "value - Meetup.com - % of members"    = function(df) tab(df, pct_members_from_meetupcom, filter(has_meetup == TRUE), percent = TRUE, na.rm = TRUE)
      , "value - Meetup.com - % of members II" = function(df) { df %>% filter(has_meetup == TRUE) %>% .$pct_members_from_meetupcom %>% var_summary(., verbose = TRUE) }
      , "has - group email"                    = function(df) tab(df, has_group_email)
      , "value - group email"                  = function(df) tab(df, want_group_email, na.rm = TRUE, percent = TRUE)
      , "value - Mentor Program"               = function(df) tab(df, value_mentor_program, percent = TRUE)
      , "value - Mentor Program II"            = function(df) tab(df, ifelse(is.na(value_mentor_program), NA, value_mentor_program %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "value - Personal Support"             = function(df) tab(df, value_personal_support, percent = TRUE)
      , "value - Personal Support II"          = function(df) tab(df, ifelse(is.na(value_personal_support), NA, value_personal_support %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "value - Practical Support"            = function(df) tab(df, value_support, percent = TRUE)
      , "value - Practical Support II"         = function(df) tab(df, ifelse(is.na(value_support), NA, value_support %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "value - Tech Support"                 = function(df) tab(df, value_tech, percent = TRUE)
      , "value - Tech Support II"              = function(df) tab(df, ifelse(is.na(value_tech), NA, value_tech %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "has - Website"                        = function(df) tab(df, has_meetup, percent = TRUE, na.rm = TRUE)
      , "value - Website Support"              = function(df) tab(df, value_website, na.rm = TRUE, percent = TRUE)
      , "value - Written Guides"               = function(df) tab(df, value_written_guides, percent = TRUE)
      , "value - Written Guides II"            = function(df) tab(df, ifelse(is.na(value_written_guides), NA, value_written_guides %in% c("Useful", "Very Useful")), na.rm = TRUE, percent = TRUE)
      , "# group members"                      = function(df) var_summary(df$num_group_members, verbose = TRUE)
      , "# members by group"                   = function(df) { df %>% group_by(group_name) %>% select(num_group_members) %>% summarise(total = min(num_group_members, na.rm = TRUE)) %>% arrange(-total) }
      , "# new EAs"                            = function(df) var_summary(df$num_new_eas, verbose = TRUE)
      , "new EAs by group"                     = function(df) { df %>% group_by(group_name) %>% select(num_new_eas) %>% summarise(total = min(num_new_eas, na.rm = TRUE)) %>% arrange(-total) }
      , "new EAs by LEAN"                      = function(df) { df %>% group_by(outside_help_lean) %>% select(num_new_eas) %>% summarise(total = sum(num_new_eas, na.rm = TRUE)) %>% arrange(-total) }
      , "new EAs by LEAN group"                = function(df) { df %>% filter(outside_help_lean == "LEAN") %>% group_by(group_name) %>% select(num_new_eas) %>% summarise(total = min(num_new_eas, na.rm = TRUE)) %>% arrange(-total) }
      , "# EA commitments"                     = function(df) var_summary(df$num_ea_commitments, verbose = TRUE)
      , "committed EAs by group"               = function(df) { df %>% group_by(group_name) %>% select(num_ea_commitments) %>% summarise(total = min(num_ea_commitments, na.rm = TRUE)) %>% arrange(-total) }
      , "committed EAs by LEAN"                = function(df) { df %>% group_by(outside_help_lean) %>% select(num_ea_commitments) %>% summarise(total = sum(num_ea_commitments, na.rm = TRUE)) %>% arrange(-total) }
      , "committed EAs by LEAN group"          = function(df) { df %>% filter(outside_help_lean == "LEAN") %>% group_by(group_name) %>% select(num_ea_commitments) %>% summarise(total = min(num_ea_commitments, na.rm = TRUE)) %>% arrange(-total) }
      , "# EA career changes"                  = function(df) var_summary(df$num_career_changes, verbose = TRUE)
      , "career changes by group"              = function(df) { df %>% group_by(group_name) %>% select(num_career_changes) %>% summarise(total = min(num_career_changes, na.rm = TRUE)) %>% arrange(-total) }
      , "career changes by LEAN"               = function(df) { df %>% group_by(outside_help_lean) %>% select(num_career_changes) %>% summarise(total = sum(num_career_changes, na.rm = TRUE)) %>% arrange(-total) }
      , "career changes by LEAN group"         = function(df) { df %>% filter(outside_help_lean == "LEAN") %>% group_by(group_name) %>% select(num_career_changes) %>% summarise(total = min(num_career_changes, na.rm = TRUE)) %>% arrange(-total) }
      , "# GWWC pledges"                       = function(df) var_summary(df$num_pledges, verbose = TRUE)
      , "GWWC pledges by group"                = function(df) { df %>% group_by(group_name) %>% select(num_pledges) %>% summarise(total = min(num_pledges, na.rm = TRUE)) %>% arrange(-total) }
      , "GWWC pledges by LEAN"                 = function(df) { df %>% group_by(outside_help_lean) %>% select(num_pledges) %>% summarise(total = sum(num_pledges, na.rm = TRUE)) %>% arrange(-total) }
      , "GWWC pledges by LEAN group"           = function(df) { df %>% filter(outside_help_lean == "LEAN") %>% group_by(group_name) %>% select(num_pledges) %>% summarise(total = min(num_pledges, na.rm = TRUE)) %>% arrange(-total) }
      , "# non-counterfactual pledges"         = function(df) var_summary(df$num_noncounterfactual_pledges, verbose = TRUE)
      , "# counterfactual pledges"             = function(df) var_summary(df$num_counterfactual_pledges, verbose = TRUE)
      , "Counterfactual pledges by group"      = function(df) { df %>% group_by(group_name) %>% select(num_counterfactual_pledges) %>% summarise(total = min(num_counterfactual_pledges, na.rm = TRUE)) %>% arrange(-total) }
      , "Counterfactual pledges by LEAN"       = function(df) { df %>% group_by(outside_help_lean) %>% select(num_counterfactual_pledges) %>% summarise(total = sum(num_counterfactual_pledges, na.rm = TRUE)) %>% arrange(-total) }
      , "Counterfactual pledges by LEAN group" = function(df) { df %>% filter(outside_help_lean == "LEAN") %>% group_by(group_name) %>% select(num_counterfactual_pledges) %>% summarise(total = min(num_counterfactual_pledges, na.rm = TRUE)) %>% arrange(-total) }
      , "survive without organizers"           = function(df) tab(df, survive_without_current_organizers)
      , "has active EAs"                       = function(df) tab(df, has_active_eas)
      , "was EA before?"                       = function(df) tab(df, prior_ea, is_member, na.rm = TRUE, percent = TRUE, byrow = FALSE)
      , "is EA group important for EA?"        = function(df) tab(df, ea_group_important_for_ea, percent = TRUE, na.rm = TRUE)
      , "is EA important for career?"          = function(df) tab(df, ea_important_for_career, percent = TRUE, na.rm = TRUE)
      , "has EA group changed thoughts?"       = function(df) tab(df, changed_thoughts_more_impact, ea_group_important_for_thoughts, percent = TRUE, na.rm = TRUE, byrow = FALSE)
      , "is group valuable?"                   = function(df) tab(df, group_valuable, is_member, na.rm = TRUE, percent = TRUE, byrow = FALSE)
      , "motivator - top motivator"            = function(df) tab(df, motivators_top, is_member, percent = TRUE, na.rm = TRUE, byrow = FALSE)
      , "motivator - enjoyable activities"     = function(df) var_summary(df$motivators_rank_enjoyable_activities)
      , "motivator - having responsibility"    = function(df) var_summary(df$motivators_rank_having_responsibility)
      , "motivator - interacting with EAs"     = function(df) var_summary(df$motivators_rank_interacting_with_eas)
      , "motivator - interesting content"      = function(df) var_summary(df$motivators_rank_interesting_content)
      , "member GWWC"                          = function(df) tab(df, member_gwwc, is_member, percent = TRUE, na.rm = TRUE, byrow = FALSE)
      , "has written EA content"               = function(df) tab(df, has_written_ea, is_member, percent = TRUE, na.rm = TRUE, byrow = FALSE)
      , "has attended EAG"                     = function(df) tab(df, has_attended_eag, is_member, na.rm = TRUE)
      , "has attended EAGx"                    = function(df) tab(df, has_attended_eagx, is_member, na.rm = TRUE)
      , "reads EA Forum"                       = function(df) tab(df, is_ea_forum_reader, is_member, na.rm = TRUE)
      , "top cause"                            = function(df) tab(df, top_cause, is_member, na.rm = TRUE, percent = TRUE, byrow = FALSE)
      , "top cause II"                         = function(df) ctab(df, top_cause, is_member, na.rm = TRUE)
    )
  )
)
