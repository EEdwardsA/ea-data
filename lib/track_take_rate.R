dat <- read_csv("data/2018/2018-ea-survey-confidential-no-anon.csv")

referrers <- list("173783193" = "EA Newsletter",
                  "173783119" = "EA Forum",
                  "173774580" = "Friends Link",
                  "173783473" = "80K",
                  "173783593" = "Optimal Memes",
                  "174130875" = "Past Takers",
                  "173783213" = "EA FB",
                  "174085661" = "EA FB Local Groups",
                  "173783349" = "CFAR Alumni",
                  "173783172" = "SSC")

dat$referrer_url <- swap_by_value(dat, "Collector ID", referrers)[["Collector ID"]]
dat$referrer_url <- ifelse(dat$referrer_url %in% referrers, dat$referrer_url, "Other")
dat$date <- as.Date(dat[["Start Date"]], "%m/%d/%y %H:%M")
dat$week <- as.integer(strftime(dat$date, "%V")) - 17


tab(dat, week)


dat %>%
  group_by(week, referrer_url) %>%
  count()


TODAY_IS_DAY_NUM <- 6
dat[, "date"] %>%
  mutate(day_num = as.numeric(dat$date - min(dat$date))) %>%
  filter(day_num <= quo(TODAY_IS_DAY_NUM)) %>%
  nrow
