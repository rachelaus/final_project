library(haven)
dec26 <- read_dta("ns20191226/ns20191226.dta")

# viewing by age

viewbyage <- dec26 %>%
  
  # selected columns i needed to work with
  
  select(age, news_sources_msnbc, news_sources_cnn, news_sources_facebook, news_sources_fox, news_sources_network, 
         news_sources_localtv, news_sources_telemundo, news_sources_npr, news_sources_amtalk, news_sources_new_york_times, 
         news_sources_local_newspaper) %>% arrange(age) %>% 
  #news_sources_other_TEXT
  
  # levels for new column age category
  #mutated and created a new variable rather than overriding a currently existing
  # column
  mutate(age_range =
           case_when(
             between(age, 18, 23) ~ "18-23",
             between(age, 24, 30) ~ "24-30",
             between(age, 31, 55) ~ "31-55",
             between(age, 56, 75) ~ "56-75",
             between(age, 76, 99) ~ "76-99"
           )) %>%
  select(-age) %>%
  arrange(age_range) %>%
  group_by(age_range) %>%
  
  # counted readers
  
  summarize(msnbc = sum(news_sources_msnbc == 1), 
            new_york_times = sum(news_sources_new_york_times == 1),
            cnn = sum(news_sources_cnn == 1),
            fb = sum(news_sources_facebook == 1),
            fox = sum(news_sources_fox == 1),
            network_tv = sum(news_sources_network == 1),
            local_tv = sum(news_sources_localtv == 1), 
            telemundo = sum(news_sources_telemundo == 1), 
            npr = sum(news_sources_npr == 1), 
            am_radio = sum(news_sources_amtalk == 1), 
            local_news = sum(news_sources_local_newspaper == 1)
  ) %>%
  
  # pivoted longer to prepare for graphing and get all news source names in
  # a single column
  
  pivot_longer(-age_range, names_to = "news_source", values_to = "num_news")  %>%
  group_by(age_range) %>%
  mutate(percent = (num_news/sum(num_news))*100) %>%
  arrange(news_source) %>%
  mutate(news_source = fct_recode(news_source, 
                                  "New York Times" = "new_york_times",
                                  "AM Radio" = "am_radio",
                                  "CNN" = "cnn",
                                  "Facebook" = "fb",
                                  "Fox" = "fox",
                                  "Local News" = "local_news",
                                  "Local TV" = "local_tv",
                                  "MSNBC" = "msnbc",
                                  "Network TV" = "network_tv",
                                  "NPR" = "npr",
                                  "Telemundo" = "telemundo"
                                  
  )) 

saveRDS(viewbyage,file="viewbyage.rds")

# viewing by race


viewbyrace <- dec26 %>%
  select(race_ethnicity, news_sources_msnbc, news_sources_cnn, news_sources_facebook, news_sources_fox, news_sources_network, 
         news_sources_localtv, news_sources_telemundo, news_sources_npr, news_sources_amtalk, news_sources_new_york_times, news_sources_local_newspaper) %>%
  group_by(race_ethnicity) %>%
  summarize(msnbc = sum(news_sources_msnbc == 1), 
            new_york_times = sum(news_sources_new_york_times == 1),
            cnn = sum(news_sources_cnn == 1),
            fb = sum(news_sources_facebook == 1),
            fox = sum(news_sources_fox == 1),
            network_tv = sum(news_sources_network == 1),
            local_tv = sum(news_sources_localtv == 1), 
            telemundo = sum(news_sources_telemundo == 1), 
            npr = sum(news_sources_npr == 1), 
            am_radio = sum(news_sources_amtalk == 1), 
            local_news = sum(news_sources_local_newspaper == 1)
  ) %>% pivot_longer(-race_ethnicity, names_to = "news_source", values_to = "num_news") %>% as_factor(levels = "labels") %>%
  
  # i was recoding in the wrong spot. i moved it to the right spot! lol
  
  mutate(race_ethnicity = fct_recode(race_ethnicity, 
                                     "Asian American + Pacific Islander" = "Asian (Filipino)", 
                                     "Asian American + Pacific Islander" = "Asian (Japanese)",
                                     "Asian American + Pacific Islander" = "Asian (Chinese)",
                                     "Asian American + Pacific Islander" = "Asian (Asian Indian)",
                                     "Asian American + Pacific Islander" = "Asian (Korean)",
                                     "Asian American + Pacific Islander" = "Asian (Vietnamese)",
                                     "Asian American + Pacific Islander" = "Asian (Other)",
                                     "Asian American + Pacific Islander" = "Pacific Islander (Native Hawaiian)",
                                     "Asian American + Pacific Islander" = "Pacific Islander (Guamanian)",
                                     "Asian American + Pacific Islander" = "Pacific Islander (Samoan)",
                                     "Asian American + Pacific Islander" = "Pacific Islander (Other)"
  )) %>%
  group_by(race_ethnicity, news_source) %>%
  summarize(num_news = sum(num_news)) %>%
  mutate(percent = (num_news/sum(num_news))*100) %>%
  arrange(race_ethnicity) %>%
  mutate(news_source = fct_recode(news_source, 
                                  "New York Times" = "new_york_times",
                                  "AM Radio" = "am_radio",
                                  "CNN" = "cnn",
                                  "Facebook" = "fb",
                                  "Fox" = "fox",
                                  "Local News" = "local_news",
                                  "Local TV" = "local_tv",
                                  "MSNBC" = "msnbc",
                                  "Network TV" = "network_tv",
                                  "NPR" = "npr",
                                  "Telemundo" = "telemundo"
                                  
  )) 

saveRDS(viewbyrace, file="viewbyrace.rds")


viewbyparty <- dec26 %>%
  select(primary_party, dem_vote_intent, rank_dems_1, rank_dems_2, rank_dems_3, house_intent, news_sources_cnn, news_sources_fox, news_sources_npr) %>%
  # filter(dem_vote_intent != 888) %>%
  # filter(rank_dems_1 != 888) %>%
  # filter(rank_dems_2 != 888) %>%
  # filter(rank_dems_3 != 888)
  mutate(party =
           case_when(
             primary_party == 1 ~ "Democrat",
             primary_party == 2 ~ "Republican"
           )) %>%
  arrange(party) %>%
  group_by(party)


saveRDS(viewbyparty, file = "viewbyparty.rds")