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
  group_by(age_range)

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
  ) %>% pivot_longer(-race_ethnicity, names_to = "news_source", values_to = "num_news") %>% as_factor(levels = "labels")

saveRDS(viewbyrace, file="viewbyrace.rds")