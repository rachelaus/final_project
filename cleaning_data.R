library(tidyverse)

# cleaning Bernie data

bernie_data <- bernie_scrape 

colnames(bernie_data) <- str_replace(colnames(bernie_data),
                                     pattern='response\\.',replace='')
colnames(bernie_data) <- str_replace(colnames(bernie_data),
                                     pattern='docs\\.',replace='')

bernie_data <- bernie_data %>% mutate(candidate = "Bernie Sanders") %>%
  select(web_url, snippet, lead_paragraph, source, pub_date, type_of_material, headline.main, headline.print_headline, candidate) %>%
  arrange(pub_date)

# cleaning Biden data

joe_data <- joe_biden 

colnames(joe_data) <- str_replace(colnames(joe_data),
                                  pattern='response\\.',replace='')
colnames(joe_data) <- str_replace(colnames(joe_data),
                                  pattern='docs\\.',replace='')

joe_data <- joe_data %>% mutate(candidate = "Joe Biden") %>% select(web_url, snippet, lead_paragraph, source, pub_date, type_of_material, headline.main, headline.print_headline, candidate) 

joined_data_plot <- bernie_data %>% full_join(joe_data)


# converting dates/times of joined data to just dates

joined_data_plot$pub_date <- as.Date(substr(joined_data_plot$pub_date,1,10))

# joining datasets

joined_data <- bernie_data %>% full_join(joe_data)

# converting dates/times of joined data to just dates

joined_data$pub_date <- as.Date(substr(joined_data$pub_date,1,10))

# removing duplicate titles

joined_data <- joined_data %>% distinct(headline.main, .keep_all = TRUE)

# save joined data as data file

saveRDS(joined_data,file="joined_data.rds")

sort_by_date <- joined_data_plot %>%
  arrange(pub_date) %>%
  group_by(pub_date, candidate) %>%
  select(pub_date, candidate) %>%
  mutate(articles_per_date_bernie = sum(candidate == "Bernie Sanders")) %>%
  mutate(articles_per_date_joe = sum(candidate == "Joe Biden")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("articles_per_date"),names_to = "candidate2", values_to = "articles_per_date") %>%
  filter(articles_per_date != 0) %>%
  select(-candidate2)

saveRDS(sort_by_date,file="sort_by_date.rds")
