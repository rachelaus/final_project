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

# joining datasets

joined_data <- bernie_data %>% full_join(joe_data)

# converting dates/times of joined data to just dates

joined_data$pub_date <- as.Date(substr(joined_data$pub_date,1,10))

# removing duplicate titles

joined_data <- joined_data %>% distinct(headline.main, .keep_all = TRUE)


# save joined data as data file

saveRDS(joined_data,file="joined_data.rds")
