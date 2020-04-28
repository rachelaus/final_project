library(tidyverse)
library(rvest)
library(jsonlite)

NYTIMES_KEY <- "nytimes_api"

term <- "Bernie Sanders"
begin_date <- "20200101"
end_date <- "20200417"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")


initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

bernie <- vector("list")

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  bernie[[i+1]] <- nytSearch 
  Sys.sleep(5)
}

bernie_scrape <- rbind_pages(bernie)

save(bernie_scrape,file="bernie_scrape.Rdata")


# Earlier Bernie data

term <- "Bernie Sanders"
begin_date <- "20190901"
end_date <- "20191001"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")


initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

bernie_early <- vector("list")

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  bernie_early[[i+1]] <- nytSearch 
  
}

early <- rbind_pages(bernie_early)

save(early,file="bernie_early.Rdata")


# Joe Biden data

term <- "Joe Biden"
begin_date <- "20200101"
end_date <- "20200417"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")


initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

joe <- vector("list")

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  joe[[i+1]] <- nytSearch 
}

joe_biden <- rbind_pages(joe)

save(joe_biden,file="joe_biden.Rdata")

