library(rvest)
library(lubridate)
library(tibble)
library(dplyr)

# max page chk

max_page <- function(tar){
  read_html(tar) %>% 
    html_nodes("div.paging_comm .link_page") %>% 
    html_text() %>% 
    as.numeric() %>% 
    max() %>% 
    return()
}

tar <- "https://news.joins.com/politics/bluehouse/list/1?filter=OnlyJoongang&date=2019-01-10"

max_page(tar)

tar_url <- "https://news.joins.com/politics/bluehouse/list/1?filter=OnlyJoongang&date=2019-01-10"

max <- max_page(tar)

root <- "https://news.joins.com"

articles <- c()

for (i in 1:max) {

  tar_url <- paste0("https://news.joins.com/politics/bluehouse/list/",i,"?date=2019-01-10")
  print(tar_url)
  read_html(tar_url) %>% 
    html_nodes("strong.headline a") %>% 
    html_attr("href") -> link_list
  
  for (j in 1:length(link_list)) {
    print(paste0(i,j))
    tar <- paste0(root, link_list[j])
    print(tar)
    news <- read_html(tar)
    news %>% 
      html_nodes("h1#article_title") %>% 
      html_text() -> title
    
    news %>% 
      html_nodes("div.byline") %>% 
      as.character() %>% 
      strsplit("em") %>% 
      .[[1]] %>% 
      grep("입력", ., value = T) %>% 
      gsub(">|입력|<|/", "", .) %>% 
      trimws() %>% 
      ymd_hm(tz = "Asia/Seoul") -> datetime
    
    news %>% 
      html_nodes("span.profile strong a") %>% 
      html_text() %>% 
      .[1] -> reporter
    
    news %>% 
      html_nodes("div#article_body") %>% 
      as.character() %>% 
      strsplit("<br>|</div>") %>% 
      .[[1]] %>% 
      trimws("both") -> body_tem
    body_tem <- body_tem[-grep("(</|<!)",body_tem)] 
    body <- body_tem[nchar(body_tem) > 1]
    
    tem <- tibble(title, datetime, reporter, body)
    articles %>% 
      bind_rows(tem) -> articles
      
  }
  
}








