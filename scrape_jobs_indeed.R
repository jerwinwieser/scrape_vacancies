library(dplyr)
library(purrr)
library(rvest)
library(tidyr)
library(stringr)

dtime_start <- paste("scraping started : ", Sys.time())

scrape_jobs_indeed <- function(job_number) {
  
  # job_number <- 1
  
  url <- paste0("https://nl.indeed.com/vacatures?q=data&start=", as.character(job_number))
  print(paste("scraping :", url))
  
  html <- url %>% 
    read_html()
  
  df_jobs <- html %>% 
    html_nodes('h2') %>% 
    html_text2() %>% 
    as_tibble() %>% 
    mutate(title = str_replace(value, "nieuw\n", ""), .keep = "none")
  
  str_link <- "date"
  
  df_date <- html %>% 
    html_nodes(xpath = paste0('//*[@class="', str_link, '"]')) %>% 
    html_text2() %>% 
    as_tibble() %>% 
    rename(date = value)
  
  html_href <- html %>% 
    html_nodes(".tapItem") %>% 
    html_attr("href") %>% 
    as_tibble() %>% 
    mutate(across(value, ~ paste0("https://nl.indeed.com", value)))
  
  df_out <- bind_cols(df_jobs, df_date, html_href)
  
  return(df_out)
  
}

job_numbers <- seq(1,25)*10

jobs <- job_numbers %>% 
  map_dfr(scrape_jobs_indeed, .id = "job_page_start") %>% 
  mutate(days_ago = strtoi(str_extract_all(date, "(\\d)+")),
         days_ago = if_else(is.na(days_ago), 0L, days_ago),
         days_ago_str = paste(days_ago, "days ago")) %>% 
  arrange(days_ago) %>% 
  filter(days_ago < 2) %>% 
  distinct(title, days_ago_str, value)

dtime_finish <- paste("scraping finished: ", Sys.time())

output_file <- "~/jobs/jobs_indeed.txt"

write.table(jobs, output_file, append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE)
write.table(dtime_start, output_file, append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE)
write.table(dtime_finish, output_file, append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE)
