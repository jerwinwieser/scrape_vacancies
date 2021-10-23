library(dplyr)
library(purrr)
library(rvest)
library(tidyr)
library(stringr)

dtime_start <- paste("scraping started : ", Sys.time())

url <- "https://www.randstad.nl/vacatures?pagina=1"

n_jobs <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@class="jobboardbar__resultcounter"]') %>%
  html_text() %>%
  str_extract(pattern = "(?<=\\().*(?=\\))") %>%
  str_sub(-5,-1) %>%
  str_replace("\\.", "") %>%
  strtoi()

n_pages <- ceiling(n_jobs / 15)

read_jobs <- function(page) {
  url <- paste0("https://www.randstad.nl/vacatures?pagina=", as.character(page))
  print(paste0("scraping : ", url))
  html <- url %>%
    read_html()
  
  df_jobs <- html %>% 
    html_nodes(xpath = '//*[@class="vacancy-tile"]') %>%
    html_text2() %>%
    as_tibble()
  
  df_links <- html %>% 
    html_nodes(xpath = '//*[@class="vacancy-tile__titlelink"]') %>% 
    html_attr('href') %>% 
    as_tibble() %>% 
    mutate(link = paste0("https://www.randstad.nl", value), .keep = "none")
  
  df_out <- bind_cols(df_jobs, df_links)
  
  return(df_out)
}

jobs <- seq(1,n_pages) %>%
  map_dfr(read_jobs, .id = "page") %>%
  filter(str_detect(tolower(value), "data")) %>%
  separate(col = value, into = c("title"), sep = "\n") %>% 
  select(-page) %>% 
  mutate(link = paste0("\n", link))

dtime_finish <- paste("scraping finished: ", Sys.time())

output_file <- "~/jobs/jobs.txt"

write.table(jobs, output_file, append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE)

write.table(dtime_start, output_file, append = TRUE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)

write.table(dtime_finish, output_file, append = TRUE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
