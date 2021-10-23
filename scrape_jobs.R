library(dplyr)
library(purrr)
library(rvest)
library(tidyr)
library(stringr)

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
  url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@class="vacancy-tile"]') %>%
    html_text2() %>%
    as_tibble()
}

jobs <- seq(1,n_pages) %>%
  map_dfr(read_jobs, .id = "page") %>%
  filter(str_detect(tolower(value), "data")) %>%
  separate(col = value, into = c("title"), sep = "\n")

write.table(jobs, "./jobs.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE)
