#import libraries
library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      

#scraping rose bikes for their road bike models
home_url <- "https://www.rosebikes.com"
url_roadbikes <- "https://www.rosebikes.com/bikes/road"

#get the url of the 3 models of the category road bike and write them into a tibble 
get_urls <- function(category_url){
  
  html_roadbikes <- read_html(category_url)
  
  bike_url_tbl <- html_roadbikes %>%
    html_nodes(css = "a.catalog-category-bikes__picture-wrapper") %>%
    html_attr("href") %>%
    enframe(name = "position", value = "url") %>%
    mutate(url = glue("{home_url}{url}"))
  
  return(bike_url_tbl)
}

bike_url_tbl <- get_urls(url_roadbikes)

#get the name and price of each models different versions
get_bike_data <- function(url){
  html_model <- read_html(url)
  
  # Extract the name and price using CSS selectors
  name <- html_model %>%
    html_node(css = ".basic-headline__title") %>%
    html_text() %>%
    str_trim()
  
  price <- html_model %>%
    html_node(css = ".catalog-category-model__price-current-value") %>%
    html_text() %>%
    str_trim()
  
  # Return the name and price as a list
  list(name = name, price = price)
}
data <- get_bike_data(bike_url_tbl$url[1])
#data <- map(bike_url_tbl$url, get_bike_data)