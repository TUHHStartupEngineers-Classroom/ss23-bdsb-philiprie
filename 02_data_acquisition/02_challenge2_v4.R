#import libraries
library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      

#scraping rose bikes for their road bike models
home_url <- "https://www.rosebikes.com"
url_roadbikes <- "https://www.rosebikes.com/bikes/road"

#get the url of every model of the category road bike
get_urls <- function(category_url){
  
  html_roadbikes <- read_html(category_url)
  
  bike_url_tbl <- html_roadbikes %>%
    #html_nodes(css = "a.catalog-category-bikes__picture-wrapper") %>%
    html_nodes(css = "a.catalog-category-bikes__button") %>%
    html_attr("href") %>%
    enframe(name = "position", value = "url") %>%
    mutate(url = glue("{home_url}{url}"))
  
  return(bike_url_tbl)
}

bike_url_tbl <- get_urls(url_roadbikes)

#get the name and price of each model
get_bike_data <- function(url){
  html_model <- read_html(url)
  
  # Extract the name and price using CSS selectors
  names <- html_model %>%
    #html_node(css = ".catalog-category-models__list-item") %>%
    html_node(css = ".catalog-category-model__title") %>%
    html_text() %>%
    str_trim() %>%
    enframe(name = "position", value = "name")
    
  
  prices <- html_model %>%
    #html_node(css = ".catalog-category-models__list-item") %>%
    html_node(css = ".catalog-category-model__price-current") %>%
    html_text() %>%
    str_trim() %>%
    enframe(name = "position", value = "price")

  bike_data <- names %>%
    left_join(prices, by = join_by(position))
    
  return(bike_data)
}

database <- map(bike_url_tbl$url, get_bike_data)