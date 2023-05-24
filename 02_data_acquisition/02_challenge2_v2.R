#import libraries
library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      

#scraping rose bikes for their road bike models
home_url <- "https://www.rosebikes.com"
url_roadbikes <- "https://www.rosebikes.com/bikes/road"
#xopen(url_roadbikes)


#get the url of every model of category road bike
get_urls <- function(category_url){
  
  html_roadbikes <- read_html(category_url)
  
  #get URLs for bikes of the road bike category
  bike_url_tbl <- html_roadbikes %>%
    html_nodes(css = "a.catalog-category-bikes__picture-wrapper") %>%
    html_attr("href") %>%
    enframe(name = "position", value = "url") %>%
    mutate(url = glue("{home_url}{url}"))
  
  return(bike_url_tbl)
}

bike_url_tbl <- get_urls(url_roadbikes)

#get the name and price of each model
get_bike_data <- function(url){
  html_model <- read_html(url)
  
  name <- html_model %>%
    html_nodes(css = ".product-headline-with-order-number > h1") %>%
    html_text()
  
  price <- html_model %>%
    html_nodes(css = ".bike-detail-price__wrapper > span > span") %>%
    html_text()
  
  data_tbl <- tibble(
    model_url = url,
    Name = name,
    Price = as.numeric(str_remove(price[1], ",")),
  )
  
  return(data_tbl)
}

data_tbl <- map(bike_url_tbl$url, get_bike_data)
