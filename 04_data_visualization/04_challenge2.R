library("tidyverse")
library("readxl")
library("lubridate")
library("data.table")
library("vroom")
library("scales")
library("ggthemes")
library("gapminder")
library("ggplot2")
library("forcats")
library("readxl")
library("ggrepel")
library("glue")

world <- map_data("world")

covid_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  mutate(location = case_when(
    
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
    
  )) %>%
  distinct() %>%
  group_by(location) %>%
  slice(which.max(as.Date(date))) %>%
  select(location, total_deaths, population,date) %>%
  mutate(death_rate = total_deaths/population)

cov_data_world <-  world %>%
  left_join(covid_data, by = c("region"="location")) %>%
  select(long, lat, group, order, region, subregion, everything())
  
cov_data_world %>% ggplot() +
  geom_map( aes(long, lat, map_id = region, fill = death_rate), 
            map = cov_data_world,
            color = "grey",
            size = 0.09
  )+
  scale_fill_gradient(low = "#FFC6C6", high = "#FF0000", na.value = "grey")
 