library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggrepel)

covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(location, date, total_cases) %>%
  filter(location %in% c("Europe", "Germany", "United Kingdom", "France", "Spain", "United States"))

covid_data_tbl %>%
  
  #plot cases over date
  ggplot(aes(x = date, y = total_cases, color = location)) +
  
  #line plot
  geom_line() +
  
  #scales for the x and y axis
  scale_x_date(labels = scales::label_date(format = "%b '%y"), date_breaks = "1 month") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, prefix = "", suffix = " M")) +
  
  #add axis labels and title
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = glue("As of {date}",
                    date=format(max(covid_data_tbl$date),"%d/%m/%Y")),
    caption = "Challenge 1",
    x = "",
    y = "Cumulative Cases",
    color = "Continent / Country",
  )+
  
  #choose minimal theme and modify
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust =1),
        plot.title = element_text(size=12),
        plot.subtitle  = element_text(size=9),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        )

  

  

