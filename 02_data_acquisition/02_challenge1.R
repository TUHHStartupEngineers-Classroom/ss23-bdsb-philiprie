#import libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(RSQLite)
library(DBI)

#access API
resp <- GET("https://datausa.io/api/data?drilldowns=Nation&measures=Population")

#check if API responded successfully
if (resp$status_code == 200) {
  print("request has succeeded")
}

#extract information and form it into a tibble
US_data_lst <- resp %>%
  .$content %>%
  rawToChar() %>%
  fromJSON()

data_lst <- US_data_lst["data"]$data
data_tbl <- as_tibble(data_lst) %>%
  select(Year, Population)

#plot data
data_tbl %>%
  ggplot(aes(x = Year, y = Population)) +
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = "")) +
  labs(
    title    = "US Population per year",
    x = "", # Override defaults for x and y
    y = "Inhabitants"
  )
