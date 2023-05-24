library(jsonlite)
library(purrr)

bike_data_lst <- fromJSON("bike_data.json")
# Open the data by clicking on it in the environment or by running View()
# View(bike_data_lst)

bike_data_lst %>%
  pluck("productDetail", "variationAttributes", "values", 1, "displayValue")