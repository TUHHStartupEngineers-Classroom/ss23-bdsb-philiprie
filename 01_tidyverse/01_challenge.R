# Load libraries ----
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

# Importing Files ----
bikes_tbl <- read_excel(path="01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel(path = "01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel(path = "01_bike_sales/01_raw_data/bikeshops.xlsx")

# Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Wrangling Data ----
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  #seperate location column
  separate(col = location,
           into = c("city","state"),
           sep = ",") %>%
  mutate(total.price = price * quantity)

# Sales by Location ----

# Step 1 - Manipulate
sales_by_loc_tbl <- bike_orderlines_wrangled_tbl %>%
  select(state, total.price) %>%
  group_by(state) %>%
  summarize(sales = sum(total.price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " € "))

# Step 2 - Visualize
sales_by_loc_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = sales)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  #geom_label(aes(label = sales_text)) + # Adding labels to the bars
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state ",
    subtitle = "Total revenue of the years 2015 to 2019",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# Sales by Year and Location ----

# Step 1 - Manipulate
sales_by_year_loc_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order.date, total.price, state) %>%
  mutate(year = year(order.date)) %>%
  group_by(year, state) %>%
  summarize(sales = sum(total.price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " € "))
# Step 2 - Visualize
sales_by_year_loc_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and location",
    subtitle = "",
    fill = "State" # Changes the legend name
  )


#  Writing Files ----

bike_orderlines_wrangled_tbl %>%
  write_xlsx("01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("01_bike_sales/02_wrangled_data/bike_orderlines.rds")
