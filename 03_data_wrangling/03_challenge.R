#import libraries----
library(tidyverse)
library(vroom)
library(data.table)
library(tidyr)
#import reduced data set----

#patent.tsv
col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# assignee.tsv

col_types <- list(
  id = col_character(),
  type = col_number(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# patent_assignee.tsv

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# uspc.tsv

col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_double()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

#patent dominance ----
#match the assigners to their patents
patent_assignee_joined_tbl <- patent_assignee_tbl %>% 
  left_join(assignee_tbl, by = c("assignee_id" = "id"))

#filter for US companies (type=2), group and count the number of patents and then reorganize
patent_us_tbl <- patent_assignee_joined_tbl %>%
  filter(type == 2) %>%
  group_by(organization) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

patent_us_tbl %>%  head(n = 10)

#recent patent activity ----
#add the date and number of claims to the tibble in order to be able to filter for August 2014
patent_august14_tbl <- patent_assignee_joined_tbl %>% 
  left_join(patent_tbl, by = c("patent_id" = "id"))

#split the date category
patent_august14_ymd_tbl <- patent_august14_tbl %>%
  mutate(day  = lubridate::day(date), month = lubridate::month(date), year = lubridate::year(date))
  
#Somehow the method with separate() did not work
#separate(col = date,
#           into = c("year", "month", "day"),
#           sep = "-")

#repeat the steps from the first task plus an additional filter for the year and month
patent_august_tbl <- patent_august14_ymd_tbl %>%
  filter(year == 2014) %>%
  filter(month == 08) %>%
  filter(type ==2) %>%
  group_by(organization) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

patent_august_tbl %>%  head(n = 10)

#innovation in tech ----
#add uspc tibble which includes the different classes of patents
patent_inno_tbl <- left_join(patent_assignee_joined_tbl,uspc_tbl)

#find top10 worldwide (types 2 or 3)
top10_tbl <- patent_inno_tbl %>%
  filter(type == 2 | type == 3) %>%
  group_by(organization) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  head(n=10)
  
#find the 5 top main classes
top5_classes_tbl <- patent_inno_tbl %>%
  filter( organization %in% top10_tbl$organization,  !is.na(mainclass_id)) %>%
  group_by(mainclass_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%  
  select(mainclass_id) %>%
  head(n = 5)

top5_classes_tbl