library(tidyverse)

diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>%  pivot_longer(cols=-1,
                           names_to = 'year',
                           values_to = 'price') 
%>% head(n=5)
