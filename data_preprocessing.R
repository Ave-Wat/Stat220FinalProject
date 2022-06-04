library(tidyverse)

residence <- read_csv('data/police_residence.csv')
cities <- read_csv('data/cities.csv')

residence %>%
  mutate(city_name = unlist(str_split(city, ','))[1])

