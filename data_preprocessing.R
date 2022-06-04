library(tidyverse)

#load datasets
residence <- read_csv('data/police_residence.csv')
cities <- read_csv('data/cities.csv')
demographics <- read_delim('data/us-cities-demographics.csv', delim = ';')
killings <- read_csv('data/police_killings.csv')
hate_crimes <- read_csv('data/hate_crimes.csv')

#standardize city variables accross geographical data sets for joining
residence <- residence %>%
  mutate(city = str_to_lower(str_extract(residence$city, '[^,]+(?=(,|$))')))

cities <- cities %>%
  mutate(city = str_to_lower(CITY), lat = LATITUDE, lon = LONGITUDE) %>%
  select(c('city', 'lat', 'lon'))

demographics <- demographics %>%
  mutate(city = str_to_lower(City)) %>%
  select(-c('City'))

joined_cities <- residence %>%
  left_join(cities) %>%
  left_join(demographics)
