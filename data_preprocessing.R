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
  mutate(city = str_to_lower(CITY), lat = LATITUDE, lon = LONGITUDE, state_code = STATE_CODE) %>%
  select(c('city', 'lat', 'lon', state_code))

dups <- duplicated(cities %>% select(c('city', 'state_code')))

cities <- cities %>%
  filter(!dups)

demographics <- demographics %>%
  mutate(city = str_to_lower(City), state_code = `State Code`) %>%
  select(-c('City', 'State Code', 'State'))

#join location and demographic data to police residence data for all cities in residence
joined_cities <- residence %>%
  left_join(cities, by = c('city', 'state_code')) %>%
  left_join(demographics, by = c('city', 'state_code'))


