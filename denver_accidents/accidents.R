library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(gganimate)
library(transformr)
library(haterzmapper)

# traffic accident data
# https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-traffic-accidents

# neighborhoods data
# https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-statistical-neighborhoods


accidents <- st_read("~/Documents/side_projects/denver_accidents/data/accidents/traffic_accidents.shp")
nhoods <- st_read("~/Documents/side_projects/denver_accidents/data/neighborhoods/statistical_neighborhoods.shp")


# accidents %>% filter(FIRST_OCCU == "2012-11-02" | FIRST_OCCU == "2012-11-03")  %>% ggplot() +
#   geom_sf() +   transition_states(
#     FIRST_OCCU,
#     transition_length = 2,
#     state_length = 1
#   )
