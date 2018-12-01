library(sf)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(magrittr)
library(gganimate)
library(transformr)
library(haterzmapper)

# traffic accident data
# https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-traffic-accidents

# neighborhoods data
# https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-statistical-neighborhoods

# demographic data
# https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-census-neighborhood-demographics-2010

# hud data
# https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-hud-income-levels

# road data
# https://catalog.data.gov/dataset/tiger-line-shapefile-2012-county-denver-county-co-all-roads-county-based-shapefile
# https://www.census.gov/geo/reference/rttyp.html


accid <- st_read("~/Documents/side_projects/denver_accidents/data/accidents/traffic_accidents.shp")
nhoods <- st_read("~/Documents/side_projects/denver_accidents/data/neighborhoods/statistical_neighborhoods.shp")
hud <- st_read("~/Documents/side_projects/denver_accidents/data/hud/hud_income_levels.shp")
roads <- st_read("~/Documents/side_projects/denver_accidents/data/roads/tl_2012_08031_roads.shp")
dem2010 <- read_csv("~/Documents/side_projects/denver_accidents/data/census_neighborhood_demographics_2010.csv")

accid %>% 
  st_set_geometry(NULL) %>% 
  group_by(NEIGHBORHO) %>% 
  tally() %>%
  left_join(select(dem2010, NBRHD_NAME, POPULATION_2010, HISPANIC_2010, 
                   WHITE_2010, BLACK_2010, NATIVEAM_2010, ASIAN_2010, 
                   HAWPACIS_2010, OTHER_2010), by = c('NEIGHBORHO' = 'NBRHD_NAME')) %>%
  mutate(accident_norm = n/POPULATION_2010) %>%
  left_join(nhoods, by = c('NEIGHBORHO' = 'NBHD_NAME')) %>%
  ggplot() + 
  geom_sf(aes(fill = accident_norm), color = 'grey', size = .1) +
  geom_sf(data = filter(roads, RTTYP == "S"), color = '#d0d1e6', size = .5) +
  geom_sf(data = filter(roads, RTTYP == "I"), color = '#f7f7f7', size = 1) +
  map_theme_soft() +
  scale_fill_gradient2(low = '#fde0dd', mid = '#fa9fb5', high = '#c51b8a', midpoint = 1)


nohwy <- accid %>% 
  st_set_geometry(NULL) %>% 
  group_by(NEIGHBORHO) %>% 
  tally() %>%
  left_join(select(dem2010, NBRHD_NAME, POPULATION_2010, HISPANIC_2010, 
                   WHITE_2010, BLACK_2010, NATIVEAM_2010, ASIAN_2010, 
                   HAWPACIS_2010, OTHER_2010), by = c('NEIGHBORHO' = 'NBRHD_NAME')) %>%
  mutate_at(vars(HISPANIC_2010:OTHER_2010), funs(./POPULATION_2010)) %>%
  rename(Hispanic = HISPANIC_2010, white = WHITE_2010, black = BLACK_2010, 
         `Native American` = NATIVEAM_2010, Asian = ASIAN_2010, 
         `Hawaiian/Pacific Islander` = HAWPACIS_2010, Other = OTHER_2010) %>% 
  gather(race_ethnicity, val, -c(NEIGHBORHO,POPULATION_2010, n)) %>% 
  left_join(nhoods, by = c('NEIGHBORHO' = 'NBHD_NAME')) %>% 
  ggplot() +
  geom_sf(aes(fill = val), color = 'grey', size = .1) + 
  scale_fill_gradient(low = '#e5f5f9', high = '#2ca25f') +
  #scale_color_gradient2(low = '#fde0dd', mid = '#fa9fb5', high = '#c51b8a', midpoint = .5) +
  map_theme_soft() + 
  labs(title = "Race/Ethnicity: {closest_state}") +
  transition_states(
    race_ethnicity, 
    transition_length = 2,
    state_length = 3
  ) 

# race/ethnicity plot
hwyplot <- accid %>% 
  st_set_geometry(NULL) %>% 
  group_by(NEIGHBORHO) %>% 
  tally() %>%
  left_join(select(dem2010, NBRHD_NAME, POPULATION_2010, HISPANIC_2010, 
                   WHITE_2010, BLACK_2010, NATIVEAM_2010, ASIAN_2010, 
                   HAWPACIS_2010, OTHER_2010), by = c('NEIGHBORHO' = 'NBRHD_NAME')) %>%
  mutate_at(vars(HISPANIC_2010:OTHER_2010), funs(./POPULATION_2010)) %>%
  rename(Hispanic = HISPANIC_2010, white = WHITE_2010, black = BLACK_2010, 
         `Native American` = NATIVEAM_2010, Asian = ASIAN_2010, 
         `Hawaiian/Pacific Islander` = HAWPACIS_2010, Other = OTHER_2010) %>% 
  gather(race_ethnicity, val, -c(NEIGHBORHO,POPULATION_2010, n)) %>% 
  left_join(nhoods, by = c('NEIGHBORHO' = 'NBHD_NAME')) %>% 
  ggplot() +
  geom_sf(aes(fill = val), color = 'grey', size = .1) + 
  geom_sf(data = filter(roads, RTTYP == "I"), color = "#dde023", size = .9, alpha = .8) +
  geom_sf(data = filter(roads, RTTYP == "U"), color = "#c6c924", size = .5, alpha = .8) +
  scale_fill_gradient(low = '#e5f5f9', high = '#2ca25f') +
  #scale_color_gradient2(low = '#fde0dd', mid = '#fa9fb5', high = '#c51b8a', midpoint = .5) +
  map_theme_soft() + 
  labs(title = "Race/Ethnicity: {closest_state}") +
  transition_states(
    race_ethnicity, 
    transition_length = 2,
    state_length = 3
  ) 

hud %>%
  mutate(low_perc = LOW/LOWMODUNIV) %>%
  ggplot() +
  geom_sf(aes(fill = low_perc), color = 'grey', size = .01) +
  geom_sf(data = filter(roads, RTTYP == "I"), color = "#dde023", size = .9, alpha = .8) +
  geom_sf(data = filter(roads, RTTYP == "U"), color = "#c6c924", size = .5, alpha = .8) +
  scale_fill_gradient(low = '#e5f5f9', high = '#2ca25f') +
  map_theme_soft()


hwy_intersections <- st_intersects(hud, st_transform(filter(roads, RTTYP == "I" | RTTYP == "U"), crs = 4326), sparse = F)

hwy_inter_bool <- apply(X = hwy_intersections, MARGIN = 1, FUN = function(x){any(x == TRUE)})

hud_nongeo <- st_set_geometry(hud, NULL)
hud_road <- hud_nongeo[hwy_inter_bool,] %>% left_join(hud) %>% mutate(low_perc = LOW/LOWMODUNIV)
hud_noroad <- hud_nongeo[!hwy_inter_bool,] %>% left_join(hud) %>% mutate(low_perc = LOW/LOWMODUNIV)

mean_hud_road <- hud_road %$% mean(low_perc, na.rm = T)
mean_hud_noroad <- hud_noroad  %$% mean(low_perc, na.rm = T)

ggplot() +
  geom_density(data = hud_noroad, aes(x = low_perc), color = '#2ca25f', fill = '#2ca25f', alpha = .6) +
  geom_vline(data = hud_noroad, xintercept = mean_hud_noroad, color = '#2ca25f') +
  geom_density(data = hud_road, aes(x = low_perc), color = '#c6c924', fill = '#c6c924', alpha = .6) +
  geom_vline(data = hud_road, xintercept = mean_hud_road, color = '#c6c924') +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'percentage of low-income households')
