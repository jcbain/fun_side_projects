# The Geography of Opportunity Part 1 Analysis
# 
# Author: James Bain <jamescbain@gmail.com>
# Date: 2018 December 07
#
# This analysis belongs to this post https://jcbain.github.io/portfolio/denver1/

library(sf)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(brms)
library(magrittr)
library(gganimate)
library(transformr)
library(haterzmapper)
library(extrafont)

# uncomment this lines to import fonts path will need to match the path where 
# fonts are stored on your machine.
# font_import(paths = "~/Library/Fonts/skyfonts-google/")

# ==============================================================================
# data
# ==============================================================================
# traffic accident data
# these data were not used for this analysis but it is still interesting none 
# the less
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


# ==============================================================================
# load data
# ==============================================================================
accid <- st_read("~/Documents/side_projects/denver_accidents/data/accidents/traffic_accidents.shp")
nhoods <- st_read("~/Documents/side_projects/denver_accidents/data/neighborhoods/statistical_neighborhoods.shp")
hud <- st_read("~/Documents/side_projects/denver_accidents/data/hud/hud_income_levels.shp")
roads <- st_read("~/Documents/side_projects/denver_accidents/data/roads/tl_2012_08031_roads.shp")
dem2010 <- read_csv("~/Documents/side_projects/denver_accidents/data/census_neighborhood_demographics_2010.csv")


# race/ethnicity highway map using gganimate -----------------------------------
# this map uses ggannimate to cycle through the different racial and ethnic 
# demographic proportions per neightborhood in the city
hwyplot <- accid %>% 
  st_set_geometry(NULL) %>% 
  group_by(NEIGHBORHO) %>% 
  tally() %>%
  left_join(select(dem2010, NBRHD_NAME, POPULATION_2010, HISPANIC_2010, 
                   WHITE_2010, BLACK_2010, NATIVEAM_2010, ASIAN_2010, 
                   HAWPACIS_2010, OTHER_2010), 
            by = c('NEIGHBORHO' = 'NBRHD_NAME')) %>%
  mutate_at(vars(HISPANIC_2010:OTHER_2010), funs(./POPULATION_2010)) %>%
  rename(Hispanic = HISPANIC_2010, white = WHITE_2010, black = BLACK_2010, 
         `Native American` = NATIVEAM_2010, Asian = ASIAN_2010, 
         `Hawaiian/Pacific Islander` = HAWPACIS_2010, Other = OTHER_2010) %>% 
  gather(race_ethnicity, val, -c(NEIGHBORHO,POPULATION_2010, n)) %>% 
  left_join(nhoods, by = c('NEIGHBORHO' = 'NBHD_NAME')) %>% 
  ggplot() +
  geom_sf(aes(fill = val), color = 'grey', size = .1) + 
  geom_sf(data = filter(roads, RTTYP == "I"), 
          color = "#dde023", size = .9, alpha = .8) +
  geom_sf(data = filter(roads, RTTYP == "U"), 
          color = "#c6c924", size = .5, alpha = .8) +
  scale_fill_gradient(low = '#e5f5f9', high = '#2ca25f') +
  map_theme_soft() + 
  theme(text=element_text(family="Montserrat")) +
  labs(title = "Race/Ethnicity: {closest_state}") +
  transition_states(
    race_ethnicity, 
    transition_length = 2,
    state_length = 3
  ) 

# poverty proportion per census tract ------------------------------------------
# this map first creates a variable of the proportion of demographic poor per 
# census tract.
hud %>%
  mutate(low_perc = LOW/LOWMODUNIV) %>%
  ggplot() +
  geom_sf(aes(fill = low_perc), color = 'grey', size = .01) +
  geom_sf(data = filter(roads, RTTYP == "I"), 
          color = "#dde023", size = .9, alpha = .8) +
  geom_sf(data = filter(roads, RTTYP == "U"), 
          color = "#c6c924", size = .5, alpha = .8) +
  scale_fill_gradient(low = '#e5f5f9', high = '#2ca25f') +
  map_theme_soft()

# ==============================================================================
# find the tracts that touch a highway
# ==============================================================================
hwy_intersections <- st_intersects(hud, st_transform(
  filter(roads, RTTYP == "I" | RTTYP == "U"
         ), crs = 4326), sparse = F)
hwy_inter_bool <- apply(X = hwy_intersections, 
                        MARGIN = 1, 
                        FUN = function(x){any(x == TRUE)})

hud_nongeo <- st_set_geometry(hud, NULL)
hud_road <- hud_nongeo[hwy_inter_bool,] %>% left_join(hud) %>% 
  mutate(low_perc = LOW/LOWMODUNIV)
hud_noroad <- hud_nongeo[!hwy_inter_bool,] %>% left_join(hud) %>% 
  mutate(low_perc = LOW/LOWMODUNIV)

mean_hud_road <- hud_road %$% mean(low_perc, na.rm = T)
mean_hud_noroad <- hud_noroad  %$% mean(low_perc, na.rm = T)

touches <- st_touches(st_as_sf(hud_noroad), st_as_sf(hud_road), sparse = F)
touches_bool <- apply(X = touches, 
                      MARGIN = 1, 
                      FUN = function(x){any(x == TRUE)})
hud_touches <- hud_noroad[touches_bool,]

mean_hud_touches <- hud_touches %$% mean(low_perc, na.rm = T)

# highway tract touchers -------------------------------------------------------
# this map shows those tracts that touch highways in denver and those tracts 
# that touch tracts that touch highways.
ggplot() +
  geom_sf(data = st_as_sf(hud), fill = 'grey', 
          color = 'grey', size = .2, alpha = .2) +
  geom_sf(data = st_as_sf(hud_road), fill = '#c6c924', 
          color = 'grey', size = .2, alpha = .7) +
  geom_sf(data = st_as_sf(hud_touches), fill = '#2ca25f', 
          color = 'grey', size = .2, alpha = .7) +
  geom_sf(data = filter(roads, RTTYP == "I"), 
          color = "#ffab84", size = .9, alpha = .8) +
  geom_sf(data = filter(roads, RTTYP == "U"), 
          color = "#ffab84", size = .5, alpha = .8) +
  map_theme_soft() +
  ggtitle(label = "Census Tracts that Touch Highways") +
  theme(text=element_text(family="Montserrat")) 

# boxplot of poverty for touch/no-touch tracts ---------------------------------
# blot the boxplot of low-income proportion for the different types of tracts:
# those that touch a highway, those that touch a tract that touches a tract that 
# touches a highway and those that don't touch.
ggplot(rbind(mutate(hud_road, cat = 'road'), 
             mutate(hud_noroad, cat = 'no road'), 
             mutate(hud_touches, cat = 'touches'))) +
  geom_boxplot(aes(x = cat, y = low_perc, color = cat )) + 
  theme_bw() + 
  scale_color_manual(values = c('#2ca25f', "#ffab84", '#c6c924')) +
  theme(legend.title = element_blank(), 
        axis.title.x = element_blank()) +
  ggtitle(label = 'Proportion of Low-Income Households') +
  theme(text=element_text(family="Montserrat"), 
        legend.position = "none",
        axis.title.y = element_blank()) 

# visualize the most prominent race/ethnicity per neighborhood -----------------
select(dem2010, NBRHD_NAME, POPULATION_2010, HISPANIC_2010, 
       WHITE_2010, BLACK_2010, NATIVEAM_2010, ASIAN_2010, 
       HAWPACIS_2010, OTHER_2010) %>%
  gather(race_ethnicity, val, -c(POPULATION_2010, NBRHD_NAME)) %>%
  group_by(NBRHD_NAME) %>%
  filter(val == max(val)) %>% View()

# ==============================================================================
# race/ethnicity analysis
# ==============================================================================

# data setup -------------------------------------------------------------------
hud_props <-hud %>% st_intersection(nhoods) %>% st_set_geometry(NULL) %>%
  mutate(low_perc = LOW/LOWMODUNIV) %>%
  group_by(NBHD_NAME) %>%
  summarize(mean_low = mean(low_perc, na.rm = T)) %>% 
  left_join(select(dem2010, NBRHD_NAME, POPULATION_2010, HISPANIC_2010, 
                   WHITE_2010, BLACK_2010, NATIVEAM_2010, ASIAN_2010, 
                   HAWPACIS_2010, OTHER_2010) %>%
              gather(race_ethnicity, val, -c(POPULATION_2010, NBRHD_NAME)), 
            by = c('NBHD_NAME' = 'NBRHD_NAME')) 


mod_dat_h <- hud_props %>%
  group_by(NBHD_NAME) %>%
  filter(val == max(val)) %>%
  mutate(prop = val/POPULATION_2010) %>% 
  filter(race_ethnicity == 'HISPANIC_2010') 

mod_dat_w <- hud_props %>%
  group_by(NBHD_NAME) %>%
  filter(val == max(val)) %>%
  mutate(prop = val/POPULATION_2010) %>% 
  filter(race_ethnicity == 'WHITE_2010') 

mod_dat <- hud_props %>%
  group_by(NBHD_NAME) %>%
  filter(val == max(val)) %>%
  mutate(prop = val/POPULATION_2010) %>%
  filter(race_ethnicity != 'BLACK_2010')

# map of ethnicity and race
mod_dat %>% left_join(nhoods) %>%
  ggplot() + 
  geom_sf(aes(fill = race_ethnicity, alpha = prop), size = .1) +
  geom_sf(data = filter(roads, RTTYP == "I"), 
          color = "#dde023", size = .9, alpha = .8) +
  geom_sf(data = filter(roads, RTTYP == "U"), 
          color = "#c6c924", size = .5, alpha = .8) +
  map_theme_soft() +
  scale_fill_manual(values = c('#ffab84', '#2ca25f')) +
  ggtitle(label = "Majority Race/Ethnicity Neighborhoods") +
  theme(legend.position="none",
        text=element_text(family="Montserrat"))

# bayes linear models ----------------------------------------------------------
# mw = predominantly white neighborhoods and poverty model
# mh = predominantly hispanic neighborhoods and poverty model
mw <- brm(data = mod_dat_w, family = gaussian, 
    mean_low ~ prop,
    prior = c(prior(normal(0, 10), class = b),
              prior(uniform(0, 10), class = sigma)),
    iter = 2000, warmup = 500, chains = 4, cores = 4)

mh <- brm(data = mod_dat_h, family = gaussian, 
          mean_low ~ 1 + prop,
          prior = c(prior(normal(0, 10), class = b),
                    prior(uniform(0, 10), class = sigma)),
          iter = 2000, warmup = 500, chains = 4, cores = 4)

# density plot of posterior distributions --------------------------------------
dens_plot <- tibble(
  x = seq(-1, 1, by = .0001),
  Posterior_1 = dnorm(x, fixef(mw)[2,1], fixef(mw)[2,2]), 
  Posterior_2 = dnorm(x, fixef(mh)[2,1], fixef(mh)[2,2])) %>%
  ggplot() +
  geom_density(aes(x = x, y = Posterior_1), stat = 'identity', 
               color = '#2ca25f', fill = '#2ca25f', alpha = .5) +
  geom_density(aes(x = x, y = Posterior_2), stat = 'identity', 
               color = '#ffab84', fill = '#ffab84', alpha = .5) + +
  theme_bw() +
  labs(x = "Posterior Slope Probabilities", y = "Density") +
  theme(text=element_text(family="Montserrat"))

dens_build <- ggplot_build(dens_plot)$data

dens_plot +
  geom_area(data = subset(dens_build[[2]], x <= (fixef(mh)[2,1] + 
                                                   (2 * fixef(mh)[2,2])) & 
                            x >= (fixef(mh)[2,1] - (2 * fixef(mh)[2,2]))), 
            aes(x = x, y = y), alpha = .5, fill = '#ffab84', 
            color = '#ffab84') +
  geom_area(data = subset(dens_build[[1]], x <= (fixef(mw)[2,1] + 
                                                   (2 * fixef(mw)[2,2])) & 
                            x >= (fixef(mw)[2,1] - (2 * fixef(mw)[2,2]))), 
            aes(x = x, y = y), alpha = .5, fill = '#2ca25f',
            color = '#2ca25f') +
  geom_hline(yintercept = 0) +
  ggtitle(label = "Posterior Slope Probabilities") +
  theme(axis.title.x = element_blank())
  

# visualize the relationship between low-income housing and proportion of a race
hud_props %>%
  group_by(NBHD_NAME) %>%
  filter(val == max(val)) %>%
  mutate(prop = val/POPULATION_2010) %>%
  ggplot(aes(x = prop, y = mean_low, color = race_ethnicity)) + 
  geom_point() + 
  geom_abline(intercept = fixef(mh)[1,1], slope = fixef(mh)[2,1], 
              color = '#ffab84') + 
  geom_abline(intercept = fixef(mw)[1,1], slope = fixef(mw)[2,1], 
              color = '#2ca25f') + 
  geom_abline(intercept = fixef(mw)[1,1], slope = fixef(mw)[2,1], 
              color = '#2ca25f') + 
  theme_bw() +
  scale_color_manual(values = c('#c6c924',"#ffab84" ,'#2ca25f' )) +
  labs(x = "Race/Ethnicity Population Proportion", 
       y = "Neighborhood Mean Low Income") +
  theme(text=element_text(family="Montserrat")) +
  theme(legend.position="none")
