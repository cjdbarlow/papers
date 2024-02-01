# Libraries
library(tidyverse)
library(lubridate)
library(MatchIt)
library(MatchThem)
library(sf)
library(patchwork)

source("functions.R")

# 0 Data ----
## Matched data
data.list = readRDS("matched-data.list.Rds")

data = data.list %>%
    map_df(as.data.frame,
           .id = ".imp")

select = readRDS("select.Rds")

## Jurisdiction shapefiles
jur.nz = st_read("../Resources/nz-dhb/nz-district-health-boards-2012.shp") %>%
    st_transform(crs = 4326) %>%
    #st_simplify(dTolerance = 100) %>%
    summarise(jurisdictionname = "NZ") %>%
    select(jurisdictionname, geometry)

jur.aus = st_read("../Resources/aus-states/STE_2021_AUST_GDA2020.shp") %>%
    st_transform(crs = 4326) %>%
    st_simplify(dTolerance = 1000) %>%
    mutate(jurisdictionname = case_when(STE_NAME21 == "Victoria" ~ "VIC",
                                        STE_NAME21 == "New South Wales" ~ "NSW",
                                        STE_NAME21 == "Queensland" ~ "QLD",
                                        STE_NAME21 == "South Australia" ~ "SA",
                                        STE_NAME21 == "Western Australia" ~ "WA",
                                        STE_NAME21 == "Tasmania" ~ "TAS",
                                        STE_NAME21 == "Northern Territory" ~ "NT",
                                        STE_NAME21 == "Australian Capital Territory" ~ "ACT")) %>%
    filter(STE_CODE21 <= 8) %>%
    select(jurisdictionname, geometry)


## ARIA shapefiles
remote = st_read("../Resources/aus-remote/RA_2016_AUST.shp") %>%
    st_transform(crs = 4326) %>%
    st_simplify(dTolerance = 100) %>%
    mutate(remoteness_cat = case_when(RA_NAME16 == "Major Cities of Australia" ~ "Major Cities",
                                      RA_NAME16 == "Inner Regional Australia" ~ "Inner Regional",
                                      RA_NAME16 == "Outer Regional Australia" ~ "Outer Regional",
                                      RA_NAME16 == "Remote Australia" ~ "Remote",
                                      RA_NAME16 == "Very Remote Australia" ~ "Very Remote")) %>%
    filter(!is.na(remoteness_cat))

all_nz = st_read("../Resources/nz-dhb/nz-district-health-boards-2012.shp") %>%
    summarise(NAME = "NZ",
              do_union = TRUE) %>%
    st_simplify(dTolerance = 1000) %>%
    st_transform(crs = 4326) 


## Fragility indices
### Calculate time difference of dataset so we can annualise fragility index
time = select %>%
    select(icu_ad_dtm, icu_ds_dtm) %>%
    summarise(ad = min(icu_ad_dtm, na.rm = TRUE),
              dc = max(icu_ds_dtm, na.rm = TRUE)) %>%
    mutate(dur = interval(ad, dc)/years(1))

### Load and calculate fragility index
frag.aria = list.files(path = "./fragility",
                       pattern = "frag-aria[0-9]*.Rds",
                       full.names = TRUE) %>%
    map_dfr(readRDS) %>%
    group_by(level) %>%
    summarise(index_mean = mean(index),
              index_min = min(index),
              index_max = max(index)) %>%
    # Annualise and round it
    mutate(across(where(is.numeric), ~./time$dur),
           across(where(is.numeric), round, digits = 0)) %>%
    rename(remoteness_cat = level)


# 1 Remoteness ----
## Set up data
remote = remote %>%
    left_join(frag.aria)

loc = tribble(~ city, ~ lat      , ~ long     , ~ rad,
              "mel" , -37.8142354, 144.9668884, 180000,
              "syd" , -33.8386302, 151.0310312, 140000,
              "bri" , -27.4671551, 153.0169995, 100000)

## Make big map
map.aria = NULL %>%
    ggplot() +
    geom_sf(data = remote,
            aes(fill = index_mean),
            lwd = 0.05,
            colour = "white") +
    # Put points in for cities that get insets
    geom_point(data = loc,
               aes(x = long, y = lat),
               size = 0.75) +
    # Themeing
    labs(fill = "ECMO Case Disparity \n (cases/year)") +
    scale_fill_distiller(type = "div",
                         palette = 7) +
    theme_void() +
    theme(legend.position = c(0.1, 0.5),
          legend.justification = "left")


## Make maps of Melbourne/Sydney/Brisbane
fn.mini_map = function(st = remote, lat, long, rad){
    # Define circle centered on a point, with a certain radius. We will restrict the main map to this area.
    circle = tibble(lat = lat,
                    long = long) %>% 
        st_as_sf(coords = c("long", "lat"),
                 crs = 4326) %>%
        st_transform(crs = 6384) %>% 
        st_buffer(dist = rad) %>% 
        st_transform(crs = 4326)
    
    # Take the map and crop it to the circle
    remote.zoom = remote %>%
        st_intersection(circle)
    
    # Make a pretty border for the inset
    border = tibble(lat = lat,
                    long = long) %>% 
        st_as_sf(coords = c("long", "lat"),
                 crs = 4326)  %>%
        st_transform(crs = 6384) %>% 
        st_buffer(dist = rad + 10) %>% 
        st_transform(crs = 4326)
    
    # Make a ggplot of same
    mini.map = NULL %>%
        ggplot() +
        geom_sf(aes(fill = index_mean),
                data = remote.zoom,
                lwd = 0.05,
                colour = "white") +
        geom_sf(data = border,
                color = "black", alpha = 0) +
    # Themeing
        scale_fill_distiller(type = "div",
                             palette = 7) +
        theme_void() +
        theme(legend.position = "none")
    
    mini.map
}

map.aria_mel = fn.mini_map(lat = loc[loc$city == "mel",]$lat,
                           long = loc[loc$city == "mel",]$long,
                           rad = loc[loc$city == "mel",]$rad)

map.aria_syd = fn.mini_map(lat = loc[loc$city == "syd",]$lat,
                           long = loc[loc$city == "syd",]$long,
                           rad = loc[loc$city == "syd",]$rad)

map.aria_bri = fn.mini_map(lat = loc[loc$city == "bri",]$lat,
                           long = loc[loc$city == "bri",]$long,
                           rad = loc[loc$city == "bri",]$rad)


# Extract the map scale so we can make points at a certain percentage of the range
## We will use this to draw lines so our insets are connected in a pretty way
map.scale = data.frame(x.lo = min(layer_scales(map.aria)$x$range$range),
                       x.hi = max(layer_scales(map.aria)$x$range$range),
                       y.lo = min(layer_scales(map.aria)$y$range$range),
                       y.hi = max(layer_scales(map.aria)$y$range$range)) %>%
    mutate(x.dif = x.hi - x.lo,
           y.dif = y.hi - y.lo)

map.aria = map.aria +
    # Brisbane
    geom_segment(data = map.scale,
                 aes(x = x.lo + x.dif * 0.85,
                     y = y.lo + y.dif * 0.54),
                 xend = loc[loc$city == "bri",]$long,
                 yend = loc[loc$city == "bri",]$lat,
                 size = 0.5) +
    # Sydney
    geom_segment(data = map.scale,
                 aes(x = x.lo + x.dif * 0.82,
                     y = y.lo + y.dif * 0.23),
                 xend = loc[loc$city == "syd",]$long,
                 yend = loc[loc$city == "syd",]$lat,
                 size = 0.5) +
    # Melbourne
    geom_segment(data = map.scale,
                 # Little correction to stop the segement end being visible in Port Phillip bay
                 aes(x = (x.lo + x.dif * 0.55) * 1.005,
                     y = (y.lo + y.dif * 0.05) * 1.005),
                 xend = loc[loc$city == "mel",]$long,
                 yend = loc[loc$city == "mel",]$lat,
                 size = 0.5)
map.aria

# All together now
map.aria_full = map.aria + 
    inset_element(map.aria_bri,
                  left = 0.75, bottom = 0.48, right = 0.95, top = 0.68) + 
    inset_element(map.aria_syd,
                  left = 0.72, bottom = 0.13, right = 0.92, top = 0.33) + 
    inset_element(map.aria_mel,
                  left = 0.45, bottom = 0, right = 0.65, top = 0.2)

map.aria_full
            
ggsave("outputs/figures/map-remote.jpeg", map.aria_full, bg = "white",
       dpi = 1200, width = 12, height = 9, units = "in")
