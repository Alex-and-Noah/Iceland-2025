library("dplyr")
library("ggplot2")
library("osmdata")
library("sf")

# the bounding box, limiting what we fetch
location <- getbb("reykjavik iceland", featuretype = "city")

# the big streets
dus_streets <- location %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk",  "primary", 
                            "secondary", "tertiary")) %>% 
  osmdata_sf()

small_streets <- location %>% 
  opq() %>% 
  add_osm_feature(
    key = "highway", 
    value = c("residential", "living", "unclassified", 
              "service", "footway")) %>% 
  osmdata_sf()

boundaries <- location %>%
  opq() %>%
  add_osm_feature(key = "admin_level", value = "6") %>%
  osmdata_sf()

background <- boundaries$osm_multipolygons %>%
  filter(name != "Reykjavíkurborg")

xlimit <- c(-21.98, -21.5)
ylimit <- c(64, 64.5)
xmid <- xlimit[1] + diff(xlimit) / 2 
ratio <- diff(xlimit) / diff(ylimit)



map <- ggplot() +
  # small streets
  geom_sf(data = small_streets$osm_lines, alpha = .6,
          linewidth = .2, colour = "grey30") +
  # large steets, making them stand out a bit with colour
  geom_sf(data = dus_streets$osm_lines, alpha = .8,
          linewidth = .4, colour = "goldenrod4") +
  # adding the background
  # need to increase size here, otherwise there will be a few semi-cutoff
  # streets directly at the city border
  geom_sf(data = background, colour = "grey10",
    linewidth = 1.5, fill = "grey10") +
  # setting limits
  coord_sf(ylim = ylimit, xlim = xlimit, expand = FALSE) +
  # finishing touches
  theme_void() +
  theme(panel.background = element_rect(fill = "grey10"),
        plot.background = element_rect(fill = "grey10"))

map
