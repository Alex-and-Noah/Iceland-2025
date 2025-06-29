library("dplyr")
library("ggplot2")
library("osmdata")
library("sf")

# the bounding box, limiting what we fetch
location <- getbb("iceland")

# the big streets
streets <- location %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk",  "primary", 
                            "secondary", "tertiary")) %>% 
  osmdata_sf()

xlimit <- c(-25, -21)
ylimit <- c(65, 67)
xmid <- xlimit[1] + diff(xlimit) / 2 
ratio <- diff(xlimit) / diff(ylimit)


#geojsson

# Geospatial data available at the geojson format
tmp_geojson <- tempfile(fileext = ".geojson")

download.file(
"https://raw.githubusercontent.com/baldurh/iceland-geodata/master/country/100/iceland.geojson",  tmp_geojson
)


my_sf <- read_sf(tmp_geojson)
my_sf <- st_transform(my_sf, crs = st_crs(streets$osm_lines))





map <- ggplot() +
  geom_sf(data = my_sf, fill = "#e9dfbe", color = "transparent") +
  # large steets, making them stand out a bit with colour
  geom_sf(data = streets$osm_lines, alpha = .8,
          linewidth = .6, colour = "#8b1414") +
  # setting limits
  coord_sf(ylim = ylimit, xlim = xlimit, expand = FALSE) +
  # finishing touches
  theme_void()

map
