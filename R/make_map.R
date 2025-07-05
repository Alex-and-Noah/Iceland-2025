library("dplyr")
library("ggplot2")
library("osmdata")
library("sf")

# the bounding box, limiting what we fetch
location <- getbb("iceland")

# the big streets
streets <- location |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary"
    )
  ) |>
  osmdata_sf()


streets$osm_lines <- streets$osm_lines |> mutate(
  colour = case_when(
    ref %in% c(
      61,
      62,
      63,
      60,
      612,
      614, # raudisandur
      40,
      41,
      42, #f
      43,
      426, # blue lagoon
      44,
      49,
      425,
      50, # reykholt
      523, # husafell
      365
    ) ~ "white",
    ref == 35 & name == "Biskupstungnabraut" &
    osm_id %in% c(
      1321387209,
        680986267,
        8095796,
        759156517,
        170477007,
        110242584,
        374946845,
        680980971,
        170476981
    ) ~ "white",
    ref == 37 & osm_id != 192618593 ~ "white",
    ref == 36 & osm_id != 680429139 ~ "white",
    ref == 427 & osm_id %in% c(
      680429139,
      678774099,
      678784640,
      135232278,
      620396428,
      1271719459,
      1336079846,
      1336079845,
      620396430,
      678774100,
      135232279
    ) ~ "white",
    ref == 427 & name == "Austurvegur" ~ "white",
    ref == 1 & name == "Hvalfjarðargöng" ~ "white",
    ref == 1 & name == "Vesturlandsvegur" &
      !(osm_id %in% c(
        679523860,
        248532836,
        248532830,
        679529118,
        679529109,
        196285913,
        26030851,
        679532008,
        248525826,
        1092566126,
        196285910
      )
      ) ~ "white",
    .default = "grey40"
  )
)


glacier <- location |>
  opq() |>
  add_osm_feature(key = "natural", value = "glacier") |>
  osmdata_sf()


xlimit <- c(-25.5, -18.5)
ylimit <- c(62.5, 67)
xmid <- xlimit[1] + diff(xlimit) / 2
ratio <- diff(xlimit) / diff(ylimit)


# geojson

# Geospatial data available at the geojson format
tmp_geojson <- tempfile(fileext = ".geojson")

download.file(
  "https://raw.githubusercontent.com/baldurh/iceland-geodata/master/country/100/iceland.geojson",
  tmp_geojson
)


my_sf <- read_sf(tmp_geojson)
my_sf <- st_transform(my_sf, crs = st_crs(streets$osm_lines))





map <- ggplot() +
  geom_sf(data = my_sf, fill = "#333333", color = "transparent") +
  geom_sf(
    data = streets$osm_lines,
    linewidth = .2, colour = streets$osm_lines$colour
  ) +
  # glacier
  geom_sf(
    data = glacier$osm_multipolygons,
    fill = "#42494a",
    colour = "#42494a"
  ) +
  annotate(
    geom = "text", y = 63, x = xmid,
    label = "Ísland", size = 8, colour = "#bfbfbf"
  ) +
  annotate(
    geom = "errorbarh",
    xmin = xmid - 1,
    xmax = xmid + 1,
    y = 62.93,
    height = 0,
    size = 0.5,
    colour = "#7F7F7F"
  ) +
  annotate(
    geom = "text", y = 62.9, x = xmid,
    label = "Júní 2025", size = 3,
    colour = "grey50"
  ) +
  coord_sf(ylim = ylimit, xlim = xlimit, expand = FALSE) +
  # finishing touches
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#4D4D4D"),
    plot.background = element_rect(fill = "grey30")
  )

map

ggsave("./static/iceland_map.png", map, height = 8 * ratio, width = 8, dpi = 1000, bg = "grey30")
