library("dplyr")
library("ggplot2")
library("osmdata")
library("sf")
library("shadowtext")
library("dots")
library("rlang")

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
) |> filter(colour == "white")

xlimit <- c(-25.2, -13)
ylimit <- c(63.2, 66.7)
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


glacier <- location |>
  opq() |>
  add_osm_feature(key = "natural", value = "glacier") |>
  osmdata_sf()

locations_multipolygons <- location |>
  opq() |>
  add_osm_features(
    features = list (
        "name:en" = "Keflavík International Airport",
        "name:en" = "	Reykjavik",
        "name:en" = "Þingvellir National Park",
        "name" = "Rauðasandur",
        "name" = "Reykjanes",
        "name:en" = "Blue Lagoon",
        "name" = "Fagradalsfjall",
        # "name" = "National Museum of Iceland",
        # "name:en" = "Grótta lighthouse",
        # "name" = "Ylströndin í Nauthólsvík",
        "name" = "Álftafjörður"
    )
  ) |>
  osmdata_sf()

numbers_multipolygons <- locations_multipolygons$osm_multipolygons |>
  as.data.frame() |>
  mutate(
    number = case_when(
      `name:en` == "Keflavík International Airport" ~ "1,26",
      `name:en` == "	Reykjavik" ~ "2,6,25",
      `name:en` == "Þingvellir National Park" ~ "3",
      `name` == "Rauðasandur" ~ "10",
      `name` == "Reykjanes" ~ "21,24",
      # `name:en` == "Blue Lagoon" ~ "21",
      # `name` == "Fagradalsfjall" ~ "23",
      # `name` == "National Museum of Iceland" ~ "PaleTurquoise",
      # `name:en` == "Grótta lighthouse" ~ "PaleTurquoise",
      # `name` == "Ylströndin í Nauthólsvík" ~ "PaleTurquoise",
      `name` == "Álftafjörður" ~ "16",
      .default = "null"
    )
  ) |>
  filter(
    number != "null"
  )

locations_polygons <- location |>
  opq() |>
  add_osm_features(
    features = list (
        "name" = "Harbour Inn Guesthouse",
        "name" = "Einarshús Guesthouse",
        "name" = "Museum of Sorcery and Witchcraft"
    )
  ) |>
  osmdata_sf()

numbers_polygons <- locations_polygons$osm_polygons |>
  as.data.frame() |>
  mutate(
    number = case_when(
      `name` == "Harbour Inn Guesthouse" ~ "12",
      `name` == "Einarshús Guesthouse" ~ "15",
      `name` == "Museum of Sorcery and Witchcraft" ~ "18",
      .default = "null"
    )
  ) |>
  filter(
    number != "null"
  )

locations_multilines <- location |>
  opq() |>
  add_osm_features(
    features = list (
        "name" = "Látrabjarg"
    )
  ) |>
  osmdata_sf()

numbers_multilines <- locations_multilines$osm_multilines |>
  as.data.frame() |>
  mutate(
    number = case_when(
      `name` == "Látrabjarg" ~ "11",
      .default = "null"
    )
  ) |>
  filter(
    number != "null"
  )

locations_lines <- location |>
  opq() |>
  add_osm_features(
    features = list (
        "name:en" = "Bridge Between Continents"
    )
  ) |>
  osmdata_sf()

numbers_lines <- locations_lines$osm_lines |>
  as.data.frame() |>
  mutate(
    number = case_when(
      `name:en` == "Bridge Between Continents" ~ "24",
      .default = "null"
    )
  ) |>
  filter(
    number != "null"
  )


locations_points <- location |>
  opq() |>
  add_osm_features(
    features = list (
        "name:en" = "Geysir",
        "name:en" = "Gullfoss",
        "name"= "Víðgelmir",
        "name:en" = "Gil Guesthouse",
        "alt_name" = "Fjallfoss",
        "name" = "Tónlistarskóli Ísafjarðar",
        "name" = "Grindavík",
        "name" = "Northern Light Inn",
        "name" = "Sky Lagoon",
        "name" = "The Sheep Farming Museum",
        "name" = "Dalahótel",
        "name" = "Barnafoss"
    )
  ) |>
  osmdata_sf()

numbers_points <- locations_points$osm_points |>
  as.data.frame() |>
  mutate(
    number = case_when(
      `name:en` == "Geysir" ~ "4",
      `name:en` == "Gullfoss" ~ "5",
      `name` == "Víðgelmir" ~ "8",
      `name:en` == "Gil Guesthouse" ~ "9",
      `alt_name` == "Fjallfoss" ~ "13",
      `name` == "Tónlistarskóli Ísafjarðar" ~ "14",
      # `name` == "Grindavík" ~ "19",
      `name` == "Northern Light Inn" ~ "22,23",
      # `name` == "Sky Lagoon" ~ "23",
      `name` == "The Sheep Farming Museum" ~ "17,18",
      `name` == "Dalahótel" ~ "19,20",
      `name` == "Barnafoss" ~ "7",
      .default = "null"
    )
  ) |>
  filter(
    number != "null"
  )

  # Barnafoss/hraunfossar
  # Sheep museum
  # Witchcraft museum

geom_sf_shadowtext <- function(
  mapping = aes(),
  data = NULL,
  stat = "sf_coordinates",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  fun.geometry = NULL
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "Both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied.",
        i = "Only use one approach to alter the position."
      ))
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer_sf(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomShadowText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    )
  )
}

size = 2

map <- ggplot() +
  geom_sf(data = my_sf, fill = "grey20", color = "transparent") +
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
  # Locations - multipolygons
  # geom_sf(
  #   data = locations_multipolygons$osm_multipolygons,
  #   fill = colours_multipolygons$colour,
  #   colour = colours_multipolygons$colour
  # ) +
  geom_sf_shadowtext(
    data = numbers_multipolygons |> st_as_sf(),
    aes(
      label = number
    ),
    size = size
  ) + 
  # Locations - polygons
  # geom_sf(
  #   data = locations_polygons$osm_polygons,
  #   fill = colours_polygons$colour,
  #   colour = colours_polygons$colour
  # ) +
  geom_sf_shadowtext(
    data = numbers_polygons |> st_as_sf(),
    aes(
      label = number
    ),
    size = size
  ) + 
  # Locations - multilines
  # geom_sf(
  #   data = locations_multilines$osm_multilines,
  #   fill = colours_multilines$colour,
  #   colour = colours_multilines$colour
  # ) +
  geom_sf_shadowtext(
    data = numbers_multilines |> st_as_sf(),
    aes(
      label = number
    ),
    size = size
  ) + 
  # Locations - lines
  # geom_sf(
  #   data = locations_lines$osm_lines,
  #   fill = colours_lines$colour,
  #   colour = colours_lines$colour
  # ) +
  # geom_sf_text(
  #   data = numbers_lines |> st_as_sf(),
  #   aes(
  #     label = number,
  #     colour = "Red"
  #   )
  # ) + 
  # Locations - points
  # geom_sf(
  #   data = locations_points$osm_points,
  #   fill = colours_points$colour,
  #   colour = colours_points$colour
  # ) +
  geom_sf_shadowtext(
    data = numbers_points |> st_as_sf(),
    aes(
      label = number
    ),
    size = size
  ) + 
  stat_sf_coordinates() + 
  coord_sf(ylim = ylimit, xlim = xlimit, expand = FALSE) +
  # finishing touches
  theme_void() +
  theme(
    # plot.background = element_rect(fill = "grey30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )

map

ggsave("./static/route_with_landmarks.png", map, dpi = 1000)
