### UNBC Mapping in R Workshop Code
### September 19, 2023
### Led by Katie Tjaden-McClement

# Load packages
list.of.packages <- c("terra", "ggmap", "leaflet", "ggplot2", "ggspatial",
                      "cowplot", "bcmaps", "dplyr", "tidyterra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(terra)
library(ggmap)
library(leaflet)
library(tidyterra)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(dplyr)

##### load data #####
cameras <- vect("data_workshop/cameras/")
cameras # calling the object will give you a summary of the data it contains as well the # of attributes, its CRS, and more

# polygons of disturbance
cutblocks <- vect("data_workshop/cutblocks/")
cutblocks

fires <- vect("data_workshop/fires/")
fires

# raster of elevation data
elevation <- rast("data_workshop/elevation.tiff")

# summarized camera detections of lynx and snowshoe hare 
detect <- read.csv("data_workshop/lynx_hare_detections.csv")
head(detect)


##### Making Maps #####

### quickly map with base R plot()
# point data (vector)
plot(cameras) 

# polygon data (vector), specifying the colour
plot(fires, col = "red")

# continuous elevation raster data
plot(elevation)

# add multiple layers
plot(elevation) 
plot(fires, col = "red", alpha = 0.5, add = TRUE)
plot(cameras, add = TRUE)

### using ggmap package
# !! all spatial layers must have same CRS !!

# define extent of your area of interest
aoi <- cameras %>% 
  # add a buffer around the actual camera points
  # so the the base map extends beyond them (in meters)
  buffer(6000) %>% 
  # get the extent of this area (xmin, xmax, ymin, ymax)
  ext()

extent <- as.vector(aoi)

center <- as.polygons(aoi) %>% 
  centroids() %>% 
  crds()

### API Keys

# NOTE: stamen maps are no longer open access,
# so ggmap now loads basemaps from Google Maps or Stadia Maps,
# both of with require an account and registering an API key
# Both of these provide free monthly credits, which would be almost impossible to 
# exceed by loading basemaps through ggmap

### For Stadia Maps, register here:
# https://client.stadiamaps.com/signup/
# and find your API key by clicking "Manage Properties & Authentification"

# Register your Stadia Maps API key:
register_stadiamaps("YOUR-API-KEY", write = TRUE)
# when write = TRUE, you only have to do this once and it will be saved in for R environment
# make sure not to share your API key (e.g. in a script that lives on github)

### For Google Maps, register here:
# https://mapsplatform.google.com/
# and create/find your API key under "Keys and Credentials"

# Register it in R using:
register_google("YOUR-API-KEY", write = TRUE)

### Loading ggmap basemaps

## load Stadia basemap
basemap_stadia <- get_stadiamap(bbox = c(extent[[1]], extent[[3]],
                                         extent[[2]], extent[[4]]), 
                                zoom = 8, # level of detail
                                maptype = "stamen_terrain")
# other options for map type: “stamen_toner”, “stamen_toner_lite”, 
# “stamen_watercolor”, “alidade_smooth”, “alidade_smooth_dark”, 
# “outdoors”, “stamen_terrain_background”, “stamen_toner_background”, 
# “stamen_terrain_labels”, “stamen_terrain_lines”, “stamen_toner_labels”, 
# “stamen_toner_lines”

# note that higher zoom levels produce a strange grid effect...

ggmap(basemap_stadia) +
  # add camera points:
  geom_spatvector(data = cameras,
                  inherit.aes = F)

## load Google basemap
basemap_google <- get_googlemap(center = c(center[[1]], center[[2]]), 
                                zoom = 8, # how close to the center you are
                                maptype = "roadmap")
# other options for map type: "terrain", "satellite", and "hybrid"

ggmap(basemap_google) +
  # add camera points:
  geom_spatvector(data = cameras,
                  inherit.aes = F) +
  # you can only get square google basemaps, 
  # so change the extent here instead:
  lims(x = c(extent[[1]], extent[[2]]),
       y = c(extent[[3]], extent[[4]]))

# it's a bit fiddly and not intuitive, but you can change elements of the map style
style1 <- c(feature = "all",
            element = "labels", visibility = "off")
style2 <- c("&style=", feature = "landscape.natural.landcover",
            element = "geometry", saturation = "-48")
style3 <- c("&style=", feature = "landscape.natural.landcover",
            element = "geometry", lightness = "-15")
style <- c(style1, style2, style3)

basemap_google <- get_googlemap(center = c(center[[1]], center[[2]]), 
                               zoom = 8, # how close to the center you are
                               maptype = "roadmap",
                               style = style)

ggmap(basemap_google) +
  # add camera points:
  geom_spatvector(data = cameras,
                  inherit.aes = F) +
  # you can only get square google basemaps, 
  # so change the extent here instead:
  lims(x = c(extent[[1]], extent[[2]]),
       y = c(extent[[3]], extent[[4]]))

# let's use the Stadia Maps basemap from here on:
basemap <- basemap_stadia

# change colours and shapes based on a variable: camera location
ggmap(basemap) +
  geom_spatvector(data = cameras,  inherit.aes = F,
                  aes(colour = location,
                      shape = location)) +
  scale_colour_manual(values = c("blue4", "orange"))
  
### plot elevation and cameras

# elevation as the basemap
ggplot() + # you don't need to start with ggmap()
  geom_spatraster(data = elevation, inherit.aes = F) +
  geom_spatvector(data = cameras, inherit.aes = F) +
  scale_fill_viridis_c() +
  labs(fill = "Elevation") +
  theme_void() # removes axes, basemap colors, etc.
  
### Plot disturbance layers
# first create combined cutblock and road layer for efficiency and to easily make nice legend
# can just use rbind for spatvector objects with the same crs
dist <- rbind(cutblocks, fires)
plot(dist) #quick check, all looks good

cams_and_dist_map <- ggmap(basemap) +
  geom_spatvector(data = dist, inherit.aes = F,
                  aes(fill = dist_type,
                      colour = dist_type), #outline colour
                  alpha = 0.5) +
  geom_spatvector(data = cameras, inherit.aes = F,
                  aes(shape = location)) +
  scale_fill_manual(values = c("red2", "purple3")) +
  scale_colour_manual(values = c("red2", "purple3")) +
  guides(colour = "none") + # remove redundant outline colour legend
  labs(shape = "Camera traps",
       fill = "",
       x = "", y = "")
cams_and_dist_map

### Adding features using the ggspatial package
cams_and_dist_map <- cams_and_dist_map +
  # scale bar:
  annotation_scale(location="bl", width_hint=0.3) +
  # north arrow:
  annotation_north_arrow(location = "tl", 
                         height = unit(1, "cm"),
                         width = unit(1, "cm"))

### Another example with incorporating species detection data
# merge detections csv with station spatial data
detect_vect <- right_join(cameras, detect, by = "station_id",
                         keep = F) %>% 
  arrange(desc(Species)) # arrange the dataframe so that lynx will plot on top

lynx_hare_detect_map <- ggmap(basemap) +
  geom_spatvector(data = cameras, inherit.aes = F,
                   shape = 17, size = 1) +
  geom_spatvector(data = detect_vect, inherit.aes = F,
                  aes(colour = Species,
                      shape = Species,
                      size = detections),
                  alpha = 0.7) +
  scale_shape_manual(values = c(15,16)) +
  scale_size_continuous(range = c(2, 8)) + # change the range of point sizes, default ones were pretty small
  theme_void() # remove axes and other features
lynx_hare_detect_map  


##### Inset maps #####

# outline of BC
bc <- vect(bcmaps::bc_bound()) %>% 
  project("EPSG:4326")

# aoi as polygon
aoi <- as.polygons(aoi)  
crs(aoi) <- "EPSG:4326"

plot(bc)
plot(as.polygons(aoi), add = T)

inset <- ggplot() +
  geom_spatvector(data = bc, inherit.aes = F,
                  fill = "white", colour = "black") +
  geom_spatvector(data = aoi, fill = "white",
                  colour = "red", 
                  lwd = 1) +
  theme_void()

# plot along with main map using cowplot

# you'll likely need to play around with margins, legend locations, 
# and plot size for the inset to fit nicely
cams_and_dist_map_prep <- cams_and_dist_map +
  theme(plot.margin = margin(0.5, 5, 0, 0, "cm"),
        legend.position = c(1.2, 0.8),
        legend.key=element_blank())

final_dist_map <- ggdraw() +
  draw_plot(cams_and_dist_map_prep, x = 0, y = 0) +
  draw_plot(inset, x = 0.72, y = 0.15, width = 0.3, height = 0.3)
# this may look weird in the plot window! Trust the process and save
# at your desired size to see how things need to be adjusted

# save map
ggsave(final_dist_map, filename = "camera_disturbance_map.png",
       width = 18, height = 12, units = "cm")

##### Interactive Maps! #####

# NOTE: you will need the most recent version (2.2.0) of leaflet installed
# for compatibility with terra objects

# quickly make an interactive leaflet map with defaults using the plet
# function from terra, limited control of aesthetics
plet(cameras)

# specify colour, basemap tile as satellite imagery, and add popup labels
plet(cameras, col = "blue", tiles = c("Esri.WorldImagery"),
     label = T, popup = T, alpha = 0.6)

### adding multiple layers:
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolygons(data = fires, col = "red", opacity = 0.4,
              weight = 1, label = ~FIRE_YEAR) %>% 
  addCircles(data = cameras, radius = 5,
             label = ~station_id, popup = ~location)


