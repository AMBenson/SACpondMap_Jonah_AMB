# Used last map (Map_SacPond_20200207) and modified for publication
# Created by Anna-Marie Benson
# Modified by Jonah L. Withers





# Libraries ---------------------------------------------------------------
library(sf)
library(ggplot2)
library(rnaturalearth)
library(cowplot)# for ggdraw to manipulat margins ets
library(ggsn)
library(dplyr)
library(rgdal)
library(sp)
library(ggpattern)
library(patchwork)





# Projections -------------------------------------------------------------
NAD83_crs <- ("+proj=utm +zone=6 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

WGS84_crs <- ("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")





# Import data -------------------------------------------------------------
# > Pond ####
# Deprecated so map and grid align with study design
# SACpond <- read_sf("./Spatial/Gravel_Pit_SAC.shp") 
SACpond <- readOGR(dsn = "./Spatial",
                   layer = "Gravel_Pit_SAC")##read vector map into a spatial object



# > Alaska ####
usa <- subset(ne_countries(scale = 'medium',
                           returnclass = 'sf'),
              admin == "United States of America")



# > Collection Sites ####
# 2018
SampleLoc2018 <- read.csv("./AllSampleLocations_20180924.csv")

# 2019
SampleLoc2019 <- read.csv("./SampleLocations2019_ElodeaStudySacPond_20210923.csv")



# > Elodea Buckets ####
ElodeaBuckets <- st_as_sf(
  data.frame(
    lon = c(7187601, 7187241),
    lat = c(468527.1, 468572.1)),
  coords = c("lat","lon"),
  crs = NAD83_crs)




  
# Reproject to UTM --------------------------------------------------------
# > Pond #### 
# Convert to WGS84
SACpond_WGS <- st_transform(SACpond %>% 
                              st_as_sf(),
                            WGS84_crs)

# Convert to UTM
SACpond_NAD <- st_transform(SACpond_WGS,
                            NAD83_crs)



# > Alaska ####
# Convert to WGS84
usa_WGS <- st_transform(usa,
                        WGS84_crs)



# > Sample points ####
# 2018
SampleLoc2018_Nad83 <- st_as_sf(SampleLoc2018,
           coords = c("x", "y"),
           crs = NAD83_crs)

# 2019
SampleLoc2019_Nad83 <- st_transform(
  st_as_sf(SampleLoc2019,
           # coords = c("x.actualsamplelocation_utm","y.actualsamplelocation_utm"),
           coords = c("x.gridmidpoint_utm", "y.gridmidpoint_utm"),
           crs = "+proj=utm +zone=6 +ellps=WGS84"),
                           crs = NAD83_crs)





# Create Grid --------------------------------------------------------------
# Deprecated. Building grid the way it was originally done to match sampling
# design.

# grid_12.5 <- st_make_grid(SACpond_NAD,
#                           cellsize = c(12.5, 12.5)) %>% # 12.5 square meters
#   st_sf(grid_id = 1:length(.))

bb = SACpond@bbox #= boundary box

# 2018
x_2018 <- seq(from = 468485.8, to = 468670.6, by = 25) # changed "to" from 468645.6 to expand grid to encompass lake boundary
y_2018 <- seq(from = 7187196.6, to = 7187612.9, by = 25) 

## create a grid of all pairs of coordinates (as a data.frame) 
xy_2018 <- expand.grid(x = x_2018, 
                  y = y_2018)

grid.pts_2018 <- SpatialPointsDataFrame(coords = xy_2018, 
                                        data = xy_2018, 
                                        proj4string = CRS(NAD83_crs))

# Make points a gridded object (i.e., TRUE or FALSE)
gridded(grid.pts_2018) <- TRUE

grid_25 <- as(grid.pts_2018, "SpatialPolygons") %>%
  st_as_sf()



# > Identify grid cells sampled 2018 ####
index_2018 <- which(lengths(st_intersects(grid_25,
                                          SampleLoc2018_Nad83)) > 0)



# > Select grid cells sampled 2018 ####
grid_sampled_2018 <- grid_25$geometry[index_2018] %>% 
  st_as_sf()



# 2019
x_2019 <- seq(from = bb[1,1], 
         to = bb[1,2], 
         by = 12.5) 

y_2019 <- seq(from = bb[2,1], 
         to = 7187612.9, 
         by = 12.5)

# create a grid of all pairs of coordinates (as a data.frame) 
xy_2019 <- expand.grid(x = x_2019, 
                  y = y_2019)

grid.pts_2019 <- SpatialPointsDataFrame(coords = xy_2019, 
                                   data = xy_2019, 
                                   proj4string = CRS(NAD83_crs))

# Make points a gridded object (i.e., TRUE or FALSE)
gridded(grid.pts_2019) <- TRUE

grid_12.5 <- as(grid.pts_2019, "SpatialPolygons") %>%
  st_as_sf()



# > Identify grid cells sampled 2019 ####
index_2019 <- which(lengths(st_intersects(grid_12.5,
                  SampleLoc2019_Nad83)) > 0)



# > Select grid cells sampled 2019 ####
grid_sampled_2019 <- grid_12.5$geometry[index_2019] %>% 
  st_as_sf()



# > Identify grid cells sampled via transect in 2019 ####
# Identify grid cells sampled. This needs to be tidied once input data is cleaned
Transects_2019 <- SampleLoc2019_Nad83 %>% 
  
  filter(Grid.Number %in% c("5", "19", "21", "22", "33", "48", "54",
                            "62", "77", "107", 
                            as.character(seq(453, 458, by = 1)), "473",
                            "488", "507", "583"))

Transect_index_2019 <- which(lengths(st_intersects(grid_12.5,
                                                   Transects_2019)) > 0)



# > Select grid cells sampled 2019 via transects ####
Transect_grid_sampled_2019 <- grid_12.5$geometry[Transect_index_2019] %>% 
  st_as_sf()





# Plot --------------------------------------------------------------------
# > Alaska map ####
Alaska_plot <- ggplot() +
  
  geom_sf(data = usa,
          fill = "white") +

  geom_sf(data = st_bbox(
    st_buffer(SACpond_WGS,
              dist = 15000)) %>% # add buffer so we can see on map
      st_as_sfc() %>%
      st_as_sf(),
    fill = "black") + 
  
  scale_x_continuous(breaks = seq(-180, -130, 
                                  by = 15)) +
  
  scale_y_continuous(breaks = seq(50, 70, 
                                  by = 5)) +
  
  coord_sf(xlim = c(-175, -130), 
           ylim = c(50, 73),
           crs = WGS84_crs) +
  
  theme(axis.title.x =  element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8.5))
  
  



# > Study area map 2018 ####
SACpond_2018_plot <- ggplot() +
  
  geom_sf(data = grid_25, 
          fill = NA) +
  
  geom_sf(data = grid_sampled_2018,
          fill = "grey") +
  
  # geom_sf(data = SampleLoc_Nad83, # Overlay actual sample points
  #   pch = 20,
  #   size = 2,
  #   color = "black") +
  
  geom_sf(data = ElodeaBuckets,
          aes(geometry = geometry),
          size = 2,
          # fill = "#6E6E6E",
          fill = "white",
          color = "black",
          pch = 23) +
  
  geom_sf(data = SACpond_NAD,
          mapping = aes(geometry = geometry),
          fill = NA) +
  
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book") +

  # ggspatial::annotation_north_arrow(
  #   location = "tr",
  #   which_north = "true",
  #   pad_x = unit(0.4, "in"),
  #   pad_y = unit(0.4, "in"),
  #   style = ggspatial::north_arrow_nautical(
  #     fill = c("grey40", "white"),
  #     line_col = "grey20",
  #     text_family = "ArcherPro Book")) +
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())



# > Study area map 2019 ####
SACpond_2019_plot <- ggplot() +
  
  geom_sf(data = grid_12.5, 
          fill = NA) +
  
  geom_sf(data = grid_sampled_2019,
          fill = "grey") +
  
  ggpattern::geom_sf_pattern(data = Transect_grid_sampled_2019,
                             pattern = 'stripe',
                             fill    = 'grey',
                             colour  = 'black') +
  
  # geom_sf(data = SampleLoc_Nad83, # Overlay actual sample points
  #   pch = 20,
  #   size = 2,
  #   color = "black") +
  
  geom_sf(data = ElodeaBuckets,
          aes(geometry = geometry),
          size = 2,
          fill = "white",
          color = "black",
          pch = 23) +

  geom_sf(data = SACpond_NAD,
          mapping = aes(geometry = geometry),
          fill = NA) +
  
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book") +
  # 
  # ggspatial::annotation_north_arrow(
  #   location = "tr", 
  #   which_north = "true",
  #   pad_x = unit(0.4, "in"), 
  #   pad_y = unit(0.4, "in"),
  #   style = ggspatial::north_arrow_nautical(
  #     fill = c("grey40", "white"),
  #     line_col = "grey20",
  #     text_family = "ArcherPro Book")) +
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())



# # > North Elodea Bucket ####
# Elodea_N_plot <- ggplot() +
#   
#   geom_sf(data = SampleLoc2019_Nad83 %>% 
#             filter(!Grid.Number %in% seq(from = 1,
#                                          to = 1000,
#                                          by = 1)),
#           pch = 20,
#           size = 2,
#           color = "black") +
#   
#   geom_sf(data = ElodeaBuckets,
#           aes(geometry = geometry),
#           size = 2,
#           fill = "#6E6E6E",
#           color = "black",
#           pch = 23) +
#   
#   coord_sf(xlim = c(468524.1, 468530.1),       
#            ylim = c(7187594, 7187601),
#            crs = NAD83_crs) +
#   
#   ggspatial::annotation_scale(
#     location = "tr",
#     bar_cols = c("grey60", "white"),
#     text_family = "ArcherPro Book") +
# 
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         rect = element_blank(),
#         plot.background = element_rect(color = "black", linewidth = 1))
# 
# 
# 
# # > South Elodea Bucket ####
# Elodea_S_plot <- ggplot() +
#   
#   geom_sf(data = SampleLoc2019_Nad83 %>% 
#             filter(!Grid.Number %in% seq(from = 1,
#                                          to = 1000,
#                                          by = 1)),
#           pch = 20,
#           size = 2,
#           color = "black") +
#   
#   geom_sf(data = ElodeaBuckets,
#           aes(geometry = geometry),
#           size = 2,
#           fill = "#6E6E6E",
#           color = "black",
#           pch = 23) +
#   
#   coord_sf(xlim = c(468569.7, 468574.7), 
#            ylim = c(7187241, 7187247),
#            crs = NAD83_crs) +
#   
#   ggspatial::annotation_scale(
#     location = "tr",
#     bar_cols = c("grey60", "white"),
#     text_family = "ArcherPro Book") +
# 
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         rect = element_blank(),
#         plot.background = element_rect(color = "black", linewidth = 1))
# 
# 
# # > Put it together ####
# # 2018
# SACpond_2018_InsetMap <- ggdraw(xlim = c(0, 28),
#                                 ylim = c(0, 20)) +
#   
#   draw_plot(Alaska_plot,
#             x = 0,
#             y = 10,
#             width = 10,
#             height = 10) +
#   
#   draw_plot(SACpond_2018_plot,
#             x = 8,
#             y = 0,
#             width = 15, # 23,
#             height = 20) + #20) +
#   
#    geom_segment(aes(x = x2,
#                    y = y2,
#                    xend = x1,
#                    yend = y1),
#                data = data.frame(x1 = 6.5,
#                                  x2 = 7.5,
#                                  y1 = 16.4,
#                                  y2 = 17.4),
#                arrow = arrow(type = "closed",
#                              length = unit(.1, "inches")),
#                lineend = "butt",
#                linewidth = 0.5) 
#   
# ggsave(filename = "./Figures/SACpond_2018_InsetMap.jpg",
#        plot = SACpond_2018_InsetMap,
#        dpi = 600,
#        width = 8,
#        height = 7)
# 
# 
# 
# # 2019
# SACpond_2019_InsetMap <- ggdraw(xlim = c(0, 28),
#                            ylim = c(0, 20)) +
#   
#   draw_plot(Alaska_plot,
#             x = 0,
#             y = 10,
#             width = 10,
#             height = 10) +
#   
#   draw_plot(SACpond_2019_plot,
#             x = 8,
#             y = 0,
#             width = 15, # 23,
#             height = 20) + #20) +
#   
#   draw_plot(Elodea_N_plot,
#             x = 22,
#             y = 10,
#             width = 5,
#             height = 10) +
#   
#   draw_plot(Elodea_S_plot,
#             x = 22,
#             y = 0,
#             width = 5,
#             height = 10) +
#   
#   geom_segment(aes(x = x2,
#                    y = y2,
#                    xend = x1,
#                    yend = y1),
#                data = data.frame(x1 = 6.5,
#                                  x2 = 7.5,
#                                  y1 = 16.4,
#                                  y2 = 17.4),
#                arrow = arrow(type = "closed",
#                              length = unit(.1, "inches")),
#                lineend = "butt",
#                linewidth = 0.5) +
#   
#   geom_segment(aes(x = x2,
#                    y = y2,
#                    xend = x1,
#                    yend = y1),
#                data = data.frame(x1 = 22,
#                                  x2 = 13,
#                                  y1 = 15,
#                                  y2 = 18.2),
#                arrow = arrow(type = "closed",
#                              length = unit(.1, "inches")),
#                lineend = "butt",
#                linewidth = 0.5) +
#   
#   geom_segment(aes(x = x2,
#                    y = y2,
#                    xend = x1,
#                    yend = y1),
#                data = data.frame(x1 = 22,
#                                  x2 = 15.2,
#                                  y1 = 5.5,
#                                  y2 = 3.6),
#                arrow = arrow(type = "closed",
#                              length = unit(.1, "inches")),
#                lineend = "butt",
#                linewidth = 0.5)
# 
# ggsave(filename = "./Figures/SACpond_2019_InsetMap.jpg",
#        plot = SACpond_2019_InsetMap,
#        dpi = 600,
#        width = 8,
#        height = 7)



# # Combo
# Combo_plot <- cowplot::plot_grid(SACpond_2018_plot,
#                                  SACpond_2019_plot,
#                                  cowplot::plot_grid(Alaska_plot, NA,
#                                                     nrow = 2,
#                                                     rel_heights = c(.5, .5)),
#                                  ncol = 3,
#                                  rel_widths = c(.4, .4, .2),
#                                  rel_heights = c(1, 1, .5))
# 
# ggsave(filename = "./Figures/SACpond_ComboMap.jpg",
#        plot = Combo_plot,
#        dpi = 600,
#        width = 8,
#        height = 7)


Combo_plot <- ggdraw(xlim = c(0, 28),
                      ylim = c(0, 20)) +
  
  draw_plot(Alaska_plot +
              ggspatial::annotation_north_arrow(
                location = "br",
                which_north = "true",
                pad_x = unit(0.1, "in"),
                pad_y = unit(0.1, "in"),
                style = ggspatial::north_arrow_nautical(
                  fill = c("grey40", "white"),
                  line_col = "grey20",
                  text_family = "ArcherPro Book")),
            x = 0,
            y = 0,
            width = 28, 
            height = 20) + 
  
  geom_segment(aes(x = x2,
                   y = y2,
                   xend = x1,
                   yend = y1),
               data = data.frame(x1 = 27,
                                 x2 = 18,
                                 y1 = 13,
                                 y2 = 11.5),
               arrow = arrow(type = "closed",
                             length = unit(.1, "inches")),
               lineend = "butt",
               linewidth = 0.5) + 
  
  SACpond_2018_plot + 
  
  SACpond_2019_plot

ggsave(filename = "./Figures/SACpond_ComboMap.jpg",
       plot = Combo_plot,
       dpi = 600,
       width = 8,
       height = 7)