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





# Projections -------------------------------------------------------------
NAD83_crs <- ("+proj=utm +zone=6 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

WGS84_crs <- ("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")





# Import data -------------------------------------------------------------
# > Pond ####
SACpond <- read_sf("./Spatial/Gravel_Pit_SAC.shp") 



# > Alaska ####
usa <- subset(ne_countries(scale = 'medium',
                           returnclass = 'sf'),
              admin == "United States of America")



# > Collection Sites ####
SampleLoc1 <- read.csv("./SampleLocations2019_ElodeaStudySacPond_20210923.csv")



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
SACpond_WGS <- st_transform(SACpond,
                            WGS84_crs)

# Convert to UTM
SACpond_NAD <- st_transform(SACpond_WGS,
                            NAD83_crs)



# > Alaska ####
# Convert to WGS84
usa_WGS <- st_transform(usa,
                        WGS84_crs)



# > Sample points ####
SampleLoc_Nad83 <- st_transform(
  st_as_sf(SampleLoc1,
           coords = c("x.actualsamplelocation_utm","y.actualsamplelocation_utm"),
           crs = "+proj=utm +zone=6 +ellps=WGS84"),
                           crs = NAD83_crs)





# Create Grid --------------------------------------------------------------
grid_12.5 <- st_make_grid(SACpond_NAD,
                          cellsize = c(12.5, 12.5)) %>% # 12.5 square meters
  st_sf(grid_id = 1:length(.))



# > Identify grid cells sampled ####
index <- which(lengths(st_intersects(grid_12.5,
                  SampleLoc_Nad83)) > 0)



# > Select grid cells sampled ####
grid_sampled <- grid_12.5$.[index] %>% 
  st_as_sf()





# Plot --------------------------------------------------------------------
# > Alaska map ####
Alaska_plot <- ggplot() +
  
  geom_sf(data = usa,
          fill = "white") +

  geom_sf(data = st_bbox(
    st_buffer(SACpond_WGS,
              dist = 10000)) %>% # add buffer so we can see on map
      st_as_sfc() %>%
      st_as_sf(),
    fill = "black") + 
  
  scale_x_continuous(breaks = seq(-180, -130, 
                                  by = 10)) +
  
  scale_y_continuous(breaks = seq(50, 70, 
                                  by = 5)) +
  
  coord_sf(xlim = c(-175, -130), 
           ylim = c(50, 73),
           crs = WGS84_crs) +
  
  theme_bw()



# > Study area map ####
SACpond_plot <- ggplot() +
  
  geom_sf(data = grid_12.5, 
          fill = NA) +
  
  geom_sf(data = grid_sampled,
          fill = "grey") +
  
  geom_sf(data = SampleLoc_Nad83, # Overlay actual sample points
    pch = 20,
    size = 2,
    color = "black") +
  
  geom_sf(data = ElodeaBuckets,
          aes(geometry = geometry),
          size = 2,
          fill = "#6E6E6E",
          color = "black",
          pch = 23) +

  geom_sf(data = SACpond_NAD,
          mapping = aes(geometry = geometry),
          fill = NA) +
  
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book") +
  
  ggspatial::annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    pad_x = unit(0.4, "in"), 
    pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book")) +
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())



# > North Elodea Bucket ####
Elodea_N_plot <- ggplot() +
  
  geom_sf(data = SampleLoc_Nad83,
          pch = 20,
          size = 2,
          color = "black") +
  
  geom_sf(data = ElodeaBuckets,
          aes(geometry = geometry),
          size = 2,
          fill = "#6E6E6E",
          color = "black",
          pch = 23) +
  
  coord_sf(xlim = c(468524.1, 468530.1),       
           ylim = c(7187594, 7187601),
           crs = NAD83_crs) +
  
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book") +

  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.background = element_rect(color = "black", linewidth = 1))



# > South Elodea Bucket ####
Elodea_S_plot <- ggplot() +
  
  geom_sf(data = SampleLoc_Nad83,
          pch = 20,
          size = 2,
          color = "black") +
  
  geom_sf(data = ElodeaBuckets,
          aes(geometry = geometry),
          size = 2,
          fill = "#6E6E6E",
          color = "black",
          pch = 23) +
  
  coord_sf(xlim = c(468569.7, 468574.7), 
           ylim = c(7187241, 7187247),
           crs = NAD83_crs) +
  
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book") +

  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.background = element_rect(color = "black", linewidth = 1))


# > Put it together ####
SACpond_InsetMap <- ggdraw(xlim = c(0, 28),
                           ylim = c(0, 20)) +
  
  draw_plot(Alaska_plot,
            x = 0,
            y = 10,
            width = 10,
            height = 10) +
  
  draw_plot(SACpond_plot,
            x = 8,
            y = 0,
            width = 15, # 23,
            height = 20) + #20) +
  
  draw_plot(Elodea_N_plot,
            x = 22,
            y = 10,
            width = 5,
            height = 10) +
  
  draw_plot(Elodea_S_plot,
            x = 22,
            y = 0,
            width = 5,
            height = 10) +
  
  geom_segment(aes(x = x2,
                   y = y2,
                   xend = x1,
                   yend = y1),
               data = data.frame(x1 = 6.5,
                                 x2 = 7.5,
                                 y1 = 16.4,
                                 y2 = 17.4),
               arrow = arrow(type = "closed",
                             length = unit(.1, "inches")),
               lineend = "butt",
               linewidth = 0.5) +
  
  geom_segment(aes(x = x2,
                   y = y2,
                   xend = x1,
                   yend = y1),
               data = data.frame(x1 = 22,
                                 x2 = 13,
                                 y1 = 15,
                                 y2 = 18.2),
               arrow = arrow(type = "closed",
                             length = unit(.1, "inches")),
               lineend = "butt",
               linewidth = 0.5) +
  
  geom_segment(aes(x = x2,
                   y = y2,
                   xend = x1,
                   yend = y1),
               data = data.frame(x1 = 22,
                                 x2 = 15.2,
                                 y1 = 5.5,
                                 y2 = 3.6),
               arrow = arrow(type = "closed",
                             length = unit(.1, "inches")),
               lineend = "butt",
               linewidth = 0.5)





# ###several points slightly outside of water, so manually snapped on to pond
# 
# SampleLoc1$y.actualsamplelocation_utm[1]<-7187606
# SampleLoc1$y.actualsamplelocation_utm[2]<-7187602
# SampleLoc1$x.actualsamplelocation_utm[6]<-468515
# SampleLoc1$x.actualsamplelocation_utm[9]<-468507
# SampleLoc1$x.actualsamplelocation_utm[14]<-468504
# SampleLoc1$x.actualsamplelocation_utm[17]<-468502
# SampleLoc1$x.actualsamplelocation_utm[19]<-468498
# SampleLoc1$y.actualsamplelocation_utm[4]<-7187606
# SampleLoc1$y.actualsamplelocation_utm[5]<-7187602
# SampleLoc1$x.actualsamplelocation_utm[8]<-468566
# SampleLoc1$x.actualsamplelocation_utm[21]<-468488
# SampleLoc1$x.actualsamplelocation_utm[23]<-468490
# SampleLoc1$y.actualsamplelocation_utm[28]<-7187244
# SampleLoc1$y.actualsamplelocation_utm[29]<-7187238
# SampleLoc1$y.actualsamplelocation_utm[30]<-7187238
# SampleLoc1$y.actualsamplelocation_utm[31]<-7187240
# SampleLoc1$y.actualsamplelocation_utm[32]<-7187240
# SampleLoc1$y.actualsamplelocation_utm[33]<-7187240
# SampleLoc1$x.actualsamplelocation_utm[36]<-468570
# SampleLoc1$x.actualsamplelocation_utm[38]<-468571
# SampleLoc1$x.actualsamplelocation_utm[18]<-468610
# SampleLoc1$x.actualsamplelocation_utm[20]<-468622
# SampleLoc <- data.frame( 
#   lon = SampleLoc1$y.actualsamplelocation_utm, 
#   lat = SampleLoc1$x.actualsamplelocation_utm)