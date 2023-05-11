##used last map (Map_SacPond_20200207)and modified for publication
library(sf)
library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")

library(cowplot)# for ggdraw to manipulat margins ets
library(plotKML)#for read.GPX
library(tmap) #one of many packages for insetting maps
world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)
usa <- subset(world, admin == "United States of America")


##Alaska Map
alaska <- ggplot(data = usa) +
  geom_sf(fill = "white") +
  coord_sf(crs = st_crs(32606), 
           xlim = c(-1714134, 1581606), 
           ylim = c(5739691, 8239691), 
           expand = FALSE)
###SACpond Map

SACpond<-read_sf("C:\\Users\\AMBenson\\Documents\\CGL\\eDNA\\SACPond_Elodea_eDNA\\DataAndSamplingPlan\\Sampling\\SACpond_shp_Buzby_20190207\\Gravel_Pit_SAC.shp") 

crs<-("+proj=utm +zone=6 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
##
#transform
SACpond<- SACpond %>%
  st_transform(crs = 32606)
st_crs(SACpond)


###create an object for elodea bucket locations
ElodeaBuckets <- data.frame( 
                     lon = c(7187601, 7187241), 
                     lat = c(468527.1, 468572.1))

ElodeaBuckets<- sf::st_as_sf(ElodeaBuckets, 
                       coords = c( "lat","lon"),
                       crs=32606)
                         
                        # "+proj=utm +zone=6 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 

                       
                       ##actual sample locations on 29 August 2019

SampleLoc1<-read.csv("C:/Users/AMBenson/Documents/CGL/eDNA/SACPond_Elodea_eDNA/DataAndSamplingPlan/Sampling/August2019/ActualSampleLocations/SampleLocations2019_ElodeaStudySacPond_20210923.csv")
#View(SampleLoc1)
str(SampleLoc1)

###several points slightly outside of water, so manually snapped on to pond

SampleLoc1$y.actualsamplelocation_utm[1]<-7187606
SampleLoc1$y.actualsamplelocation_utm[2]<-7187602
SampleLoc1$x.actualsamplelocation_utm[6]<-468515
SampleLoc1$x.actualsamplelocation_utm[9]<-468507
SampleLoc1$x.actualsamplelocation_utm[14]<-468504
SampleLoc1$x.actualsamplelocation_utm[17]<-468502
SampleLoc1$x.actualsamplelocation_utm[19]<-468498
SampleLoc1$y.actualsamplelocation_utm[4]<-7187606
SampleLoc1$y.actualsamplelocation_utm[5]<-7187602
SampleLoc1$x.actualsamplelocation_utm[8]<-468566
SampleLoc1$x.actualsamplelocation_utm[21]<-468488
SampleLoc1$x.actualsamplelocation_utm[23]<-468490
SampleLoc1$y.actualsamplelocation_utm[28]<-7187244
SampleLoc1$y.actualsamplelocation_utm[29]<-7187238
SampleLoc1$y.actualsamplelocation_utm[30]<-7187238
SampleLoc1$y.actualsamplelocation_utm[31]<-7187240
SampleLoc1$y.actualsamplelocation_utm[32]<-7187240
SampleLoc1$y.actualsamplelocation_utm[33]<-7187240
SampleLoc1$x.actualsamplelocation_utm[36]<-468570
SampleLoc1$x.actualsamplelocation_utm[38]<-468571
SampleLoc1$x.actualsamplelocation_utm[18]<-468610
SampleLoc1$x.actualsamplelocation_utm[20]<-468622
SampleLoc <- data.frame( 
  lon = SampleLoc1$y.actualsamplelocation_utm, 
  lat = SampleLoc1$x.actualsamplelocation_utm)

SampleLoc<- sf::st_as_sf(SampleLoc, 
                             coords = c( "lat","lon"),
                             crs = 32606)
##Make a grid 12.5m by 12.5m
  grid_12.5<-
    st_make_grid(SACpond, cellsize = c(12.5,12.5)) %>% 
    st_sf(grid_id = 1:length(.))
  #color = "#bdbdbd") 
SACpond_map<-
  ggplot()+
  geom_sf(data = SACpond,
          mapping = aes(geometry = geometry),
          fill = "#f0f0f0") +
 
  geom_sf(data = ElodeaBuckets,
          aes(geometry = geometry),
          size = 2,
          fill ="#6E6E6E",
          color= "black",
          pch=23)+
  
    geom_sf(data = grid_12.5, fill = 'transparent')+
    
    geom_sf(data = SampleLoc,
          aes(geometry = geometry),
          pch=20,
          size = 1,
          color="black")+
    theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())
    
  #theme_bw()
  #theme(axis.text.x=element_text(angle=45,hjust=1)) 
    dark grey "#6E6E6E"

#Calculate bounding box of SACpond

SACpond_bbox<-st_as_sfc(st_bbox(SACpond))
SACpond_bbox<- st_buffer(SACpond_bbox,dist=10000)##add a buffer of 20,000 meters around SACpond so we can see on map
##Alaska Map
  alaska <- ggplot(data = usa) +
    geom_sf(fill = "white") +
    #annotate("rect", xmin = 456487.9, xmax = 460671.7, ymin = 7185187, ymax = 7199607,
             #alpha = .1,fill = "red")+
     geom_sf(data = SACpond_bbox, fill = "black", pch = 2,size=4) +
    coord_sf(crs = st_crs(32606), 
             xlim = c(-1314134, 1581606), 
             ylim = c(5739691, 8239691), 
             expand = FALSE)+
   
    theme(axis.text = element_text(size = 2)) +
    theme_bw()
  
  
  
  arrow <- data.frame(x1 = 15.8, x2 = 13.2, y1 = 12, y2 = 11.3) ##use an arrow and ggdraw
  
  SACpond_InsetMap<-
    ggdraw(xlim = c(0, 28), ylim = c(0, 20)) +
    draw_plot(alaska,x = 0, y = 0, width = 20, height = 20 )+
    draw_plot(SACpond_map,x = 15, y = 7.8, width = 5, height = 10) +
  
    geom_segment(aes(x = x2, y = y2, xend = x1, yend = y1), data = arrow, 
                 arrow = arrow(type="closed",length = unit(0.07, "inches")), lineend = "butt",size=.4) 
  
  
  ggsave(plot =  SACpond_InsetMap, width = 6, height = 4, dpi = 300, filename = "C:/Users/AMBenson/Documents/CGL/eDNA/SACPond_Elodea_eDNA/Publication/Map_final_ForPublication_20221115/SACpond_InsetMap.jpg")
  
  
  
  
  
  
  
  
  alaska+
    geom_rect(data = data.frame( xmin = 467000,
                                 xmax = 469999,
                                 ymin = 7186187,
                                 ymax = 7188187),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "grey", colour="black", size=3) 
  
    geom_rect(aes(xmin = 467000,
                  xmax = 469999,
                  ymin = 7186187,
                  ymax = 7188187),
              fill = xmin,colour = "black", size = 2)
  
  ##TRying to use this example for
 geom_rect(data = data.frame( xmin = 467488,
                              xmax = 469671,
                              ymin = 7186187,
                              ymax = 7188187),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5, size=10) +
  
  ############################################
  SACpond_InsetMap<-
    ggdraw(xlim = c(0, 28), ylim = c(0, 20)) +
    draw_plot(alaska,x = 0, y = 0, width = 20, height = 20 )+
    draw_plot(SACpond_map,x = 15, y = 7.4, width = 5, height = 10) 
    
  ggsave(plot =  SACpond_InsetMap, width = 6, height = 4, dpi = 300, filename = "C:/Users/AMBenson/Documents/CGL/eDNA/SACPond_Elodea_eDNA/Publication/Map_final_ForPublication_20221115/SACpond_InsetMap.jpg")
  
  
 #SACpond big
 SACpond_InsetMap<-
    ggdraw(xlim = c(0, 28), ylim = c(0, 20)) +
    draw_plot(SACpond_map,x = 0, y = 0, width = 18, height = 18 ) +
    draw_plot(alaska,x = 9, y = 15.25, width = 10, height = 6)
  
  
   
##Draw an arrow pointin inset to big map
    geom_segment(aes(x = x2, y = y2, xend = x1, yend = y1), data = arrow, 
                 arrow = arrow(), lineend = "round") 
 
    arrow <- data.frame(x1 = 15.3, x2 = 16.9, y1 = 14, y2 = 18.8) ##use an arrow and ggdraw
  
  ggdraw(xlim = c(0, 28), ylim = c(0, 20)) +
    draw_plot(SACpond_map,x = 0, y = 0, width = 20, height = 20 ) +
    draw_plot(alaska,x = 20, y = 11.25, width = 8, height = 8) +
     
    
