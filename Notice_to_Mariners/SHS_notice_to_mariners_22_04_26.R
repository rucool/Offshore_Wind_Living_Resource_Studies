# SHS survey validate coordinates

library(dplyr)
library(raster)
library(ggplot2)
library(sp)
library(ggspatial)
library(marmap)

# data
reef = as.data.frame(rbind(c(39.28167,	-74.25467),
                         c(39.27033,	-74.26967),
                         c(39.22167,	-74.21167),
                         c(39.23217,	-74.19667),
                         c(39.28167,	-74.25467)))
names(reef) = c("lat","long")
reef = sp::Polygon(reef)

site = as.data.frame(rbind(c(39.15567,	-74.19362),
                           c(39.18,	-74.15737),
                           c(39.08133,	-74.04203),
                           c(39.05483,	-74.08125),
                           c(39.15567,	-74.19362)))
names(site) = c("lat","long")
site = sp::Polygon(site)

wf = as.data.frame(rbind(c(39.1138, -74.3615),
                         c(39.0159, -74.2654),
                         c(39.1445, -74.0974),
                         c(39.2209, -74.1937),
                         c(39.1138, -74.3615)))
names(wf) = c("lat","long")
wf = sp::Polygon(wf)

ref =  as.data.frame(rbind(c(38.87, -74.428),
                           c(38.95, -74.53),
                           c(38.9, -74.60),
                           c(38.78, -74.442),
                           c(38.975, -74.21),
                           c(39.0159, -74.2654),
                           c(38.87, -74.428)))
names(ref) = c("lat","long")
ref = sp::Polygon(ref)

#lease = raster::shapefile("Documents/BOEM_Renewable_Energy_Areas_Shapefile_3_29_2021/BOEM_Wind_Planning_Areas_3_29_2021.shp")
areas = raster::shapefile("Documents/BOEM_Renewable_Energy_Areas_Shapefile_3_29_2021/BOEM_Lease_Areas_3_29_2021.shp")

# Get bathymetry data
#bathy <- getNOAA.bathy(-74.6, -74.0, 38.75, 39.35, res = 1, keep = TRUE)
#ggbathy <- fortify(bathy)

#cols = c("Lease Areas" = "grey", 
#         "Artificial Reef" = "blue", 
#         "SHS Site" = "gold", 
#         "Wind Farm" = "red", 
#         "Control Site" = "green")
p = ggplot() + 
  #geom_contour(data = ggbathy, aes(x = x, y = y, z = z), binwidth = 200, color = "seafoam", size = 0.5) +
  geom_polygon(data = areas, aes(y = lat, x=long, group=group), col="grey", fill = "white", alpha = 0.1) +
  #geom_polygon(data = lease, aes(y = lat, x=long, group=group), col="orange", fill = "white", alpha = 0.3) +
  geom_polygon(data = reef, aes(x= lat, y = long), fill = "blue", alpha = 0.6) + 
  geom_polygon(data = ref, aes(x = lat, y = long), fill = "green", alpha = 0.6) + 
  geom_polygon(data = wf, aes(x= lat, y = long), fill = "red", alpha = 0.6) + 
  geom_polygon(data = site, aes(x = lat, y = long), fill = "gold", alpha = 0.6) + 
  coord_cartesian(xlim = c(-74.6, -74.05), ylim = c(38.75, 39.35)) +
  theme_bw() + 
  labs(x="Longitude", y="Latitude") + 
  theme(text = element_text(size=20)) #+ 
#  legend("topright", 
#         legend=c("Lease Areas", 
#                  "Artificial Reef",
#                  "SHS Site", 
#                  "Experimental Site", 
#                  "Control Site"), 
#         fill=c("white", "blue","gold","red","green"),
#         border=c("gray", "blue","gold","red","green")) 
#guide=guide_legend(position = "right"),
#                     name = "Legend",
#                     values = c("grey","blue","gold","red","green"),
#                     labels = c("))+
#  ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
#                                      pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
#                                      style = north_arrow_orienteering(fill = c("grey40", "white"), line_col = "grey20"))
p

  
