# glider coordinates

# load packages
library(dplyr)
library(raster)
library(ggplot2)
library(sp)
library(ggspatial)
library(marmap)
library(rgdal)


# data
wf = as.data.frame(rbind(c(39.1138, -74.3615),
                         c(39.0159, -74.2654),
                         c(39.1445, -74.0974),
                         c(39.2209, -74.1937),
                         c(39.1138, -74.3615)))
names(wf) = c("lat","long")
wf = sp::Polygon(wf)

bb =  as.data.frame(rbind(c(39.33775, -74.31985), 
                            c(39.03994, -73.9746), 
                            c(38.79626, -74.33211),
                            c(39.08586, -74.63179)))
names(bb) = c("lat","long")
bb = sp::Polygon(bb)

#lease = raster::shapefile("Documents/BOEM_Renewable_Energy_Areas_Shapefile_3_29_2021/BOEM_Wind_Planning_Areas_3_29_2021.shp")
areas = raster::shapefile("Documents/BOEM_Renewable_Energy_Areas_Shapefile_3_29_2021/BOEM_Lease_Areas_3_29_2021.shp")

# Get bathymetry data
#bathy <- getNOAA.bathy(-74.6, -74.0, 38.75, 39.35, res = 1, keep = TRUE)
#ggbathy <- fortify(bathy)

# plot
p = ggplot() + 
  geom_polygon(data = areas, aes(y = lat, x=long, group=group), col="grey", fill = "white", alpha = 0.1) +
  geom_polygon(data = wf, aes(x= lat, y = long), fill = "red", alpha = 0.6) + 
  geom_polygon(data = bb, aes(x = lat, y = long), fill = "gold", alpha = 0.6) + 
  coord_cartesian(xlim = c(-74.61, -73.9), ylim = c(38.75, 39.35)) +
  theme_bw() + 
  labs(x="Longitude", y="Latitude") + 
  theme(text = element_text(size=20)) 
p


# export as shapefile
pbb = Polygons(list(bb),1)
sbb = SpatialPolygons(list(pbb))
proj4string(sbb) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
data = data.frame(f=NA)
sbbdf = SpatialPolygonsDataFrame(sbb,data)
writeOGR(sbbdf, dsn = '.', layer = 'Orsted_Fisheries_Glider_box', driver = "ESRI Shapefile")

  
