# -------- #
# Create a map showing number of passes across the leases
# Dave created the coverage ploy 
# -------- #

# -------- #
# load leases
# -------- #
leases = st_read(dsn = "~/Downloads/BOEM_Renewable_Energy_Shapefiles_1/",
                 layer = "Wind_Lease_Outlines_2_2023")
REV = leases[leases$LEASE_NUMB %in% "OCS-A 0486",]
SRW = leases[leases$LEASE_NUMB %in% "OCS-A 0487",]
SFW = leases[leases$LEASE_NUMB %in% "OCS-A 0517",]
# -------- #

# -------- #
# load Dave's poly and .mat
# -------- #
require(sf)
shape <- read_sf(dsn = "~/Downloads/", layer = "total_coverage_poly")

require(R.matlab)
# read in our data
xyz <- readMat("~/Downloads/x_y_N_hist.mat")
str(xyz)

# play around with different formats
r <- rasterFromXYZ(xyz)
r <-raster(
  xyz$N,
  xmn=range(xyz$x)[1], xmx=range(xyz$x)[2],
  ymn=range(xyz$y)[1], ymx=range(xyz$y)[2], 
  crs=CRS("+proj=longlat")
)
#plot(r)

y=unlist(xyz$y)
x=unlist(xyz$x)
n=unlist(xyz$N)

xyn = as.data.frame(cbind(as.vector(x),as.vector(y),as.vector(n)))
names(xyn) = c("Longitude","Latitude","Passes")

xyn2 = xyn[!is.na(xyn$Passes),]
xyn_sf = st_as_sf(xyn2, coords= c("Longitude", "$Latitude"))

library(raster)
dfr <- rasterFromXYZ(xyn2)  #Convert first two columns as lon-lat and third as value                
#plot(dfr)

my_raster_df <- raster::as.data.frame(r, xy = TRUE) 
# -------- #

# -------- #
# plots
# -------- #
library(ggplot2)
ggplot(data = xyn2, aes(x=Longitude, y=Latitude, color = Passes))+
  geom_point()+theme_bw() +
  theme(text = element_text(size=15)) +
  scale_fill_brewer(palette="YlOrBr")

ggplot() +
  geom_raster(data = my_raster_df, aes(x=x, y=y, fill = log(layer)), 
              stat = "identity") + 
  theme_bw()+
  scale_fill_binned(type = "viridis")#scale_fill_continuous(type = "viridis")

xyn3 = xyn2 %>% dplyr::mutate(Passes = ifelse(Passes > 5, 5, Passes))
xyn4 = xyn2 %>% filter(Passes > 7)
ggplot()+
  theme(text = element_text(size=15)) +
  geom_sf(data = leases %>% 
            filter(LEASE_NUMB %in% c("OCS-A 0486","OCS-A 0487","OCS-A 0517")),
          col="black") + 
  geom_point(data = xyn4, aes(x=Longitude, y=Latitude, color = Passes))+
  theme_bw() 
  
my_raster_df2 = my_raster_df %>% 
  dplyr::mutate(layer = ifelse(layer > 7, 8, layer)) %>%
  filter(!is.na(layer))

ggplot() +
  geom_raster(data = my_raster_df2, 
              aes(x=x, y=y, fill = layer), 
              stat = "identity") + 
  geom_sf(data = leases %>% 
            filter(LEASE_NUMB %in% c("OCS-A 0486","OCS-A 0487","OCS-A 0517")),
          fill=NA, col="black", lwd=1) +
  theme_bw()+ 
  theme(text = element_text(size=20))+
  #paletteer::scale_fill_paletteer_c("viridis::plasma") + 
  scale_fill_gradient2(na.value = "white", low = "yellow", mid = "magenta", 
                        high = "purple", midpoint = 4) +
  labs(fill = "Passes",y="Latitude",x="Longtitude")
  #scale_fill_viridis_c(option = "magma")
  #scale_fill_binned(type = "viridis") 

# in lease
sf = st_as_sf(my_raster_df, coords = c("x","y"), as_points = T, crs = st_crs(4326))
sf_in_REV = st_intersection(sf, leases[leases$LEASE_NUMB %in% "OCS-A 0486",])
sf_in_SRW = st_intersection(sf, leases[leases$LEASE_NUMB %in% "OCS-A 0487",])
sf_in_SFW = st_intersection(sf, leases[leases$LEASE_NUMB %in% "OCS-A 0517",])

sf_in_REV2 =  sf_in_REV %>% 
  dplyr::mutate(layer = ifelse(layer > 7, 8, layer))

ggplot() +
  geom_sf(data = sf_in_REV2, aes(col=layer)) + 
  geom_sf(data = sf_in_SRW, aes(col=layer)) + 
  geom_sf(data = sf_in_SFW, aes(col=layer)) + 
  geom_sf(data = leases %>% 
            filter(LEASE_NUMB %in% c("OCS-A 0486","OCS-A 0487","OCS-A 0517")),
          color="black", fill=NA, lwd=1) +
  theme_bw() + 
  labs(fill = "Passes",y="Latitude",x="Longtitude") +
  scale_color_gradient2(na.value = "white", low = "yellow", mid = "magenta", 
                        high = "purple", midpoint = 4) 
  # scale_color_binned(type = "viridis", na.value = "white")
