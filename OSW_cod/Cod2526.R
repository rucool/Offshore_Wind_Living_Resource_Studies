##### Cod 2025 Final Report

##### packages
require(sf)
require(tidyverse)
require(ggplot2)
require(readxl)
require(lubridate)
require(marmap)
library(stars)
#library(sfheaders)
library(sftime)
#require(rnaturalearth)
library(writexl)
library(geosphere)

#### leases
#leases = st_read(dsn = "~/Downloads/BOEM-Renewable-Energy-Shapefiles_22_10_21/", 
#                 layer = "BOEMWindLeaseOutlines_6_30_2022")
leases = st_read(dsn = "~/Downloads/boem-renewable-energy-shapefiles_0/", 
                 layer = "Offshore_Wind_Leases_outlines")
REV = leases[leases$LEASE_NUMB %in% "OCS-A 0486",]
SFW = leases[leases$LEASE_NUMB %in% "OCS-A 0517",]
SRW = leases[leases$LEASE_NUMB %in% "OCS-A 0487",]
rm(leases)

##### import receiver locations
#hms_array = st_read('~/Downloads/HMS_Acoustic_Telemetry.kml')
#sfw_array = st_read('~/Downloads/SFW01_VPS_Acoustic_Telemetry.kml')

#### glider paths
# mission 1: ru34-20251103T1347
# mission 2: unit_1190-20251209T1402
# mission 3: unit_1190-20260121T1322
# mission 4: unit_1190-20260307T1315
path1 = read_csv("~/Downloads/ru34-20251103T1347-trajectory-raw-delayed_029a_762a_c173.csv", skip=1)
path2 = read_csv("~/Downloads/unit_1190-20251209T1402-trajectory-raw-delayed_97f9_91c3_b10f.csv", skip=1)
path3 = read_csv("~/Downloads/unit_1190-20260121T1322-trajectory-raw-delayed_16e0_b5b3_c9c8.csv", skip=1)
path4 = read_csv("~/Downloads/unit_1190-20260307T1315-trajectory-raw-delayed_edca_ecb2_a962.csv", skip=1)

path1 = path1[,1:4]
path2 = path2[,1:4]
path3 = path3[,1:4]
path4 = path4[,1:4]

names(path1) = c("date_time","latitude","longitude","depth")
names(path2) = c("date_time","latitude","longitude","depth")
names(path3) = c("date_time","latitude","longitude","depth")
names(path4) = c("date_time","latitude","longitude","depth")

path1$date_time <- as.POSIXct(path1$date_time,tz="UTC") # define as UTC
path2$date_time <- as.POSIXct(path2$date_time,tz="UTC") # define as UTC
path3$date_time <- as.POSIXct(path3$date_time,tz="UTC") # define as UTC
path4$date_time <- as.POSIXct(path4$date_time,tz="UTC") # define as UTC

path1 = path1 %>% filter(!is.na(depth)) # remove dups
path2 = path2 %>% filter(!is.na(depth)) # remove dups
path3 = path3 %>% filter(!is.na(depth)) # remove dups
path4 = path4 %>% filter(!is.na(depth)) # remove dups
#path4 = path4[1:50178,] # remove last point, from boat ride home

path1_sf = st_as_sf(path1, coords = c("longitude", "latitude"), 
                    crs = 4326, remove = FALSE) %>%
  arrange(date_time) %>%
  group_by(date(date_time)) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") 

# when you group by date_time it breaks the lines, if you dont want this then don't group
# needed to group for aggregation below, otherwise it was one line/one object and didnt count self intersections
path2_sf = st_as_sf(path2, coords = c("longitude", "latitude"), crs = 4326) %>%
  arrange(date_time) %>%
  group_by(date(date_time)) %>%
  summarise(do_union = FALSE) %>%                   
  st_cast("LINESTRING") 
path3_sf = st_as_sf(path3, coords = c("longitude", "latitude"), crs = 4326) %>%
  arrange(date_time) %>%
  group_by(date(date_time)) %>%
  summarise(do_union = FALSE) %>%                   
  st_cast("LINESTRING") 
path4_sf = st_as_sf(path4, coords = c("longitude", "latitude"), crs = 4326) %>%
  arrange(date_time) %>%
  group_by(date(date_time)) %>%
  summarise(do_union = FALSE) %>%                   
  st_cast("LINESTRING") 

path1_sfbuff = st_buffer(path1_sf, dist = 500)
path2_sfbuff = st_buffer(path2_sf, dist = 500)
path3_sfbuff = st_buffer(path3_sf, dist = 500)
path4_sfbuff = st_buffer(path4_sf, dist = 500)
buffs = rbind(path1_sfbuff, path2_sfbuff, path3_sfbuff, path4_sfbuff)

buffs_utm19 <- st_transform(buffs, 32619)
ggrid <- st_make_grid(buffs_utm19, cellsize = c(100, 100), square = TRUE) #100m x 100m 
grid_sf <- st_sf(ggrid) %>% mutate(grid_id = row_number())

buffs1_utm19 <- st_transform(path1_sfbuff, 32619)
buffs2_utm19 <- st_transform(path2_sfbuff, 32619)
buffs3_utm19 <- st_transform(path3_sfbuff, 32619)
buffs4_utm19 <- st_transform(path4_sfbuff, 32619)

grid_sf$counts = lengths(st_intersects(grid_sf, buffs_utm19, sparse = TRUE))
grid_sf = grid_sf %>% mutate(counts2 = ifelse(counts > 8, 8, counts))

grid_sf <- grid_sf %>% mutate(counts2 = ifelse(counts2==0, NA, counts2))
ggplot() + geom_sf(data = grid_sf, aes(fill=counts2), col=NA)+ 
  scale_fill_gradient2(na.value = NA, 
                       low = "navy", 
                       mid = "lightblue", 
                       high = "yellow", midpoint = 4) +  
  geom_sf(data = REV, fill=NA, color="black", lwd = 1) + 
  geom_sf(data = SRW, fill=NA, color="black", lwd = 1) + 
  geom_sf(data = SFW[1,], fill=NA, color="black", lwd = 1)+
  theme_bw() + 
  theme(text = element_text(size= 20), 
        panel.grid = element_blank()) + 
  labs(fill = "Passes")

##### import tag data
tag1_detections = read_csv("~/Downloads/ru34-20251103T1347_rxlive_detections (1).csv")
tag1_info = read_csv("~/Downloads/ru34-20251103T1347_rxlive_taginfo (1).csv")
tag2_detections = read_csv("~/Downloads/unit_1190-20251209T1402_rxlive_detections (1).csv")
tag2_info = read_csv("~/Downloads/unit_1190-20251209T1402_rxlive_taginfo (1).csv")
tag3_detections = read_csv("~/Downloads/unit_1190-20260121T1322_rxlive_detections.csv")
tag3_info = read_csv("~/Downloads/unit_1190-20260121T1322_rxlive_taginfo.csv")
tag4_detections = read_csv("~/Downloads/unit_1190-20260307T1315_rxlive_detections.csv")
tag4_info = read_csv("~/Downloads/unit_1190-20260307T1315_rxlive_taginfo.csv")

taginfo = rbind(tag1_info, tag2_info, tag3_info, tag4_info) %>% 
  mutate(t0 = as.POSIXct(t0, format = "%m/%d/%Y %H:%M:%S",, tz="UTC"),
         t1 = as.POSIXct(t1, format = "%m/%d/%Y %H:%M:%S",, tz="UTC")) %>%
  group_by(tagID, species, PI) %>% 
  summarise(n = sum(nDetections),
            start = min(t0),
            end = max(t1))
write_xlsx(taginfo, path = "~/Downloads/cod2526_taginfo.xlsx")

cod = c("A69-1602-58729","A69-1602-58733","A69-1602-58746","A69-1602-58759",
        "A69-1602-58784","A69-1602-58808","A69-1602-58815")#,
        #"A69-1602-58789","A69-1602-58805")


tags = rbind(tag1_detections, tag2_detections, 
             tag3_detections, tag4_detections) %>% 
  filter(Transmitter %in% cod) %>%
  mutate(date_time = as.POSIXct(`Date and Time (UTC)`, format = "%m/%d/%Y %H:%M:%S",, tz="UTC"))
paths = rbind(path1, path2, path3, path4)

# combine detection with location
find_nearest_date <- function(date, date_match){  
  date_nearest <- date %>% 
    map_dbl(find_nearest_date_worker, date_match) %>% 
    as.POSIXct(tz = "UTC", origin = "1970-01-01")
  return(date_nearest)
}

find_nearest_date_worker <- function(date, date_vector) {
  delta <- abs(date - date_vector)
  index <- which.min(delta)
  x <- date_vector[index]
  return(x)
}

tags$match_time = find_nearest_date(tags$date_time, paths$date_time)
tags = left_join(tags, paths, by =c("match_time"="date_time"))

### plots
bathy = fortify(getNOAA.bathy(-71.4, -70.8, 40.8, 41.3)) # lat/long coordinates of area of interest
bathy$z[bathy$z >= 0] = 0 #
bathy$z = abs(bathy$z) # convert to absolute values so it plots and gradients correctly

p = ggplot() + 
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = z), 
                      show.legend = FALSE, alpha = 0.3) +
  scale_fill_brewer(palette = "Blues", direction=1) +
  geom_sf(data = REV, fill=NA, color="black") + 
  geom_sf(data = SRW, fill=NA, color="black") + 
  geom_sf(data = SFW[1,], fill=NA, color="black") +
  coord_sf(xlim = c(-71.39, -70.81), ylim = c(40.82, 41.28)) 
p

p1 = p + 
  geom_point(data = tags, aes(y = latitude, x = longitude, color = Transmitter)) + 
  theme_bw() +
  labs(x = "Longitude", y = "Latitude", 
       title = "Atlantic Cod Transmitters", colour = "Tag ID", fill = "Bathy") +
  coord_sf(xlim = c(-71.35, -70.81), ylim = c(40.88, 41.28)) 
p1
ggsave("cod_tags2526.png", p1)

p2 = p + 
  geom_point(data = tags, aes(y = latitude, x = longitude, color = Transmitter)) + 
  facet_wrap(~Transmitter) +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude", 
       title = "Atlantic Cod Transmitters", colour = "Tag ID", fill = "Bathy") +
  coord_sf(xlim = c(-71.35, -70.81), ylim = c(40.88, 41.28)) +
  theme(legend.position = "none",
        text = element_text(size = 11))
p2
ggsave("cod_tags2526_facet.png", p2)

#---------------#
# Time diff
#---------------#
# for dead cod uncomment transmitters above before creating `tags`
cod_sum = tags %>% 
  group_by(Transmitter) %>%
  summarise(first_lat = first(latitude),
         first_lon = first(longitude),
         first_hit = first(date_time),
         last_lat = last(latitude),
         last_lon = last(longitude),
         last_hit = last(date_time)) %>%
  rowwise() %>%
  mutate(dist = round(distm(c(first_lon, first_lat), c(last_lon, last_lat), 
                                fun = distHaversine), digits = 2), 
         secs = (last_hit - first_hit),
         mins = ifelse(secs > 60, round(secs/60, digits = 2), NA),
         days = ifelse(secs > 86400, round(secs/86400, digits = 2), NA))

#---------------#

#---------------#
# Comparison to previous seasons
#---------------#
#run REV_SRW_tags_2025.R

#---------------#
