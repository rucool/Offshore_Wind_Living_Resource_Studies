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

#### leases
leases = st_read(dsn = "~/Downloads/BOEM-Renewable-Energy-Shapefiles_22_10_21/", 
                 layer = "BOEMWindLeaseOutlines_6_30_2022")
REV = leases[leases$LEASE_NUMB %in% "OCS-A 0486",]
SFW = leases[leases$LEASE_NUMB %in% "OCS-A 0517",]
SRW = leases[leases$LEASE_NUMB %in% "OCS-A 0487",]
rm(leases)

##### import receiver locations
hms_array = st_read('~/Downloads/HMS_Acoustic_Telemetry.kml')
sfw_array = st_read('~/Downloads/SFW01_VPS_Acoustic_Telemetry.kml')

#### glider paths
# mission 1: ru34-20251103T1347
# mission 2: unit_1190-20251209T1402
# mission 3: unit_1190-20260121T1322
# mission 4: unit_1190-20260307T1315
path1 = read_csv("~/Downloads/ru34-20251103T1347-trajectory-raw-delayed_029a_762a_c173.csv", skip=1)
path2 = read_csv("~/Downloads/unit_1190-20251209T1402-trajectory-raw-delayed_97f9_91c3_b10f.csv", skip=1)
path3 = read_csv("~/Downloads/unit_1190-20260121T1322-trajectory-raw-delayed_16e0_b5b3_c9c8.csv", skip=1)
path4 = read_csv("~/Downloads/unit_1190-20260307T1315-trajectory-raw-rt_baf3_c30e_0af2.csv", skip=1)

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
path4 = path4[1:50178,] # remove last point, from boat ride home

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

# ggplot() + 
#   geom_sf(data = REV, fill=NA, color="black") + 
#   geom_sf(data = SRW, fill=NA, color="black") + 
#   geom_sf(data = SFW, fill=NA, color="black") +
#   coord_sf(xlim = c(-71.39, -70.81), ylim = c(40.82, 41.28)) +
#   geom_sf(data = path1_sf, color="orange2") + 
#   geom_sf(data = path2_sf, color="cornflowerblue") +
#   geom_sf(data = path3_sf, color="yellow3") +
#   geom_sf(data = path4_sf, color="purple3") + 
#   theme_bw()

path1_sfbuff = st_buffer(path1_sf, dist = 500)
path2_sfbuff = st_buffer(path2_sf, dist = 500)
path3_sfbuff = st_buffer(path3_sf, dist = 500)
path4_sfbuff = st_buffer(path4_sf, dist = 500)
buffs = rbind(path1_sfbuff, path2_sfbuff, path3_sfbuff, path4_sfbuff)

buffs_utm19 <- st_transform(buffs, 32619)
ggrid <- st_make_grid(buffs_utm19, cellsize = c(500, 500), square = TRUE) #500m x 500m 
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
                       mid = "cornflowerblue", 
                       high = "yellow", midpoint = 4) +  
  geom_sf(data = REV, fill=NA, color="black") + 
  geom_sf(data = SRW, fill=NA, color="black") + 
  geom_sf(data = SFW, fill=NA, color="black")+
  theme_bw() + 
  theme(text = element_text(size= 20), 
        panel.grid = element_blank()) + 
  labs(fill = "Passes")

# ggplot() + 
#   geom_sf(data = REV, fill=NA, color="black") + 
#   geom_sf(data = SRW, fill=NA, color="black") + 
#   geom_sf(data = SFW, fill=NA, color="black") +
#   coord_sf(xlim = c(-71.39, -70.81), ylim = c(40.82, 41.28)) +
#   geom_sf(data = path1_sfbuff, color = "khaki", fill ="gold", alpha=0.2) + 
#   geom_sf(data = path2_sfbuff, color = "khaki", fill ="gold", alpha=0.2) +
#   geom_sf(data = path3_sfbuff, color = "khaki", fill ="gold", alpha=0.2) +
#   geom_sf(data = path4_sfbuff, color = "khaki", fill ="gold", alpha=0.2) + 
#   geom_sf(data = path1_sf, color="orange") + 
#   geom_sf(data = path2_sf, color="orange") +
#   geom_sf(data = path3_sf, color="orange") +
#   geom_sf(data = path4_sf, color="orange") + 
#   theme_bw()

##### import tag data
tag1_detections = read_csv("~/Downloads/ru34-20251103T1347_rxlive_detections (1).csv")
tag1_info = read_csv("~/Downloads/ru34-20251103T1347_rxlive_taginfo (1).csv")
tag2_detections = read_csv("~/Downloads/unit_1190-20251209T1402_rxlive_detections (1).csv")
tag2_info = read_csv("~/Downloads/unit_1190-20251209T1402_rxlive_taginfo (1).csv")
tag3_detections = read_csv("~/Downloads/unit_1190-20260121T1322_rxlive_detections.csv")
tag3_info = read_csv("~/Downloads/unit_1190-20260121T1322_rxlive_taginfo.csv")
tag4_detections = read_csv("~/Downloads/unit_1190-20260307T1315_rxlive_detections.csv")
tag4_info = read_csv("~/Downloads/unit_1190-20260307T1315_rxlive_taginfo.csv")

# receiver sync tags
sync_tag_id = c("61099","61140","61141","61142","61143","61146","61148","61149",
                "61150","61153","61154","61155","61156","61157","61158","61159",
                "61160","61161","61162","61163","61164","61165","61171","61188",
                "61196","61197","61212","61215","61219","61221","61243","61245",
                "65006","65008")
#sync_tags = tag_layers[lapply(tag_layers, layer_name %in% sync_tag_id)] 

# Ali's tags
FreyTags <- read_excel("Downloads/FreyAllSMASTTags.xlsx")
FreyTags$name = sapply(strsplit(FreyTags$'Tag ID', "-"), tail, 1)
matches = FreyTags[FreyTags$name %in% tag_ids,]
Frey_tags = dplyr::filter(tag_sum, name %in% matches$name)
Frey_tags = left_join(Frey_tags, FreyTags, by = "name") %>% filter(species %in% "cod")
#result <- as.data.frame(matrix(unlist(layers_sf), ncol = length(layers_sf), byrow = TRUE))


### plots
bathy = fortify(getNOAA.bathy(-71.4, -70.8, 40.8, 41.3)) # lat/long coordinates of area of interest
bathy$z[bathy$z >= 0] = 0 #
bathy$z = abs(bathy$z) # convert to absolute values so it plots and gradients correctly

#states <- ne_states(returnclass = "sf") %>% 
#  filter(admin %in% "United States of America",
#         name_id %in% "Massachusetts")

p = ggplot() + 
  #geom_sf(data = states) +
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = z), colour = NA,
                      breaks = c(seq(from = -60, to = 0, by = 5)), 
                      show.legend = FALSE, alpha = 0.3) +
  scale_fill_brewer(palette = "Blues", direction=-1) +
  #scale_fill_continuous(breaks = c(min(bathy$z), median(bathy$z), max(bathy$z))) + 
  #scale_fill_grey(start = 0.9, end = 0.3) +
  geom_sf(data = REV, fill=NA, color="black") + 
  geom_sf(data = SRW, fill=NA, color="black") + 
  geom_sf(data = SFW, fill=NA, color="black") +
  coord_sf(xlim = c(-71.39, -70.81), ylim = c(40.82, 41.28)) 
p

p1 = p + 
  geom_sf(data = tag_df[tag_df$Name %in% Frey_tags$name[Frey_tags$species %in% "cod"],], aes(color = Name)) + 
  theme_bw() +
  labs(title = "Frey Cod", colour = "Tag ID", fill = "Bathy") +
  coord_sf(xlim = c(-71.39, -70.81), ylim = c(40.82, 41.28)) 
p1
ggsave("cod_nov25_Frey_cod_tags.png", p1)

p2 = p + 
  geom_sf(data = hms_array, color = "black", size=0.5) + 
  geom_sf(data = sfw_array, color = "darkgrey", size=0.5) + 
  geom_sf(data = tag_df[tag_df$Name %in% sync_tag_id,], aes(color = Name)) + 
  theme_bw() +
  labs(title = "Sync Tags", colour = "Tag ID", fill = "Bathy") +
  coord_sf(xlim = c(-71.39, -70.81), ylim = c(40.82, 41.28)) 
p2
ggsave("cod_dec25_sync_tags.png", p2)

#### exports
for_Ali = tag_df[tag_df$Name %in% Frey_tags$name[Frey_tags$species %in% "cod"],]
for_Ali = cbind(for_Ali, as.data.frame(st_coordinates(for_Ali))) %>% 
  dplyr::select(-Description, -geometry, -Z)



