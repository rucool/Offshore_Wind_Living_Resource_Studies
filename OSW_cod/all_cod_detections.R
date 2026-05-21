##### Cod 2025 Final Report

##### packages
require(sf)
require(tidyverse)
require(ggplot2)
require(marmap)
library(ggpubr)
library(RColorBrewer)
library(lubridate)

#### leases
#leases = st_read(dsn = "~/Downloads/BOEM-Renewable-Energy-Shapefiles_22_10_21/", 
#                 layer = "BOEMWindLeaseOutlines_6_30_2022")
leases = st_read(dsn = "~/Downloads/boem-renewable-energy-shapefiles_0/", 
                 layer = "Offshore_Wind_Leases_outlines")
REV = leases[leases$LEASE_NUMB %in% "OCS-A 0486",]
SFW = leases[leases$LEASE_NUMB %in% "OCS-A 0517",]
SRW = leases[leases$LEASE_NUMB %in% "OCS-A 0487",]
rm(leases)

### turbines
# https://hub.marinecadastre.gov/datasets/f39813f704d04702a7035492fba6e9ca_0/explore?location=40.988110%2C-71.119720%2C10
turbines = st_read(dsn = "~/Downloads/Offshore_Wind_Turbines (1)/", 
                 layer = "Offshore_Wind_Turbines")

turbines = filter(turbines, sitename %in% c("South Fork Wind","Revolution Wind","Sunrise Wind"))

### grunts
sfw_grunt = as.data.frame(cbind(0, 41.109, -71.197))
names(sfw_grunt) = c("date_tm", "lat", "lon")
sfw_grunt$date_tm = as.POSIXct("2023-03-17 19:04", tz = "UTC")

# rev_grunts = as.data.frame(rbind(cbind(0, 41.1472, -70.9168),
#                                  cbind(0, 41.0980, -71.0420 ),
#                                  cbind(0, 41.1640, -70.8684),
#                                  cbind(0, 41.1245, -70.9024),
#                                  cbind(0, 41.0983, -71.0444),
#                                  cbind(0, 41.1454, -70.8346)))
rev_grunts = as.data.frame(rbind(cbind(0, 41.14139, -70.91475),
                                 cbind(0, 41.09898, -71.04247),
                                 cbind(0, 41.16096, -70.86291),
                                 cbind(0, 41.12491, -70.90393),
                                 cbind(0, 41.10513, -71.03608),
                                 cbind(0, 41.13448, -70.83889)))
names(rev_grunts) = c("date_tm", "lat", "lon")
rev_grunts$date_tm = c(as.POSIXct("2024-01-17 19:45", tz = "UTC"),
                      as.POSIXct("2024-01-30 23:56", tz = "UTC"),
                      as.POSIXct("2024-03-17 20:54", tz = "UTC"),
                      as.POSIXct("2024-01-18 00:33", tz = "UTC"),
                      as.POSIXct("2024-01-31 02:50", tz = "UTC"),
                      as.POSIXct("2024-03-18 00:22", tz = "UTC"))
# lines
m1 = read_csv("~/Downloads/ru34-20240112T1722-trajectory-raw-delayed_4744_9558_2e17.csv", skip=1)
m1 = m1[,1:4]
names(m1) = c("date_time","latitude","longitude","depth")

m2 = read_csv("~/Downloads/ru34-20240301T1336-trajectory-raw-delayed_a92e_a68a_e11e.csv", skip=1)
m2 = m2[,1:4]
names(m2) = c("date_time","latitude","longitude","depth")

l1 = m1 %>% filter(date_time > as.POSIXct("2024-01-17 19:45", tz = "UTC") & 
                     date_time < as.POSIXct("2024-01-18 00:33", tz = "UTC"), 
                   !is.na(depth)) %>% mutate(ID ="line1")
l2 = m1 %>% filter(date_time > as.POSIXct("2024-01-30 23:56", tz = "UTC") & 
                     date_time < as.POSIXct("2024-01-31 02:50", tz = "UTC"), 
                   !is.na(depth)) %>% mutate(ID ="line2")
l3 = m2 %>% filter(date_time > as.POSIXct("2024-03-17 20:54", tz = "UTC") & 
                     date_time < as.POSIXct("2024-03-18 00:22", tz = "UTC"), 
                   !is.na(depth)) %>% mutate(ID ="line3")

lines_sf = st_as_sf(rbind(l1,l2,l3), 
                 coords = c("longitude", "latitude"),
                 crs = 4326, remove = FALSE) %>%
  arrange(date_time) %>%
  group_by(ID) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") 

### tags
codtags2324 = st_read(dsn = "~/Downloads/", layer = "codtags2324")
codtags2425 = st_read(dsn = "~/Downloads/", layer = "codtags2425")
codtag2526 = st_read(dsn = "~/Downloads/", layer = "codtags2526")
all_cod_tags = rbind(codtags2324, codtags2425, codtag2526) %>%
  mutate(season = 0, 
         season = ifelse(date_tm > as.Date("2023-10-1") & date_tm < as.Date("2024-04-30"), 
                         "Season 2 (1/2024-3/2024)", season),
         season = ifelse(date_tm > as.Date("2024-10-1") & date_tm < as.Date("2025-04-30"), 
                         "Season 3 (11/2024-4/2025)", season),
         season = ifelse(date_tm > as.Date("2025-10-1") & date_tm < as.Date("2026-04-30"), 
                         "Season 4 (11/2025-3/2026)", season))
all_cod_df = all_cod_tags %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  as.data.frame()


all_cod_detections = rbind(all_cod_df %>% mutate(Type = "Transmitter"),
                           sfw_grunt  %>% mutate(Type = "PAM", season = "Season 1 (3/2023)", Trnsmtt=NA),
                           rev_grunts  %>% mutate(Type = "PAM", season = "Season 2 (1/2024-3/2024)", Trnsmtt=NA))



### plots
bathy = fortify(getNOAA.bathy(-71.4, -70.8, 40.8, 41.3, resolution=1))
bathy$z[bathy$z >= 0] = 0 #
bathy$z = abs(bathy$z) # convert to absolute values so it plots and gradients correctly
blues_extended <- colorRampPalette(brewer.pal(9, "Blues"))(13)  # 20 colors

p = ggplot() + 
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = z), 
                      show.legend = FALSE, alpha = 0.3) +
  scale_fill_manual(values = blues_extended) +
  geom_sf(data = REV, fill=NA, color="black") + 
  geom_sf(data = SRW, fill=NA, color="black") + 
  geom_sf(data = SFW[1,], fill=NA, color="black") +
  geom_sf(data = turbines, fill=NA, color="white", size = 1) +
  coord_sf(xlim = c(-71.35, -70.83), ylim = c(40.88, 41.28)) + 
  labs(x = "Longitude", y = "Latitude")
p


p + 
  geom_sf(data = all_cod_tags, aes(col=Trnsmtt, shape = factor(season)), alpha=0.8, size =3) + 
  coord_sf(xlim = c(-71.35, -70.83), ylim = c(40.88, 41.28)) + 
  labs(title = "Atlantic Cod Transmitters Across Seasons (2024-2026)",
       col="Transmitter", shape="Season") + 
  theme()

p + 
  geom_point(data = all_cod_detections, aes(x=lon, y=lat, col=Type, shape = factor(season)), alpha=0.8, size =3) + 
  coord_sf(xlim = c(-71.35, -70.83), ylim = c(40.88, 41.28)) + 
  labs(title = "Atlantic Cod Detections Across Seasons (2023-2026)",
       col="Detection", shape="Season") + 
  theme() + 
  scale_colour_manual(values = c("navy","orange")) + 
  scale_shape_manual(values = c(9, 0, 1, 2)) + 
  geom_sf(data = lines_sf, col="navy") + 
  coord_sf(xlim = c(-71.35, -70.83), ylim = c(40.88, 41.28)) 
  
  

ggplot() + 
  geom_point(data = all_cod_df, aes(x=lon, y=lat, col=Trnsmtt, shape = factor(season)), alpha=0.5) + 
  facet_wrap(~Trnsmtt, scales = "free", ncol=5) + 
  theme_bw() + 
  theme(legened.position = "none", text = element_text(size=12)) +
  labs(col="Transmitter",shape="Year",x="Longitude", y="Latitude") 
  
  
  
