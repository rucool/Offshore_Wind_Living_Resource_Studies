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


### tags
codtags2324 = st_read(dsn = "~/Downloads/", layer = "codtags2324")
codtags2425 = st_read(dsn = "~/Downloads/", layer = "codtags2425")
codtag2526 = st_read(dsn = "~/Downloads/", layer = "codtags2526")
all_cod_tags = rbind(codtags2324, codtags2425, codtag2526) %>%
  mutate(season = 0, 
         season = ifelse(date_tm > as.Date("2023-10-1") & date_tm < as.Date("2024-04-30"), 
                         "Season 1/2024-3/2024", season),
         season = ifelse(date_tm > as.Date("2024-10-1") & date_tm < as.Date("2025-04-30"), 
                         "Season 11/2024-4/2025", season),
         season = ifelse(date_tm > as.Date("2025-10-1") & date_tm < as.Date("2026-04-30"), 
                         "Season 11/2025-3/2026", season))
all_cod_df = all_cod_tags %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  as.data.frame()

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
       col="Transmitter", shape="Season")
  

ggplot() + 
  geom_point(data = all_cod_df, aes(x=lon, y=lat, col=Trnsmtt, shape = factor(season)), alpha=0.5) + 
  facet_wrap(~Trnsmtt, scales = "free", ncol=5) + 
  theme_bw() + 
  theme(legened.position = "none", text = element_text(size=12)) +
  labs(col="Transmitter",shape="Year",x="Longitude", y="Latitude") 
  
  
  
