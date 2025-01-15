##### Cod 2025 Final Report

##### packages
require(sf)
require(tidyverse)
require(ggplot2)
require(readxl)
require(lubridate)
require(marmap)
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

#### glider path
#ru34_nov24 = st_read(dsn = "~/Downloads/")

##### import tag data
# deployment 1 
# https://marine.rutgers.edu/~dkaragon/glider_profile_plotting/ru34-20241102T1737/ru34-20241102T1737_vemco-detections.kmz
tag_layers = st_layers('~/Downloads/ru34-20241102T1737_vemco-detections.kml')
tag_ids = st_layers('~/Downloads/ru34-20241102T1737_vemco-detections.kml')[,1]
tag_sum = tibble(tag_layers) %>% dplyr::select(name, features)
write.csv(tag_sum, file="tags_Nov_Dec_2024_summary.csv")

# read each tag id layer
layers_sf = list()
for(i in 1:length(tag_ids)) {
  layers_sf[[tag_ids[i]]] = read_sf(dsn = '~/Downloads/ru34-20241102T1737_vemco-detections.kml', 
                                    layer = tag_ids[i])
}

tag_df = sf::st_as_sf(data.table::rbindlist(layers_sf))
tag_df$date = sapply(strsplit(
  sapply(strsplit(tag_df$Description,"<br>"),head,1),
  "DATE: "),tail,1)
      #format = "%d-%b-%Y %h:%M:%s")
tag_df$signal_strength = as.numeric(
  sapply(strsplit(
    sapply(strsplit(
      sapply(strsplit(tag_df$Description,"NOISE LEVEL"),head,1),
      "SIGNAL LEVEL: S="),tail,1),
    " | "),head,1))

# a plot f the first to test the list object `layers_sf`:
#plot(layers_sf[[1]])

# path<-"~/Downloads/ru34-20241102T1737_vemco-detections.kml"
# gdb<-st_layers(path)
# list_of_features<-purrr::map(gdb$name,~st_read(dsn=path,layer=.))

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
ggsave("cod_nov24_Frey_cod_tags.png", p1)

p2 = p + 
  geom_sf(data = hms_array, color = "black", size=0.5) + 
  geom_sf(data = sfw_array, color = "darkgrey", size=0.5) + 
  geom_sf(data = tag_df[tag_df$Name %in% sync_tag_id,], aes(color = Name)) + 
  theme_bw() +
  labs(title = "Sync Tags", colour = "Tag ID", fill = "Bathy") +
  coord_sf(xlim = c(-71.39, -70.81), ylim = c(40.82, 41.28)) 
p2
ggsave("cod_nov24_sync_tags.png", p2)





